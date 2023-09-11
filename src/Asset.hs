{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Asset (Pool(..),aggPool
       ,Asset(..),AggregationRule
       ,getIssuanceField,calcPmt
       ,calcPiFlow,calc_p_i_flow_even,calc_p_i_flow_i_p
       ,buildAssumptionPpyDefRecRate
       ,calcRecoveriesFromDefault
       ,priceAsset
) where

import qualified Data.Time as T
import qualified Data.Text as Text
import Text.Read (readMaybe)

import Lib (Period(..)
           ,Ts(..),periodRateFromAnnualRate,toDate
           ,getIntervalDays,zipWith9,mkTs,periodsBetween
           ,mkRateTs,daysBetween)

import qualified Cashflow as CF -- (Cashflow,Amount,Interests,Principals)
import qualified Assumptions as A
import qualified AssetClass.AssetBase as ACM 

import qualified Data.Map as Map
import Analytics
import Data.List
import Data.Maybe
import Data.Ratio
import Data.Aeson hiding (json)
import Language.Haskell.TH
import GHC.Generics
import Data.Aeson.TH
import Data.Aeson.Types
import Types hiding (Current)
import Text.Printf
import Data.Fixed
import qualified InterestRate as IR
import Util

import AssetClass.AssetBase

import Debug.Trace
import Assumptions (ExtraStress(ExtraStress))
debug = flip trace

class Show a => Asset a where
  -- | project contractual cashflow of an asset with interest assumptions
  calcCashflow :: a -> Date -> Maybe [RateAssumption] -> CF.CashFlowFrame
  -- | Get current balance of an asset
  getCurrentBal :: a -> Balance
  -- | Get original balance of an asset
  getOriginBal :: a -> Balance
  -- | Get original rate of an asset
  getOriginRate :: a -> IRate
  -- | Get origination date of an asset
  getOriginDate :: a -> Date
  -- | Get origin info of an asset
  getOriginInfo :: a -> OriginalInfo  
  -- | if the asset is defaulted
  isDefaulted :: a -> Bool
  -- | project projected dates of an asset
  getPaymentDates :: a -> Int -> [Date]
  -- | get number of remaining payments
  getRemainTerms :: a -> Int
  -- | project asset cashflow under credit stress and interest assumptions
  projCashflow :: a -> Date -> A.AssetPerfAssumption -> Maybe [RateAssumption] -> CF.CashFlowFrame
  -- | Get possible number of borrower 
  getBorrowerNum :: a -> Int
  -- | Split asset per rates passed in 
  splitWith :: a -> [Rate] -> [a]
  -- | ! Change the origination date of an asset
  updateOriginDate :: a -> Date -> a
  -- | ! Internal use
  calcAlignDate :: a -> Date -> Date
  calcAlignDate ast d = let 
                          payDates = getPaymentDates ast 0
                          remainTerms = getRemainTerms ast 
                          benchDate = (reverse payDates)!!remainTerms
                          offset = daysBetween benchDate d
                        in 
                          T.addDays offset $ getOriginDate ast
                          
  {-# MINIMAL calcCashflow,getCurrentBal,getOriginBal,getOriginRate #-}



data Pool a = Pool {assets :: [a]                                           -- ^ a list of assets in the pool
                   ,futureCf :: Maybe CF.CashFlowFrame                      -- ^ projected cashflow from the assets in the pool
                   ,asOfDate :: Date                                        -- ^ date of the assets/pool cashflow
                   ,issuanceStat :: Maybe (Map.Map IssuanceFields Balance)  -- ^ other misc balance data
                   }deriving (Show,Generic)

-- | get stats of pool 
getIssuanceField :: Pool a -> IssuanceFields -> Centi
getIssuanceField p@Pool{issuanceStat = Just m} s
  = case Map.lookup s m of
      Just r -> r
      Nothing -> error ("Failed to lookup "++show s++" in stats "++show m)
getIssuanceField Pool{issuanceStat = Nothing} _ 
  = error "There is no pool stats"


-- | calculate period payment (Annuity/Level mortgage)
calcPmt :: Balance -> IRate -> Int -> Amount
calcPmt bal periodRate periods =
  let
    periodRate1 = toRational periodRate
    r1 =  ((1+periodRate1)^^periods) / ((1+periodRate1)^^periods-1) -- `debug` ("PR>>"++show periodRate)
    pmtFactor = periodRate1 * r1 -- `debug` ("R1>>"++ show r1)
  in
    mulBR bal pmtFactor -- `debug` ("Factor"++ show pmtFactor)

-- | apply ExtraStress on prepayment/default rates
applyExtraStress :: Maybe A.ExtraStress -> [Date] -> [Rate] -> [Rate] -> ([Rate],[Rate])
applyExtraStress Nothing _ ppy def = (ppy,def)
applyExtraStress (Just ExtraStress{A.defaultFactors= mDefFactor
                                  ,A.prepaymentFactors = mPrepayFactor}) ds ppy def =
  case (mPrepayFactor,mDefFactor) of
    (Nothing,Nothing) -> (ppy,def)
    (Nothing,Just defFactor) -> (ppy ,getTsVals $ multiplyTs Exc (zipTs ds def) defFactor)
    (Just ppyFactor,Nothing) -> (getTsVals $ multiplyTs Exc (zipTs ds ppy) ppyFactor, def)
    (Just ppyFactor,Just defFactor) -> (getTsVals $ multiplyTs Exc (zipTs ds ppy) ppyFactor
                                       ,getTsVals $ multiplyTs Exc (zipTs ds def) defFactor)


-- | build pool assumption rate (prepayment, defaults, recovery rate , recovery lag)
buildAssumptionPpyDefRecRate :: [Date] -> A.AssetPerfAssumption -> ([Rate],[Rate],Rate,Int)
buildAssumptionPpyDefRecRate ds (A.LoanAssump mDa mPa mRa mESa) = buildAssumptionPpyDefRecRate ds ((A.MortgageAssump mDa mPa mRa mESa))
buildAssumptionPpyDefRecRate ds (A.MortgageAssump mDa mPa mRa mESa)
  = (prepayRates2,defaultRates2,recoveryRate,recoveryLag)
    where 
      size = length ds
      zeros = replicate size 0.0
      prepayRates = case mPa of
                      Nothing -> zeros
                      Just (A.PrepaymentConstant r) -> replicate size r
                      Just (A.PrepaymentCPR r) -> (map (Util.toPeriodRateByInterval r)
                                                       (getIntervalDays ds))
                      Just (A.PrepaymentVec vs) -> zipWith 
                                                     Util.toPeriodRateByInterval
                                                     (paddingDefault 0.0 vs (pred size))
                                                     (getIntervalDays ds)
                      _ -> error ("failed to find prepayment type"++ show mPa)

      defaultRates = case mDa of 
                       Nothing -> zeros
                       Just (A.DefaultConstant r) ->  replicate size r
                       Just (A.DefaultCDR r) -> (map (Util.toPeriodRateByInterval r)
                                                     (getIntervalDays ds))
                       Just (A.DefaultVec vs) -> zipWith 
                                                   Util.toPeriodRateByInterval
                                                   (paddingDefault 0.0 vs (pred size))
                                                   (getIntervalDays ds)
                       _ -> error ("failed to find prepayment type"++ show mDa)
      
      (recoveryRate,recoveryLag) = case mRa of 
                                     Nothing -> (0,0)
                                     Just (A.Recovery (r,lag)) -> (r,lag)

      (prepayRates2,defaultRates2) = applyExtraStress mESa ds prepayRates defaultRates


-- calculate Level P&I type mortgage cashflow
_calcPiFlow :: Amount -> Balance -> [Balance] -> [Amount] -> [Amount] -> [IRate] -> [Bool] -> ([Balance],CF.Principals,CF.Interests)
_calcPiFlow pmt last_bal bals ps is [] _ = (bals,ps,is)
_calcPiFlow pmt last_bal bals ps is (r:rs) (flag:flags)
  | last_bal < 0.01  =  (bals,ps,is)
  | otherwise
    = _calcPiFlow pmt new_bal (bals++[new_bal]) (ps++[new_prin]) (is++[new_int]) rs flags
      where
        new_int = mulBI last_bal r
        new_prin = pmt - new_int
        new_bal = last_bal - new_prin
        new_pmt = if flag then 
                    calcPmt new_bal (head rs) (length rs)
                  else
                    pmt
                
-- Dates -> include begining balance
-- Rates -> length Dates - 1
calcPiFlow :: DayCount -> Balance -> Amount -> [Date] -> [IRate] -> ([Balance],CF.Principals,CF.Interests)
calcPiFlow dc bal pmt dates rs =
  _calcPiFlow pmt bal [] [] [] period_r resetFlags
    where
      size = length dates
      resetFlags = A.calcResetDates rs []
      period_r = [ calcIntRate (dates!!d) (dates!!(d+1)) (rs!!d) dc | d <- [0..size-2]]

_calc_p_i_flow_even :: Amount -> Balance -> [Balance] -> [Amount] -> [Amount] -> [IRate] -> ([Balance],CF.Principals,CF.Interests)
_calc_p_i_flow_even evenPrin last_bal bals ps is [] = (bals,ps,is) -- `debug` ("Return->"++show(bals)++show(is))
_calc_p_i_flow_even evenPrin last_bal bals ps is (r:rs)
  | last_bal < 0.01 = (bals,ps,is)
  | otherwise
    = _calc_p_i_flow_even evenPrin new_bal (bals++[new_bal]) (ps++[evenPrin]) (is++[new_int]) rs -- `debug` ("new bal"++show(new_bal)++"INT"++show(new_int)++">>R"++show(rs))
      where
        new_int = mulBI last_bal r
        new_bal = last_bal - evenPrin

calc_p_i_flow_even :: Amount -> Balance -> Dates -> IRate -> ([Balance],CF.Principals,CF.Interests)
calc_p_i_flow_even evenPrin bal dates r
  = _calc_p_i_flow_even evenPrin bal [] [] [] period_r  -- `debug` ("SIze of rates"++show(length period_r))
    where
      size = length dates
      period_r = [ calcIntRate (dates!!d) (dates!!(d+1)) r DC_ACT_360 | d <- [0..size-2]]

calc_p_i_flow_i_p :: Balance -> Dates -> IRate -> ([Balance],CF.Principals,CF.Interests)
calc_p_i_flow_i_p bal dates r
  = (_bals,_prins,_ints)
    where
      size =  length dates
      flow_size = pred $ length $ tail dates
      period_rs = [ calcIntRate (dates!!d) (dates!!(d+1)) r DC_ACT_360 | d <- [0..size-2]]
      _ints = [  mulBI bal _r | _r <- period_rs ]
      _bals = (replicate flow_size bal ) ++ [ 0 ]
      _prins = (replicate flow_size 0 ) ++ [ bal ]

calcRecoveriesFromDefault :: Balance -> Rate -> [Rate] -> [Amount]
calcRecoveriesFromDefault bal recoveryRate recoveryTiming
  = mulBR recoveryAmt <$> recoveryTiming
    where
      recoveryAmt = mulBR bal recoveryRate

priceAsset :: Asset a => a -> Date -> PricingMethod -> A.AssetPerfAssumption -> Maybe [RateAssumption] -> PriceResult
priceAsset m d (PVCurve curve) assumps mRates
  = let 
      CF.CashFlowFrame txns = projCashflow m d assumps mRates
      ds = getDate <$> txns 
      amts = CF.tsTotalCash <$> txns 
      pv = pv3 curve d ds amts
      cb =  getCurrentBal m
      wal = calcWAL ByYear cb d (zip amts ds)
    in 
      AssetPrice pv wal (-1) (-1) (-1)  --TODO missing duration and convixity

priceAsset m d (BalanceFactor currentFactor defaultedFactor) assumps mRates
  = let 
      cb =  getCurrentBal m
      val = case isDefaulted m of 
              False -> mulBR cb currentFactor 
              True  -> mulBR cb defaultedFactor
      CF.CashFlowFrame txns = projCashflow m d assumps mRates
      ds = getDate <$> txns 
      amts = CF.tsTotalCash <$> txns 
      wal = calcWAL ByYear cb d (zip amts ds) 
    in 
      AssetPrice val wal (-1) (-1) (-1)  --TODO missing duration and convixity

-- | Aggregate all cashflow into a single cashflow frame
aggPool :: [CF.CashFlowFrame]  -> CF.CashFlowFrame
aggPool [] = CF.CashFlowFrame []
aggPool xs = foldr1 CF.combine xs  

data AggregationRule = Regular Date Period
                     | Custom Date [Date]



$(deriveJSON defaultOptions ''Pool)
$(deriveJSON defaultOptions ''AggregationRule)
