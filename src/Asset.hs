{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Asset ( Asset(..)
       ,calcPiFlow,calc_p_i_flow_even,calc_p_i_flow_i_p
       ,buildAssumptionPpyDefRecRate,buildAssumptionPpyDelinqDefRecRate
       ,calcRecoveriesFromDefault
       ,priceAsset,applyHaircut,buildPrepayRates,buildDefaultRates
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
import AssetClass.AssetCashflow

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

import AssetClass.AssetBase ( OriginalInfo, calcPmt )

import Debug.Trace
import Assumptions (ExtraStress(ExtraStress))

import Control.Lens hiding (element)
import Control.Lens.TH


debug = flip trace

class (Show a,IR.UseRate a) => Asset a where
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
  projCashflow :: a -> Date -> A.AssetPerf -> Maybe [RateAssumption] -> (CF.CashFlowFrame, Map.Map CutoffFields Balance)
  -- | Get possible number of borrower 
  getBorrowerNum :: a -> Int
  -- | Split asset per rates passed in 
  splitWith :: a -> [Rate] -> [a]
  -- | ! Change the origination date of an asset
  updateOriginDate :: a -> Date -> a
  -- | ! Internal use
  calcAlignDate :: a -> Date -> Date
  calcAlignDate ast d = let 
                          payDates = getOriginDate ast:getPaymentDates ast 0
                          remainTerms = getRemainTerms ast 
                          benchDate = reverse payDates!! remainTerms  
                          offset = daysBetween benchDate d
                        in 
                          T.addDays offset $ getOriginDate ast
                          
  {-# MINIMAL calcCashflow,getCurrentBal,getOriginBal,getOriginRate #-}









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


buildPrepayRates :: [Date] -> Maybe A.AssetPrepayAssumption -> [Rate]
buildPrepayRates ds Nothing = replicate (pred (length ds)) 0.0
buildPrepayRates ds mPa = 
  case mPa of
    Just (A.PrepaymentConstant r) -> replicate size r
    Just (A.PrepaymentCPR r) -> Util.toPeriodRateByInterval r <$> getIntervalDays ds
    Just (A.PrepaymentVec vs) -> zipWith 
                                    Util.toPeriodRateByInterval
                                    (paddingDefault 0.0 vs (pred size))
                                    (getIntervalDays ds)
    _ -> error ("failed to find prepayment type"++ show mPa)
  where
    size = length ds

buildDefaultRates :: [Date] -> Maybe A.AssetDefaultAssumption -> [Rate]
buildDefaultRates ds Nothing = replicate (pred (length ds)) 0.0
buildDefaultRates ds mDa = 
  case mDa of
    Just (A.DefaultConstant r) ->  replicate size r
    Just (A.DefaultCDR r) -> Util.toPeriodRateByInterval r <$> getIntervalDays ds
    Just (A.DefaultVec vs) -> zipWith 
                                Util.toPeriodRateByInterval
                                (paddingDefault 0.0 vs (pred size))
                                (getIntervalDays ds)
    _ -> error ("failed to find prepayment type"++ show mDa)    
  where
    size = length ds



-- | build pool assumption rate (prepayment, defaults, recovery rate , recovery lag)
buildAssumptionPpyDefRecRate :: [Date] -> A.AssetPerfAssumption -> ([Rate],[Rate],Rate,Int)
buildAssumptionPpyDefRecRate ds (A.LoanAssump mDa mPa mRa mESa) = buildAssumptionPpyDefRecRate ds (A.MortgageAssump mDa mPa mRa mESa)
buildAssumptionPpyDefRecRate ds (A.MortgageAssump mDa mPa mRa mESa)
  = (prepayRates2,defaultRates2,recoveryRate,recoveryLag)
    where 
      size = length ds
      zeros = replicate size 0.0
      prepayRates = buildPrepayRates ds mPa
      defaultRates = buildDefaultRates ds mDa
      (recoveryRate,recoveryLag) = case mRa of 
                                     Nothing -> (0,0)
                                     Just (A.Recovery (r,lag)) -> (r,lag)

      (prepayRates2,defaultRates2) = applyExtraStress mESa ds prepayRates defaultRates

-- | build prepayment rates/ delinq rates and (%,lag) convert to default, recovery rate, recovery lag
buildAssumptionPpyDelinqDefRecRate :: [Date] -> A.AssetPerfAssumption -> ([Rate],[Rate],(Rate,Lag),Rate,Int)
buildAssumptionPpyDelinqDefRecRate ds (A.MortgageDeqAssump mDeqDefault mPa mRa (Just _)) = error "Delinq assumption doesn't support extra stress"
buildAssumptionPpyDelinqDefRecRate ds (A.MortgageDeqAssump mDeqDefault mPa mRa Nothing)
  = (prepayRates,delinqRates,(defaultPct,defaultLag),recoveryRate, recoveryLag)
    where 
      prepayRates = buildPrepayRates ds mPa
      (recoveryRate,recoveryLag) = case mRa of 
                                     Nothing -> (0,0)
                                     Just (A.Recovery (r,lag)) -> (r,lag)
      zeros = replicate (length ds) 0.0
      (delinqRates,defaultLag,defaultPct) = case mDeqDefault of
                                              Nothing -> (zeros,0,0.0)
                                              Just (A.DelinqCDR r (lag,pct)) -> 
                                                (map (Util.toPeriodRateByInterval r) (getIntervalDays ds)
                                                ,lag 
                                                ,pct)


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
      period_r = [ IR.calcIntRate (dates!!d) (dates!!(d+1)) (rs!!d) dc | d <- [0..size-2]]

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
      period_r = [ IR.calcIntRate (dates!!d) (dates!!(d+1)) r DC_ACT_360 | d <- [0..size-2]]

calc_p_i_flow_i_p :: Balance -> Dates -> IRate -> ([Balance],CF.Principals,CF.Interests)
calc_p_i_flow_i_p bal dates r
  = (_bals,_prins,_ints)
    where
      size =  length dates
      flowSize = pred $ length $ tail dates
      period_rs = [ IR.calcIntRate (dates!!d) (dates!!(d+1)) r DC_ACT_360 | d <- [0..size-2]]
      _ints = [  mulBI bal _r | _r <- period_rs ]
      _bals = replicate flowSize bal ++ [ 0 ]
      _prins = replicate flowSize 0 ++ [ bal ]

calcRecoveriesFromDefault :: Balance -> Rate -> [Rate] -> [Amount]
calcRecoveriesFromDefault bal recoveryRate recoveryTiming
  = mulBR recoveryAmt <$> recoveryTiming
    where
      recoveryAmt = mulBR bal recoveryRate

priceAsset :: Asset a => a -> Date -> PricingMethod -> A.AssetPerf -> Maybe [RateAssumption] -> PriceResult
priceAsset m d (PVCurve curve) assumps mRates
  = let 
      (CF.CashFlowFrame txns,_) = projCashflow m d assumps mRates
      ds = getDate <$> txns 
      amts = CF.tsTotalCash <$> txns 
      pv = pv3 curve d ds amts -- `debug` ("pricing"++ show d++ show ds++ show amts)
      cb =  getCurrentBal m
      wal = calcWAL ByYear cb d (zip amts ds)
      duration = calcDuration d (zip ds amts) curve
    in 
      AssetPrice pv wal duration (-1) (-1)  --TODO missing duration and convixity

priceAsset m d (BalanceFactor currentFactor defaultedFactor) assumps mRates
  = let 
      cb =  getCurrentBal m
      val = if isDefaulted m then 
              mulBR cb defaultedFactor -- `debug` ("Defulat CB"++ show cb)
            else
              mulBR cb currentFactor  -- `debug` ("CB"++ show cb)
      (CF.CashFlowFrame txns,_) = projCashflow m d assumps mRates
      ds = getDate <$> txns 
      amts = CF.tsTotalCash <$> txns 
      wal = calcWAL ByYear cb d (zip amts ds) -- `debug` ("pricing"++ show d++ show ds++ show amts)
    in 
      AssetPrice val wal (-1) (-1) (-1)  --TODO missing convixity
      
priceAsset m d (PvRate r) assumps mRates 
  = let 
      (CF.CashFlowFrame txns,_) = projCashflow m d assumps mRates
      cb =  getCurrentBal m
      ds = getDate <$> txns 
      amts = CF.tsTotalCash <$> txns 
      wal = calcWAL ByYear cb d (zip amts ds) 
      pv = sum $ zipWith (pv2 (fromRational r) d) ds amts
      curve = mkTs $ zip ds (repeat r)
      duration = calcDuration d (zip ds amts) curve
    in 
      AssetPrice pv wal (duration) (-1) (-1)  --TODO missing convixity 