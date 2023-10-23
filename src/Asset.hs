{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Asset (Pool(..),aggPool
       ,Asset(..),AggregationRule
       ,getIssuanceField,calcPmt
       ,calcPiFlow,calc_p_i_flow_even,calc_p_i_flow_i_p
       ,buildAssumptionPpyDefRecRate,buildAssumptionPpyDelinqDefRecRate
       ,calcRecoveriesFromDefault
       ,priceAsset,applyHaircut
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
                          payDates = getPaymentDates ast 0
                          remainTerms = getRemainTerms ast 
                          benchDate = (reverse payDates)!!(pred remainTerms) --  `debug` ("\nPayDates"++show payDates++"\nremain terms"++ show remainTerms)
                          offset = daysBetween benchDate d
                        in 
                          T.addDays offset $ getOriginDate ast
                          
  {-# MINIMAL calcCashflow,getCurrentBal,getOriginBal,getOriginRate #-}



data Pool a = Pool {assets :: [a]                                           -- ^ a list of assets in the pool
                   ,futureCf :: Maybe CF.CashFlowFrame                      -- ^ projected cashflow from the assets in the pool
                   ,asOfDate :: Date                                        -- ^ include cashflow after this date 
                   ,issuanceStat :: Maybe (Map.Map CutoffFields Balance)    -- ^ cutoff balance of pool
                   ,extendPeriods :: Maybe DatePattern                      -- ^ dates for extend pool collection
                   } deriving (Show,Generic)

-- | get stats of pool 
getIssuanceField :: Pool a -> CutoffFields -> Centi
getIssuanceField p@Pool{issuanceStat = Just m} s
  = case Map.lookup s m of
      Just r -> r
      Nothing -> error ("Failed to lookup "++show s++" in stats "++show m)
getIssuanceField Pool{issuanceStat = Nothing} _ 
  = error "There is no pool stats"


-- | calculate period payment (Annuity/Level mortgage)
calcPmt :: Balance -> IRate -> Int -> Amount
calcPmt bal 0.0 periods = divideBI bal periods
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


-- | apply haircuts to cashflow from a stress map

applyHaircut :: Maybe A.ExtraStress -> CF.CashFlowFrame -> CF.CashFlowFrame
applyHaircut Nothing cf = cf 
applyHaircut (Just ExtraStress{A.poolHairCut = Nothing}) cf = cf
applyHaircut (Just ExtraStress{A.poolHairCut = Just haircuts}) (CF.CashFlowFrame txns)
  = CF.CashFlowFrame $ 
      (\txn -> foldr 
                 (\fn acc -> fn acc ) 
                 txn 
                 (applyHaircutTxn <$> haircuts) ) <$> txns
    where
      applyHaircutTxn (CollectedInterest,r) 
                      (CF.MortgageDelinqFlow d bal prin interest ppy delinq def recovery loss irate mbn mppn mst) 
        = CF.MortgageDelinqFlow d bal prin (mulBR interest (1-r)) ppy delinq def recovery loss irate mbn mppn mst
      applyHaircutTxn (CollectedPrincipal,r)
                      (CF.MortgageDelinqFlow d bal prin interest ppy delinq def recovery loss irate mbn mppn mst) 
        = CF.MortgageDelinqFlow d bal (mulBR prin (1-r)) interest ppy delinq def recovery loss irate mbn mppn mst
      applyHaircutTxn (CollectedRecoveries,r)
                      (CF.MortgageDelinqFlow d bal prin interest ppy delinq def recovery loss irate mbn mppn mst) 
        = CF.MortgageDelinqFlow d bal prin interest ppy delinq def (mulBR recovery (1-r)) loss irate mbn mppn mst
      applyHaircutTxn (CollectedPrepayment,r)
                      (CF.MortgageDelinqFlow d bal prin interest ppy delinq def recovery loss irate mbn mppn mst) 
        = CF.MortgageDelinqFlow d bal prin interest (mulBR ppy (1-r)) delinq def recovery loss irate mbn mppn mst
      applyHaircutTxn (CollectedPrepaymentPenalty,r)
                      (CF.MortgageDelinqFlow d bal prin interest ppy delinq def recovery loss irate mbn mppn mst) 
        = CF.MortgageDelinqFlow d bal prin interest ppy delinq def recovery loss irate mbn ((\x -> mulBR x (1-r) ) <$> mppn) mst
      
      applyHaircutTxn (CollectedInterest,r) 
                      (CF.MortgageFlow d bal prin interest ppy def recovery loss irate mbn mppn mst) 
        = CF.MortgageFlow d bal prin (mulBR interest (1-r)) ppy def recovery loss irate mbn mppn mst
      applyHaircutTxn (CollectedPrincipal,r)
                      (CF.MortgageFlow d bal prin interest ppy def recovery loss irate mbn mppn mst) 
        = CF.MortgageFlow d bal (mulBR prin (1-r)) interest ppy def recovery loss irate mbn mppn mst
      applyHaircutTxn (CollectedRecoveries,r)
                      (CF.MortgageFlow d bal prin interest ppy def recovery loss irate mbn mppn mst) 
        = CF.MortgageFlow d bal prin interest ppy def (mulBR recovery (1-r)) loss irate mbn mppn mst
      applyHaircutTxn (CollectedPrepayment,r)
                      (CF.MortgageFlow d bal prin interest ppy def recovery loss irate mbn mppn mst) 
        = CF.MortgageFlow d bal prin interest (mulBR ppy (1-r)) def recovery loss irate mbn mppn mst
      applyHaircutTxn (CollectedPrepaymentPenalty,r)
                      (CF.MortgageFlow d bal prin interest ppy def recovery loss irate mbn mppn mst)
        = CF.MortgageFlow d bal prin interest ppy def recovery loss irate mbn ((\x -> mulBR x (1-r) ) <$> mppn) mst
      
      applyHaircutTxn _ _ = error "Not implemented"
        

buildPrepayRates :: [Date] -> Maybe A.AssetPrepayAssumption -> [Rate]
buildPrepayRates ds Nothing = replicate (pred (length ds)) 0.0
buildPrepayRates ds mPa = 
  case mPa of
    Just (A.PrepaymentConstant r) -> replicate size r
    Just (A.PrepaymentCPR r) -> (map (Util.toPeriodRateByInterval r)
                                     (getIntervalDays ds))
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
    Just (A.DefaultCDR r) -> (map (Util.toPeriodRateByInterval r)
                                  (getIntervalDays ds))
    Just (A.DefaultVec vs) -> zipWith 
                                Util.toPeriodRateByInterval
                                (paddingDefault 0.0 vs (pred size))
                                (getIntervalDays ds)
    _ -> error ("failed to find prepayment type"++ show mDa)    
  where
    size = length ds



-- | build pool assumption rate (prepayment, defaults, recovery rate , recovery lag)
buildAssumptionPpyDefRecRate :: [Date] -> A.AssetPerfAssumption -> ([Rate],[Rate],Rate,Int)
buildAssumptionPpyDefRecRate ds (A.LoanAssump mDa mPa mRa mESa) = buildAssumptionPpyDefRecRate ds ((A.MortgageAssump mDa mPa mRa mESa))
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
                                                ((map (Util.toPeriodRateByInterval r) (getIntervalDays ds))
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

priceAsset :: Asset a => a -> Date -> PricingMethod -> A.AssetPerf -> Maybe [RateAssumption] -> PriceResult
priceAsset m d (PVCurve curve) assumps mRates
  = let 
      (CF.CashFlowFrame txns,_) = projCashflow m d assumps mRates
      ds = getDate <$> txns 
      amts = CF.tsTotalCash <$> txns 
      pv = pv3 curve d ds amts -- `debug` ("pricing"++ show d++ show ds++ show amts)
      cb =  getCurrentBal m
      wal = calcWAL ByYear cb d (zip amts ds)
    in 
      AssetPrice pv wal (-1) (-1) (-1)  --TODO missing duration and convixity

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
      AssetPrice val wal (-1) (-1) (-1)  --TODO missing duration and convixity

-- | Aggregate all cashflow into a single cashflow frame
aggPool :: [(CF.CashFlowFrame, Map.Map CutoffFields Balance)]  -> (CF.CashFlowFrame, Map.Map CutoffFields Balance)
aggPool [] = (CF.CashFlowFrame [],Map.empty)
aggPool xs 
  = let
      cfs = fst <$> xs
      cf = foldr1 CF.combine cfs 
      stats = foldr1 (Map.unionWith (+)) $ snd <$> xs
    in
      (cf,stats)
    

data AggregationRule = Regular Date Period
                     | Custom Date [Date]



$(deriveJSON defaultOptions ''Pool)
$(deriveJSON defaultOptions ''AggregationRule)
