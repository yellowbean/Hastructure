{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Asset (Pool(..),OriginalInfo(..),calc_p_i_flow
       ,aggPool,RateType(..),AmortPlan(..)
       ,Status(..),IssuanceFields(..)
       ,Asset(..),AggregationRule
       ,getIssuanceField,calcPmt
       ,buildAssumptionRate,getRateAssumption
       ,calc_p_i_flow_even,calc_p_i_flow_i_p
) where

import qualified Data.Time as T
import qualified Data.Text as Text
import Text.Read (readMaybe)

import Lib (Period(..),genDates
           ,Ts(..),Index(..),periodRateFromAnnualRate,previousDate,toDate
           ,nextDate,getIntervalDays,zipWith9,mkTs,periodsBetween
           ,mulBI,mkRateTs)

import qualified Cashflow as CF -- (Cashflow,Amount,Interests,Principals)
import qualified Assumptions as A

import qualified Data.Map as Map
import Data.List
import Data.Maybe
import Data.Ratio
import Data.Aeson hiding (json)
import Language.Haskell.TH
import Data.Aeson.TH
import Data.Aeson.Types
import Types hiding (Current)
import Text.Printf
import Data.Fixed
import Util
import Debug.Trace
debug = flip trace

class Show a => Asset a where
  calcCashflow :: a -> Date -> CF.CashFlowFrame
  getCurrentBal :: a -> Balance
  getOriginBal :: a -> Balance
  getOriginRate :: a -> IRate
  isDefaulted :: a -> Bool
  getPaymentDates :: a -> Int -> [Date]
  projCashflow :: a -> Date -> [A.AssumptionBuilder] -> CF.CashFlowFrame

data IssuanceFields = IssuanceBalance
                    deriving (Show,Ord,Eq,Read)

instance ToJSONKey IssuanceFields where
  toJSONKey = toJSONKeyText (Text.pack . show)

instance FromJSONKey IssuanceFields where
  fromJSONKey = FromJSONKeyTextParser $ \t -> case readMaybe (Text.unpack t) of
    Just k -> pure k
    Nothing -> fail ("Invalid key: " ++ show t)

data Pool a = Pool {assets :: [a]
                   ,futureCf :: Maybe CF.CashFlowFrame
                   ,asOfDate :: Date
                   ,issuanceStat :: Maybe (Map.Map IssuanceFields Centi)}
                    deriving (Show)

getIssuanceField :: Pool a -> IssuanceFields -> Centi
getIssuanceField p _if
  = case issuanceStat p of
      Just m -> Map.findWithDefault 0.0 _if m
      Nothing -> 0.0

calcPmt :: Balance -> IRate -> Int -> Amount
calcPmt bal periodRate periods =
   let
     periodRate1 = toRational periodRate
     r1 =  ((1+periodRate1)^^periods) / ((1+periodRate1)^^periods-1) -- `debug` ("PR>>"++show periodRate)
     pmtFactor = periodRate1 * r1 -- `debug` ("R1>>"++ show r1)
   in
     mulBR bal pmtFactor -- `debug` ("Factor"++ show pmtFactor)

data RateType = Fix IRate
              | Floater Index Spread IRate Period (Maybe Floor)
              -- index, spread, initial-rate reset interval,floor
              deriving (Show)

data AmortPlan = Level   -- for mortgage
               | Even    -- for mortgage
               | I_P     -- interest only and principal due at last payment
               | F_P     -- fee based 
               | ScheduleRepayment Ts-- custom principal follow
               deriving (Show)

data Status = Current
            | Defaulted (Maybe Date)
            -- | Delinquency (Maybe Int)
            -- | Extended (Maybe T.Day)
            deriving (Show)

data OriginalInfo = MortgageOriginalInfo {
    originBalance :: Centi
    ,originRate :: RateType
    ,originTerm :: Int
    ,period :: Period
    ,startDate :: Date
    ,prinType :: AmortPlan }
  | LoanOriginalInfo {
     originBalance :: Centi
    ,originRate :: RateType
    ,originTerm :: Int
    ,period :: Period
    ,startDate :: Date
    ,prinType :: AmortPlan
    } deriving (Show)

buildAssumptionRate :: [Date]-> [A.AssumptionBuilder] -> [Rate] -> [Rate] -> Rate -> Int -> ([Rate],[Rate],Rate,Int) -- prepay rates,default rates,
buildAssumptionRate pDates (assump:assumps) _ppy_rates _def_rates _recovery_rate _recovery_lag = case assump of
       A.DefaultConstant r ->
           buildAssumptionRate pDates assumps _ppy_rates (replicate cf_dates_length r) _recovery_rate _recovery_lag
       A.PrepaymentConstant r ->
           buildAssumptionRate pDates assumps (replicate cf_dates_length r) _def_rates  _recovery_rate _recovery_lag
       A.Recovery (rr,rl) ->
           buildAssumptionRate pDates assumps _ppy_rates _def_rates  rr rl
       A.DefaultCDR r ->
           buildAssumptionRate pDates assumps _ppy_rates
                                              (map (A.toPeriodRateByInterval r)
                                                   (getIntervalDays pDates))
                                               _recovery_rate _recovery_lag
       A.PrepaymentCPR r -> -- TODO need to convert to annualized rate
           buildAssumptionRate pDates assumps (map (A.toPeriodRateByInterval r)
                                                   (getIntervalDays pDates))
                                              _def_rates
                                              _recovery_rate _recovery_lag
       A.PrepaymentFactors _ts -> 
           let
             ppy_ts = zipTs pDates _ppy_rates
             new_prepayment_rates = getTsVals $ multiplyTs ppy_ts _ts 
           in                   
             buildAssumptionRate pDates assumps 
                                              new_prepayment_rates
                                              _def_rates
                                              _recovery_rate _recovery_lag

       A.DefaultFactors _ts -> 
           let
             def_ts = zipTs pDates _def_rates
             new_def_rates = getTsVals $ multiplyTs def_ts _ts 
           in                   
             buildAssumptionRate pDates assumps 
                                              _ppy_rates
                                              new_def_rates
                                              _recovery_rate _recovery_lag

       A.PrepaymentVec vs ->  
           let 
             _new_ppy = paddingDefault 0.0 vs (pred (length pDates))
             new_ppy = zipWith A.toPeriodRateByInterval _new_ppy (getIntervalDays pDates)
           in 
             buildAssumptionRate pDates assumps new_ppy _def_rates _recovery_rate _recovery_lag

       A.DefaultVec vs ->  
           let 
             _new_def = paddingDefault 0.0 vs (pred (length pDates))
             new_def = zipWith A.toPeriodRateByInterval _new_def (getIntervalDays pDates)
           in 
             buildAssumptionRate pDates assumps _ppy_rates new_def _recovery_rate _recovery_lag

       _ -> buildAssumptionRate pDates assumps _ppy_rates _def_rates _recovery_rate _recovery_lag
   where
     cf_dates_length = length pDates

buildAssumptionRate pDates [] _ppy_rates _def_rates _recovery_rate _recovery_lag 
  = (paddingDefault 0.0 _ppy_rates stressSize
     ,paddingDefault 0.0 _def_rates stressSize
     ,_recovery_rate
     ,_recovery_lag)
   where 
     stressSize = (pred (length pDates))

_calc_p_i_flow :: Amount -> Balance -> [Balance] -> [Amount] -> [Amount] -> [IRate] -> ([Balance],CF.Principals,CF.Interests)
_calc_p_i_flow pmt last_bal bals ps is [] = (bals,ps,is)
_calc_p_i_flow pmt last_bal bals ps is (r:rs)
  | last_bal < 0.01  =  (bals,ps,is)
  | otherwise
    = _calc_p_i_flow pmt new_bal (bals++[new_bal]) (ps++[new_prin]) (is++[new_int]) rs
      where
        new_int = mulBI last_bal r
        new_prin = pmt - new_int
        new_bal = last_bal - new_prin

calc_p_i_flow :: Balance -> Amount -> Dates -> IRate -> ([Balance],CF.Principals,CF.Interests)
calc_p_i_flow bal pmt dates r =
  _calc_p_i_flow pmt bal [] [] [] period_r
    where
      size = length dates
      period_r = [ calcIntRate (dates!!d) (dates!!(d+1)) r DC_ACT_360 | d <- [0..size-2]]

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

aggPool :: [CF.CashFlowFrame]  -> CF.CashFlowFrame
aggPool [] = undefined `debug` ("Empty cashflow from assets")
aggPool xs = foldr1 CF.combine xs  -- `debug` ("XS"++show(xs))

data AggregationRule = Regular Date Period
                     | Custom Date [Date]

getRateAssumption :: [A.AssumptionBuilder] -> Index -> Maybe A.AssumptionBuilder
getRateAssumption assumps idx
  = find (\x ->
            case x of
             A.InterestRateCurve _idx vs -> idx == _idx 
             A.InterestRateConstant _idx v -> idx == _idx
             _ -> False) assumps

$(deriveJSON defaultOptions ''Status)
$(deriveJSON defaultOptions ''OriginalInfo)
$(deriveJSON defaultOptions ''RateType)
$(deriveJSON defaultOptions ''Pool)
$(deriveJSON defaultOptions ''AmortPlan)
$(deriveJSON defaultOptions ''IssuanceFields)
$(deriveJSON defaultOptions ''AggregationRule)
