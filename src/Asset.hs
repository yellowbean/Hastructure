{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Asset (Mortgage(..),Pool(..),OriginalInfo(..),calc_p_i_flow
       ,aggPool,calcCashflow,getCurrentBal,getOriginBal,runPool2
       ,RateType(..),projCashflow,MortgageAmortPlan(..)
       ,Status(..),isDefaulted,IssuanceFields(..)
) where

import Data.Time (Day)
import qualified Data.Time as T
import qualified Data.Text as Text
import Text.Read (readMaybe)

import Lib (Period(..), Balance,calcInt,Dates,DayCount(..),calcIntRate,genDates
           ,Balance,Rate,Ts(..),Spread,Index(..),periodRateFromAnnualRate,previousDate,toDate
           ,getIntervalDays,zipWith9,getValByDates,mkTs)
import qualified Cashflow as CF -- (Cashflow,Amount,Interests,Principals)
import qualified Assumptions as A
import qualified Data.Map as Map
import Data.List
import Data.Aeson hiding (json)
import Language.Haskell.TH
import Data.Aeson.TH
import Data.Aeson.Types

import Debug.Trace
debug = flip trace

type PrepaymentRate = Float
type DefaultRate = Float
type RecoveryRate = Float
type Floor = Float


class Asset a where
  calcCashflow :: a -> CF.CashFlowFrame
  getCurrentBal :: a -> Float
  getOriginBal :: a -> Float
  getOriginRate :: a -> Float
  isDefaulted :: a -> Bool
  getPaymentDates :: a -> [T.Day]
  projCashflow :: a -> T.Day -> [A.AssumptionBuilder] -> CF.CashFlowFrame

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
                   ,asOfDate :: T.Day
                   ,issuanceStat :: Maybe (Map.Map IssuanceFields Float)}
                    deriving (Show)

calcPmt :: Float -> Float -> Int -> Float
calcPmt bal periodRate periods = 
    bal * (periodRate * (1+periodRate)^periods)/((1+periodRate)^periods-1)

replace :: [a] -> Int -> a -> [a]
replace xs i e = case splitAt i xs of
   (before, _:after) -> before ++ e: after
   _ -> xs

data RateType = Fix Rate
              | Floater Index Spread Rate Period (Maybe Floor) 
              -- index, spread, initial-rate reset interval,floor
              deriving (Show)

data MortgageAmortPlan = Level
                       | Even
              deriving (Show)

data Status = Current
            | Defaulted (Maybe T.Day)
            -- | Delinquency (Maybe Int)
            -- | Extended (Maybe T.Day)
            deriving (Show)

data OriginalInfo = OriginalInfo {
    originBalance::Float
    ,originRate:: RateType
    ,originTerm:: Int
    ,period:: Period
    ,startDate :: Day
    ,prinType :: MortgageAmortPlan
    } deriving (Show)

type RemainTerms = Int

data Mortgage = Mortgage OriginalInfo Balance Rate RemainTerms Status
              | ScheduleMortgageFlow [CF.TsRow]
              deriving (Show)
-- trs _bal _last_date (_pdate:_pdates) (_def_rate:_def_rates) (_ppy_rate:_ppy_rates) (_rec_amt:_rec_amts) (_loss_amt:_loss_amts) (_rate:_rates)

--data MortgageAssumption = Simple [Float] [Float] (Float,Int) -- default rate , prepayment rate, recovery rate, recovery lag
--                        | Deq [Float] [Float] (Float,Int) -- delinquency rate, default rate , prepayment rate, recovery rate, recovery lag
--                        | DeqStatus [Float] [Float] (Float,Int)
--
---- project cashflow with different input assumptions
--projCashflow :: Mortgage -> [[Float]] -> [[Float]]
--projCashflow m v
--  =
buildAssumptionRate :: [T.Day]-> [A.AssumptionBuilder] -> [Float] -> [Float] -> Float -> Int -> ([Float],[Float],Float,Int)
buildAssumptionRate pDates (assump:assumps) _ppy_rates _def_rates _recovery_rate _recovery_lag = case assump of
       A.DefaultConstant r ->
           buildAssumptionRate pDates assumps (replicate cf_dates_length r) _ppy_rates _recovery_rate _recovery_lag
       A.PrepaymentConstant r ->
           buildAssumptionRate pDates assumps _def_rates (replicate cf_dates_length r) _recovery_rate _recovery_lag
       A.Recovery (rr,rl) ->
           buildAssumptionRate pDates assumps _def_rates _ppy_rates rr rl
       A.DefaultCDR r ->
           buildAssumptionRate pDates assumps (map (A.toPeriodRateByInterval r)
                                                 (getIntervalDays pDates))
                                            _ppy_rates _recovery_rate _recovery_lag
       A.PrepaymentCPR r -> -- TODO need to convert to annualized rate
           buildAssumptionRate pDates assumps _def_rates
                                            (map (A.toPeriodRateByInterval r)
                                                 (getIntervalDays pDates))
                                            _recovery_rate _recovery_lag
       A.PrepaymentCPRCurve vs ->
           buildAssumptionRate pDates assumps _def_rates vs _recovery_rate _recovery_lag

       _ -> buildAssumptionRate pDates assumps _def_rates _ppy_rates _recovery_rate _recovery_lag
   where
     cf_dates_length = length pDates

buildAssumptionRate pDates [] _def_rates _ppy_rates _recovery_rate _recovery_lag = (_def_rates,_ppy_rates,_recovery_rate,_recovery_lag)

instance Asset Mortgage  where
  calcCashflow m@(Mortgage (OriginalInfo ob or ot p sd ptype)  _bal _rate _term _) =
      CF.CashFlowFrame $ zipWith9
                            CF.MortgageFlow
                              cf_dates
                              b_flow
                              prin_flow
                              int_flow
                              (replicate l 0.0)
                              (replicate l 0.0)
                              (replicate l 0.0)
                              (replicate l 0.0)
                              (replicate l 0.0)
    where
      orate = getOriginRate m
      pmt = calcPmt ob (periodRateFromAnnualRate p orate) ot
      cf_dates = getPaymentDates m
      l = length cf_dates
      (b_flow,prin_flow,int_flow) = case ptype of
                                     Level -> calc_p_i_flow _bal pmt cf_dates _rate
                                     Even ->  calc_p_i_flow_even
                                               (ob / fromIntegral ot) ob cf_dates _rate --  `debug` ("Even Pay"++show((ob / (fromIntegral ot))))
  calcCashflow s@(ScheduleMortgageFlow flows) = CF.CashFlowFrame flows

  getCurrentBal (Mortgage x _bal _ _ _) = _bal

  getOriginBal (Mortgage (OriginalInfo _bal _ _ _ _ _ ) _ _ _ _) = _bal

  getOriginRate (Mortgage (OriginalInfo _ or _ _ _ _ ) _ _ _ _)
    = case or of
       Fix _r -> _r
       Floater _ _ _r _ Nothing -> _r
       Floater _ _ _r _ (Just floor) -> max _r floor

  getPaymentDates (Mortgage (OriginalInfo _ _ ot p sd _) _ _ ct _)
    = genDates sd p (ot+ct)

  isDefaulted (Mortgage _ _ _ _ (Defaulted _)) = True
  isDefaulted (Mortgage _ _ _ _ _) = False

  projCashflow m@(Mortgage (OriginalInfo ob or ot p sd prinPayType) cb cr rt Current) asOfDay assumps =
    CF.CashFlowFrame $ _projCashflow [] cb last_pay_date cf_dates def_rates ppy_rates (replicate cf_recovery_length 0.0) (replicate cf_recovery_length 0.0) rate_vector -- `debug` ("RV"++show(rate_vector))
    where
      cf_dates = take rt $ filter (> asOfDay) $ getPaymentDates m
      last_pay_date = previousDate (head cf_dates) p -- `debug` ("RT->"++show rt++" cf-dates "++show cf_dates)
      cf_dates_length = length cf_dates -- `debug` ("CF dates=>"++show(cf_dates))
      rate_vector = case or of
                      Fix r ->  replicate cf_dates_length r
                      Floater idx sprd _orate p mfloor ->
                              case getRateAssumption assumps idx of
                                Just (A.InterestRateCurve idx ps) ->  map (+sprd)   $ getValByDates (mkTs ps) cf_dates
                                Just (A.InterestRateConstant idx v) ->  map (+sprd) $ replicate cf_dates_length v
                                Nothing -> (replicate cf_dates_length 0.0)

      cf_recovery_length = cf_dates_length + recovery_lag
      -- (def_rates,ppy_rates,recovery_rate,recovery_lag) = buildAssumpCurves (last_pay_date:cf_dates) assumps
      (def_rates,ppy_rates,recovery_rate,recovery_lag) = buildAssumptionRate (last_pay_date:cf_dates) assumps
                               (replicate cf_dates_length 0.0)
                               (replicate cf_dates_length 0.0) 
                               0
                               0

      _projCashflow 
           trs _bal _last_date (_pdate:_pdates) (_def_rate:_def_rates) (_ppy_rate:_ppy_rates) (_rec_amt:_rec_amts) (_loss_amt:_loss_amts) (_rate:_rates)
           | _bal > 0.01 = _projCashflow 
                                 (trs++[tr]) 
                                 _end_bal 
                                 _pdate 
                                 _pdates 
                                 _def_rates 
                                 _ppy_rates 
                                 (replace _rec_amts recovery_lag _new_rec) -- `debug` ("Adding TR->>>"++show(tr))
                                 (replace _loss_amts recovery_lag _new_loss) -- `debug` ("Adding TR->>>"++show(tr))
                                 _rates
           | otherwise = trs
           where
              _remain_terms = 1 + length _pdates
              _new_default = _bal * _def_rate
              _new_bal_after_default = _bal - _new_default
              _new_prepay = _new_bal_after_default * _ppy_rate
              _new_bal_after_ppy = _new_bal_after_default - _new_prepay
              _new_int = _new_bal_after_ppy * calcIntRate _last_date _pdate _rate ACT_360 -- `debug` ("Payment dates"++show(_pdate))
              _pmt = calcPmt _new_bal_after_ppy (periodRateFromAnnualRate p _rate) _remain_terms --  `debug` ("Remain Term"++show(_remain_terms))
              _new_prin = case prinPayType of
                             Level -> _pmt - _new_int
                             Even ->  _new_bal_after_ppy / fromIntegral _remain_terms --(ob / (fromIntegral ot)) * (_new_bal_after_ppy / ob)
              _new_rec = _new_default * recovery_rate
              _new_loss = _new_default * (1 - recovery_rate)
              _end_bal = _new_bal_after_ppy - _new_prin
              tr = CF.MortgageFlow _pdate _end_bal _new_prin _new_int _new_prepay _new_default _rec_amt _loss_amt _rate
      
      _projCashflow trs _bal _last_date [] _ _ _ _ _ = trs  -- `debug` ("Ending trs=>"++show(trs))

  projCashflow m@(Mortgage (OriginalInfo ob or ot p sd prinPayType) cb cr rt (Defaulted _) ) asOfDay assumps
    = CF.CashFlowFrame $ [CF.MortgageFlow asOfDay cb 0 0 0 0 0 0 cr]
    --  where

    --    totalRecovery = cb * rrate
    --    _projCashflow trs (_pdate:_pdates) (_rec_amt:_rec_amts) (_loss_amt:_loss_amts)
    --      = _projCashflow (trs++(CF.MortgageFlow _pdate cb 0 0 0 0 _rec_amt _loss_amt cr))
    --                      new_bal
    --                      _pdates
    --                      _pdates


    --        where
    --          new_bal = _bal - _rec_amt - _loss_amt

    --   recovery_lag = 10
    --   recovery_rate = 0.5
    --   total_recovery_amt = cb * recovery_rate
    --   recoveries = replicate recovery_lag $ (total_recovery_amt / recovery_lag)
    --   cf_dates = take (rt+recovery_lag) $ filter (> asOfDay) $ getPaymentDates m

  projCashflow (ScheduleMortgageFlow flows) asOfDay assumps
    = CF.CashFlowFrame $ flows
     where
       cf_dates_length = length flows
       cf_dates = map CF.tsDate flows
       last_pay_date = previousDate (head cf_dates) Lib.Monthly -- TODO hard code to Monthly
       (def_rates,ppy_rates,recovery_rate,recovery_lag) = buildAssumptionRate (last_pay_date:cf_dates) assumps
                              (replicate cf_dates_length 0.0)
                              (replicate cf_dates_length 0.0)
                              0
                              0
--instance Asset ScheduleMortgageFlow  where
--  calcCashflow (ScheduleMortgageFlow flow) = CF.CashFlowFrame flow


_calc_p_i_flow :: Float -> [Balance] -> [Float] -> [Float] -> [Rate] -> (CF.Balances,CF.Principals,CF.Interests)
_calc_p_i_flow pmt bals ps is [] = (bals,ps,is)
_calc_p_i_flow pmt bals ps is (r:rs)
  | last bals < 0.01  =  (bals,ps,is)
  | otherwise
    = _calc_p_i_flow pmt (bals++[new_bal]) (ps++[new_prin]) (is++[new_int]) rs
      where
        new_int = last bals * r
        new_prin = pmt - new_int
        new_bal = last bals - new_prin

calc_p_i_flow :: Balance -> Float -> Dates -> Rate -> (CF.Balances,CF.Principals,CF.Interests)
calc_p_i_flow bal pmt dates r =
  _calc_p_i_flow pmt [bal] [] [] period_r
    where
      size = length dates
      period_r = [ calcIntRate (dates!!d) (dates!!(d+1)) r ACT_360 | d <- [0..size-2]]

_calc_p_i_flow_even :: Float -> [Balance] -> [Float] -> [Float] -> [Rate] -> (CF.Balances,CF.Principals,CF.Interests)
_calc_p_i_flow_even evenPrin bals ps is [] = (bals,ps,is)
_calc_p_i_flow_even evenPrin bals ps is (r:rs)
  | last bals < 0.01 = (bals,ps,is)
  | otherwise
    = _calc_p_i_flow_even evenPrin (bals++[new_bal]) (ps++[evenPrin]) (is++[new_int]) rs
      where
        new_int = last bals * r
        new_bal = last bals - evenPrin

calc_p_i_flow_even :: Float -> Balance -> Dates -> Rate -> (CF.Balances,CF.Principals,CF.Interests)
calc_p_i_flow_even evenPrin bal dates r
  = _calc_p_i_flow_even evenPrin [bal] [] [] period_r
    where
      size = length dates
      period_r = [ calcIntRate (dates!!d) (dates!!(d+1)) r ACT_360 | d <- [0..size-2]]

data Loan = Loan OriginalInfo Balance Rate RemainTerms
                deriving (Show)

runPool2 :: Pool Mortgage -> [A.AssumptionBuilder]-> [CF.CashFlowFrame]
runPool2 (Pool as _ asof _) [] = map calcCashflow as
runPool2 (Pool as _ asof _) assumps = map (\x -> projCashflow x asof assumps) as

aggPool :: [CF.CashFlowFrame]  -> CF.CashFlowFrame
aggPool = foldr1 CF.combine 

getRateAssumption :: [A.AssumptionBuilder] -> Index -> Maybe A.AssumptionBuilder
getRateAssumption assumps idx
  = find (\x ->
            case x of
             A.InterestRateCurve _idx vs -> idx == _idx 
             A.InterestRateConstant _idx v -> idx == _idx
             _ -> False) assumps

$(deriveJSON defaultOptions ''Mortgage)
$(deriveJSON defaultOptions ''Status)
$(deriveJSON defaultOptions ''OriginalInfo)
$(deriveJSON defaultOptions ''RateType)
$(deriveJSON defaultOptions ''Pool)
$(deriveJSON defaultOptions ''MortgageAmortPlan)
$(deriveJSON defaultOptions ''IssuanceFields)
