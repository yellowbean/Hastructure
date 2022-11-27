{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Asset (Loan(..),Pool(..),OriginalInfo(..),calc_p_i_flow
       ,aggPool,calcCashflow,getCurrentBal,getOriginBal,runPool2
       ,RateType(..),projCashflow,AmortPlan(..)
       ,Status(..),isDefaulted,IssuanceFields(..)
       ,Asset(..),AggregationRule
       ,getIssuanceField,calcPmt
       ,buildAssumptionRate,getRateAssumption
       ,calc_p_i_flow_even
) where

import qualified Data.Time as T
import qualified Data.Text as Text
import Text.Read (readMaybe)

import Lib (Period(..),Dates,Date,genDates
           ,Balance,Rate,IRate,Ts(..),Spread,Index(..),periodRateFromAnnualRate,previousDate,toDate
           ,nextDate,Amount,getIntervalDays,zipWith9,mkTs,periodsBetween,Floor,Rate
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


class Asset a where
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
     -- pmtFactor = (periodRate * (1+periodRate)^periods)/((1+periodRate)^periods-1)
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
 



data Loan = PersonalLoan OriginalInfo Balance IRate RemainTerms Status
          | DUMMY
          deriving (Show)

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

buildAssumptionRate pDates [] _ppy_rates _def_rates _recovery_rate _recovery_lag = (_ppy_rates,_def_rates,_recovery_rate,_recovery_lag)


projectLoanFlow :: [CF.TsRow] -> Balance -> Date -> Dates 
                -> [DefaultRate] -> [PrepaymentRate] -> [Amount] -> [Amount] -> [IRate] -> (Int,Rate) -> Period 
                -> AmortPlan -> [CF.TsRow]
projectLoanFlow trs _bal _last_date (_pdate:_pdates) 
                (_def_rate:_def_rates) (_ppy_rate:_ppy_rates) _rec_vector@(_rec_amt:_rec_amts) _loss_vector@(_loss_amt:_loss_amts) (_rate:_rates) (recovery_lag,recovery_rate) 
                p pt
  | length _pdates >= recovery_lag = projectLoanFlow
                  (trs++[tr])
                  _end_bal
                  _pdate
                  _pdates
                  _def_rates
                  _ppy_rates
                  (tail _current_rec) -- (replace _rec_vector recovery_lag  _new_rec) -- `debug` ("Adding TR->>>"++show(tr))
                  (tail _current_loss) -- (replace _loss_vector recovery_lag _new_loss) -- `debug` ("Adding TR->>>"++show(tr))
                  _rates
                  (recovery_lag,recovery_rate)
                  p
                  pt  -- `debug` ("bal"++show _bal++"payment dates" ++ show _pdates ++  "Reocvery vector length"++ show (length _current_rec))
                where
               _remain_terms = 1 + max 0 ((length _pdates) - recovery_lag)
               _new_default = mulBR _bal _def_rate  --  `debug` ("REMAIN TERM>"++ show _remain_terms)
               _new_bal_after_default = _bal - _new_default -- `debug` ("DEF AMT"++ show _bal ++"D R"++ show _def_rate)
               _new_prepay = mulBR _new_bal_after_default _ppy_rate
               _new_bal_after_ppy = _new_bal_after_default - _new_prepay
               _int_rate = calcIntRate _last_date _pdate _rate DC_ACT_360 
               _new_int = case pt of 
                            F_P -> 0
                            _ -> mulBI _new_bal_after_ppy _int_rate -- `debug` ("Balance"++show(_new_bal_after_ppy)++"Rate>>"++show _int_rate )
               _pmt = calcPmt _new_bal_after_ppy _int_rate _remain_terms 
               _new_prin = case pt of 
                             I_P -> case _remain_terms of 
                                      1 -> _new_bal_after_ppy
                                      _ -> 0
                             F_P -> divideBI _new_bal_after_ppy _remain_terms
                             ScheduleRepayment _ts -> -1  --`debug` ("REmain terms>>>"++ show _remain_terms)

               _new_rec = mulBR _new_default recovery_rate
               _new_loss = mulBR _new_default (1 - recovery_rate)

               _current_rec = replace _rec_vector recovery_lag _new_rec
               _current_loss = replace _loss_vector recovery_lag _new_loss

               _end_bal = _new_bal_after_ppy - _new_prin
               tr = CF.LoanFlow _pdate _end_bal _new_prin _new_int _new_prepay _new_default (head _current_rec) (head _current_loss) _rate

projectLoanFlow trs _b _last_date (_pdate:_pdates) _  _ (_rec_amt:_rec_amts) (_loss_amt:_loss_amts) _ _lag_rate _p _pt
 = projectLoanFlow (trs++[tr]) _b _pdate _pdates [] [] _rec_amts _loss_amts [0.0] _lag_rate _p _pt  `debug` (">>> in recovery & Loss"++"pdates>"++show (length _pdates)++"rec>"++ show (length _rec_amts))
  where
    tr = CF.LoanFlow _pdate _b 0 0 0 0 _rec_amt _loss_amt 0.0

projectLoanFlow trs _ _ [] _ _ [] [] _ _ _ _ = trs -- `debug` ("===>C") --  `debug` ("End at "++show(trs))




        -- last_pay_date = previousDate (head cf_dates) temp_p -- `debug` ("CF Dates"++show(cf_dates))

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


instance Asset Loan where
  calcCashflow pl@(PersonalLoan (LoanOriginalInfo ob or ot p sd ptype) _bal _rate _term _ ) asOfDay = _cf 
   where
      orate = getOriginRate pl
      pmt = calcPmt _bal (periodRateFromAnnualRate p _rate) _term
      cf_dates = take (succ _term) $ sliceDates (SliceAfterKeepPrevious asOfDay) $ getPaymentDates pl 0
      l = (length cf_dates) - 1
      (b_flow,prin_flow,int_flow) = case ptype of
                                     Level -> calc_p_i_flow _bal pmt cf_dates _rate
                                     Even  -> calc_p_i_flow_even (_bal / fromIntegral _term) _bal cf_dates _rate
                                     I_P   -> calc_p_i_flow_i_p _bal cf_dates _rate
      _cf =  CF.CashFlowFrame $ zipWith9
                         CF.LoanFlow
                           (tail cf_dates)
                           b_flow
                           prin_flow
                           int_flow
                           (replicate l 0.0)
                           (replicate l 0.0)
                           (replicate l 0.0)
                           (replicate l 0.0)
                           (replicate l _rate)  -- `debug` ("prin size "++ show (prin_flow)++ "date size"++ show (length cf_dates )++"int"++show (int_flow)++"ds"++ show (cf_dates))


  getCurrentBal pl@(PersonalLoan (LoanOriginalInfo ob or ot p sd ptype ) _bal _rate _term _ )
    = _bal

  getOriginRate pl@(PersonalLoan (LoanOriginalInfo ob or ot p sd ptype ) _bal _rate _term _ )
    = case or of
       Fix _r -> _r
       Floater _ _ _r _ Nothing -> _r
       Floater _ _ _r _ (Just floor) -> max _r floor

  getOriginBal pl@(PersonalLoan (LoanOriginalInfo ob _ _ _ _ _) _ _ _ _ ) = ob

  isDefaulted pl@(PersonalLoan _ _ _ _ (Defaulted _)) = True
  isDefaulted pl@(PersonalLoan _ _ _ _ _ ) = False

  getPaymentDates pl@(PersonalLoan (LoanOriginalInfo ob _ ot p sd _ ) _bal _rate _term _ )  extra
    = genDates sd p (ot+_term+extra)

  projCashflow pl@(PersonalLoan (LoanOriginalInfo ob or ot p sd I_P) cb cr rt Current) asOfDay assumps =
    CF.CashFlowFrame $ projectLoanFlow
                            []
                            cb
                            last_pay_date
                            cf_dates
                            adjusted_def_rates
                            ppy_rates
                            (replicate cf_dates_length 0.0)
                            (replicate cf_dates_length 0.0)
                            rate_vector
                            (recovery_lag,recovery_rate)
                            p
                            I_P
    where
      last_pay_date:cf_dates = take ((succ rt) + recovery_lag) $ sliceDates (SliceAfterKeepPrevious asOfDay) $ getPaymentDates pl recovery_lag
      cf_dates_length = length cf_dates  --  `debug` ("incoming assumption "++ show assumps)
      rate_vector = case or of
                      Fix r ->  replicate cf_dates_length cr   --calcIntRateCurve DC_ACT_360 r (last_pay_date:cf_dates) -- replicate cf_dates_length cr
                      Floater idx sprd _orate p mfloor ->
                              case getRateAssumption assumps idx of
                                Just (A.InterestRateCurve idx ps) ->  map (\x -> sprd + (fromRational x))   $ getValByDates (mkRateTs ps) cf_dates
                                Just (A.InterestRateConstant idx v) ->  map (\x -> sprd + x) $ replicate cf_dates_length v
                                Nothing -> replicate cf_dates_length 0.0
      schedule_flow = calcCashflow pl asOfDay
      schedule_cf = map CF.tsTotalCash $ CF.getTsCashFlowFrame schedule_flow
      sum_cf = sum schedule_cf
      pdates = (CF.getDatesCashFlowFrame schedule_flow)
      cdr = fromMaybe 0.0 $ A.getCDR assumps
      -- cpr = fromMaybe 0.0 $ A.getCPR assumps
      proj_years = yearCountFraction DC_ACT_365F last_pay_date (last pdates)
      lifetime_default_pct = toRational $ proj_years * cdr 
      -- lifetime_prepayment_pct = toRational $ proj_years * cpr -- `debug` ("TOTAL DEF AMT"++show lifetime_default_pct)
      cf_factor = map (\x ->  (toRational x)  / (toRational sum_cf)) schedule_cf
      (ppy_rates,_,recovery_rate,recovery_lag) = buildAssumptionRate (last_pay_date:cf_dates) assumps
                               (replicate cf_dates_length 0.0)
                               (replicate cf_dates_length 0.0) 
                               0
                               0
      adjusted_def_rates = map (\x -> (toRational x) * lifetime_default_pct) cf_factor `debug` ("Factors"++ show cf_factor ++ "SUM UP"++ show (sum cf_factor))
      -- adjusted_ppy_rates = map (\x -> (toRational x) * lifetime_prepayment_pct) cf_factor

  projCashflow pl@(PersonalLoan (LoanOriginalInfo ob or ot p sd prinPayType) cb cr rt Current) asOfDay assumps =
    CF.CashFlowFrame $ projectLoanFlow
                            []
                            cb
                            last_pay_date
                            cf_dates
                            def_rates
                            ppy_rates
                            (replicate cf_dates_length 0.0)
                            (replicate cf_dates_length 0.0)
                            rate_vector
                            (recovery_lag,recovery_rate)
                            p
                            prinPayType  -- `debug` ("rate"++show rate_vector)
    where
      last_pay_date:cf_dates = take ((succ rt) + recovery_lag) $ sliceDates (SliceAfterKeepPrevious asOfDay) $ getPaymentDates pl recovery_lag
      cf_dates_length = length cf_dates  --  `debug` ("incoming assumption "++ show assumps)
      rate_vector = case or of
                      Fix r ->  replicate cf_dates_length cr   --calcIntRateCurve DC_ACT_360 r (last_pay_date:cf_dates) -- replicate cf_dates_length cr
                      Floater idx sprd _orate p mfloor ->
                              case getRateAssumption assumps idx of
                                Just (A.InterestRateCurve idx ps) ->  map (\x -> sprd + (fromRational x))   $ getValByDates (mkRateTs ps) cf_dates
                                Just (A.InterestRateConstant idx v) ->  map (\x -> sprd + x) $ replicate cf_dates_length v
                                Nothing -> replicate cf_dates_length 0.0
      (ppy_rates,def_rates,recovery_rate,recovery_lag) = buildAssumptionRate (last_pay_date:cf_dates) assumps
                               (replicate cf_dates_length 0.0)
                               (replicate cf_dates_length 0.0) 
                               0
                               0



runPool2 :: Asset a => (Pool a) -> [A.AssumptionBuilder]-> [CF.CashFlowFrame]
runPool2 (Pool as (Just cf) asof _) [] = [cf]
runPool2 (Pool as _ asof _) [] = map (\x -> calcCashflow x asof) as -- `debug` ("RUNPOOL"++ show (map (\x -> calcCashflow x asof) as ))
runPool2 (Pool as _ asof _) assumps 
  = map (\x -> projCashflow x asof assumps) as --  `debug` ("Assumping->" ++ show assumps)

aggPool :: [CF.CashFlowFrame]  -> CF.CashFlowFrame
aggPool xs = foldr1 CF.combine xs  -- `debug` ("XS"++show(xs))

data AggregationRule = Regular Date Period
                     | Custom Date [Date]

--projPoolCFs :: Pool Mortgage -> [A.AssumptionBuilder] -> AggregationRule -> CF.CashFlowFrame
--projPoolCFs pool as (Regular cutoffDay p) = CF.CashFlowFrame $ CF.aggTsByDates rightCfs intervals
--     where
--       intervals = genDates cutoffDay p $ fromIntegral periodsInRange
--       periodsInRange = (periodsBetween cutoffDay (CF.tsDate (last rightCfs)) p) + 1
--       rightCfs = case mRightCfs of
--                     Just (CF.CashFlowFrame txns) -> txns
--                     Nothing -> []
--       (mLeftCfs,mRightCfs) = CF.splitCashFlowFrameByDate aggCf cutoffDay -- `debug` ("pool aggCF"++ show aggCf)
--       poolCfs = runPool2 pool as
--       aggCf = aggPool poolCfs  -- `debug` ("pool CF"++ show poolCfs)

getRateAssumption :: [A.AssumptionBuilder] -> Index -> Maybe A.AssumptionBuilder
getRateAssumption assumps idx
  = find (\x ->
            case x of
             A.InterestRateCurve _idx vs -> idx == _idx 
             A.InterestRateConstant _idx v -> idx == _idx
             _ -> False) assumps

$(deriveJSON defaultOptions ''Loan)
$(deriveJSON defaultOptions ''Status)
$(deriveJSON defaultOptions ''OriginalInfo)
$(deriveJSON defaultOptions ''RateType)
$(deriveJSON defaultOptions ''Pool)
$(deriveJSON defaultOptions ''AmortPlan)
$(deriveJSON defaultOptions ''IssuanceFields)
$(deriveJSON defaultOptions ''AggregationRule)
