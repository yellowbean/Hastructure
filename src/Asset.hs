{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Asset (Mortgage(..),Pool(..),OriginalInfo(..),calc_p_i_flow
       ,aggPool,calcCashflow,getCurrentBal,getOriginBal,runPool2
       ,RateType(..),projCashflow,AmortPlan(..)
       ,Status(..),isDefaulted,IssuanceFields(..)
       ,Asset,projPoolCFs,AggregationRule
) where

import Data.Time (Day)
import qualified Data.Time as T
import qualified Data.Text as Text
import Text.Read (readMaybe)

import Lib (Period(..),calcInt,Dates,Date,DayCount(..),calcIntRate,genDates
           ,Balance,Rate,IRate,Ts(..),Spread,Index(..),periodRateFromAnnualRate,previousDate,toDate
           ,nextDate,Amount,getIntervalDays,zipWith9,getValByDates,mkTs,periodsBetween,Floor,Rate
           ,mulBI,mkRateTs)
import qualified Cashflow as CF -- (Cashflow,Amount,Interests,Principals)
import qualified Assumptions as A
import qualified Data.Map as Map
import Data.List
import Data.Maybe
import Data.Aeson hiding (json)
import Language.Haskell.TH
import Data.Aeson.TH
import Data.Aeson.Types
import Text.Printf
import Data.Fixed

import Debug.Trace
debug = flip trace

type PrepaymentRate = Rate
type DefaultRate = Rate
type RecoveryRate = Rate
type RemainTerms = Int


class Asset a where
  calcCashflow :: a -> Date -> CF.CashFlowFrame
  getCurrentBal :: a -> Balance
  getOriginBal :: a -> Balance
  getOriginRate :: a -> IRate
  isDefaulted :: a -> Bool
  getPaymentDates :: a -> Int -> [T.Day]
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
                   ,asOfDate :: T.Day
                   ,issuanceStat :: Maybe (Map.Map IssuanceFields Centi)}
                    deriving (Show)

calcPmt :: Balance -> IRate -> Int -> Amount
calcPmt bal periodRate periods =
   let
     pmtFactor =  (periodRate * (1+periodRate)^periods)/((1+periodRate)^periods-1)
   in
     mulBI bal pmtFactor

replace :: [a] -> Int -> a -> [a]
replace xs i e = case splitAt i xs of
   (before, _:after) -> before ++ e: after
   _ -> xs

data RateType = Fix IRate
              | Floater Index Spread IRate Period (Maybe Floor)
              -- index, spread, initial-rate reset interval,floor
              deriving (Show)

data AmortPlan = Level   -- for mortgage
               | Even    -- for mortgage
               | I_P -- interest only and principal due at last payment
               deriving (Show)

data Status = Current
            | Defaulted (Maybe T.Day)
            -- | Delinquency (Maybe Int)
            -- | Extended (Maybe T.Day)
            deriving (Show)

data OriginalInfo = OriginalInfo {
    originBalance::Centi
    ,originRate:: RateType
    ,originTerm:: Int
    ,period:: Period
    ,startDate :: Day
    ,prinType :: AmortPlan
    } deriving (Show)

data Mortgage = Mortgage OriginalInfo Balance IRate RemainTerms Status
              | ScheduleMortgageFlow [CF.TsRow]
              deriving (Show)

data ConsumerCredit = PersonalLoan OriginalInfo Balance IRate RemainTerms Status

buildAssumptionRate :: [T.Day]-> [A.AssumptionBuilder] -> [Rate] -> [Rate] -> Rate -> Int -> ([Rate],[Rate],Rate,Int)
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
       A.PrepaymentCPRCurve vs ->
           buildAssumptionRate pDates assumps vs _def_rates _recovery_rate _recovery_lag

       _ -> buildAssumptionRate pDates assumps _ppy_rates _def_rates _recovery_rate _recovery_lag
   where
     cf_dates_length = length pDates

buildAssumptionRate pDates [] _ppy_rates _def_rates _recovery_rate _recovery_lag = (_ppy_rates,_def_rates,_recovery_rate,_recovery_lag)

projectMortgageFlow :: [CF.TsRow] -> Balance -> Date -> Dates -> [DefaultRate] -> [PrepaymentRate] -> [Amount] -> [Amount] -> [IRate] -> (Int,Rate) -> Period -> AmortPlan -> [CF.TsRow]
projectMortgageFlow trs _bal _last_date (_pdate:_pdates) (_def_rate:_def_rates) (_ppy_rate:_ppy_rates) _rec_vector@(_rec_amt:_rec_amts) _loss_vector@(_loss_amt:_loss_amts) (_rate:_rates) (recovery_lag,recovery_rate) p pt
  | _bal > 0.01 = projectMortgageFlow
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
                  pt
                where
               _remain_terms = 1 + (length _pdates) - recovery_lag
               _new_default = _bal * (fromRational _def_rate)
               _new_bal_after_default = _bal - _new_default
               _new_prepay = _new_bal_after_default * (fromRational _ppy_rate)
               _new_bal_after_ppy = _new_bal_after_default - _new_prepay
               _new_int = mulBI _new_bal_after_ppy (calcIntRate _last_date _pdate _rate ACT_360) -- `debug` ("Payment dates"++show(_pdate))
               _pmt = calcPmt _new_bal_after_ppy (periodRateFromAnnualRate p _rate) _remain_terms --  `debug` ("Remain Term"++show(_remain_terms))
               _new_prin = case pt of
                              Level -> _pmt - _new_int
                              Even ->  _new_bal_after_ppy / fromIntegral _remain_terms --(ob / (fromIntegral ot)) * (_new_bal_after_ppy / ob)

               _new_rec = _new_default * (fromRational recovery_rate)
               _new_loss = _new_default * (fromRational (1 - recovery_rate))

               _current_rec = (replace _rec_vector recovery_lag _new_rec)
               _current_loss = (replace _loss_vector recovery_lag _new_loss)

               _end_bal = _new_bal_after_ppy - _new_prin
               tr = CF.MortgageFlow _pdate _end_bal _new_prin _new_int _new_prepay _new_default (head _current_rec) (head _current_loss) _rate

projectMortgageFlow trs _b _last_date (_pdate:_pdates) _  _ (_rec_amt:_rec_amts) (_loss_amt:_loss_amts) _ _lag_rate _p _pt
 = projectMortgageFlow
    (trs++[tr])-- projectMortgageFlow trs _b _last_date (_pdate:_pdates) _  _ (_rec_amt:_rec_amts) (_loss_amt:_loss_amts) _
    _b
    _pdate
    _pdates
    []
    []
    _rec_amts
    _loss_amts
    [0.0]
    _lag_rate
    _p
    _pt
  where
    tr = CF.MortgageFlow _pdate _b 0 0 0 0 _rec_amt _loss_amt 0.0

projectMortgageFlow trs _ _ [] _ _ [] [] _ _ _ _ = trs   -- `debug` ("Ending trs=>"++show(trs))

projectScheduleFlow :: [CF.TsRow] -> Rate -> Balance -> [CF.TsRow] -> [DefaultRate] -> [PrepaymentRate] -> [Amount] -> [Amount] -> (Int, Rate) -> [CF.TsRow]
projectScheduleFlow trs bal_factor last_bal (flow:flows) (_def_rate:_def_rates) (_ppy_rate:_ppy_rates) _rec _loss (recovery_lag,recovery_rate)
  = projectScheduleFlow (trs++[tr]) _survive_rate _end_bal flows _def_rates _ppy_rates (tail _rec_vector) (tail _loss_vector) (recovery_lag,recovery_rate) -- `debug` ("===>C")
     where
     _start_bal = last_bal
     _def_amt = _start_bal * (fromRational _def_rate)
     _ppy_amt = (_start_bal - _def_amt) * (fromRational _ppy_rate) -- `debug` ("Def amt"++show(_def_amt)++"Def rate"++show(_def_rate))
     _after_bal = _start_bal - _def_amt - _ppy_amt
     _survive_rate = (1 - _def_rate) * (1 - _ppy_rate) * bal_factor -- `debug` ("Bal factor"++show(bal_factor))

     _schedule_bal = (CF.mflowBalance flow)

     _schedule_prin = fromRational ( _survive_rate * (toRational (CF.mflowPrincipal flow))) --TODO round trip  -- `debug` ("Schedule Principal"++(printf "%.2f" (CF.mflowPrincipal flow))++" Rate"++show(_schedule_rate))
     _schedule_int = fromRational (_survive_rate * (toRational (CF.mflowInterest flow)))

     _new_rec = _def_amt * (fromRational recovery_rate)
     _new_loss = _def_amt * (fromRational (1 - recovery_rate))

     _rec_vector = replace _rec recovery_lag _new_rec
     _loss_vector = replace _loss recovery_lag _new_loss

     _end_bal = max 0 $ _after_bal - _schedule_prin

     tr = CF.MortgageFlow (CF.tsDate flow) _end_bal _schedule_prin _schedule_int _ppy_amt _def_amt (head _rec_vector) (head _loss_vector) 0.0

projectScheduleFlow trs b_factor last_bal [] _ _ (r:rs) (l:ls) (recovery_lag,recovery_rate)
  = projectScheduleFlow
      (trs++[tr])
      b_factor
      last_bal
      []
      []
      []
      rs
      ls
      (recovery_lag - 1,recovery_rate) --  `debug` ("===>B")
   where
      remain_length = length rs
      last_date = CF.tsDate (last trs)
      flow_date = nextDate last_date Lib.Monthly
      tr = CF.MortgageFlow
             flow_date
             last_bal
             0
             0
             0
             0
             r
             l
             0.0
projectScheduleFlow trs _ last_bal [] _ _ [] [] (_,_) = trs -- `debug` ("===>C") --  `debug` ("End at "++show(trs))

instance Asset Mortgage  where
  calcCashflow m@(Mortgage (OriginalInfo ob or ot p sd ptype)  _bal _rate _term _) d =
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
                              (replicate l 0.0) -- `debug` ("cfdates->"++show(cf_dates)++"final bals =>"++show(b_flow))
    where
      orate = getOriginRate m
      pmt = calcPmt ob (periodRateFromAnnualRate p orate) ot
      cf_dates = take _term $ filter (\x -> x >= d) $ getPaymentDates m 0
      last_pay_date = previousDate (head cf_dates) p
      l = length cf_dates

      (b_flow,prin_flow,int_flow) = case ptype of
                                     Level -> calc_p_i_flow _bal pmt cf_dates _rate
                                     Even ->  calc_p_i_flow_even (_bal / fromIntegral _term) _bal ([last_pay_date]++cf_dates) _rate -- `debug` show(calc_p_i_flow_even (_bal / fromIntegral _term) _bal ([last_pay_date]++cf_dates) _rate)
  calcCashflow s@(ScheduleMortgageFlow flows )  d = CF.CashFlowFrame flows

  getCurrentBal (Mortgage x _bal _ _ _) = _bal

  getOriginBal (Mortgage (OriginalInfo _bal _ _ _ _ _ ) _ _ _ _) = _bal

  getOriginRate (Mortgage (OriginalInfo _ or _ _ _ _ ) _ _ _ _)
    = case or of
       Fix _r -> _r
       Floater _ _ _r _ Nothing -> _r
       Floater _ _ _r _ (Just floor) -> max _r floor

  getPaymentDates (Mortgage (OriginalInfo _ _ ot p sd _) _ _ ct _) extra
    = genDates sd p (ot+ct+extra)

  isDefaulted (Mortgage _ _ _ _ (Defaulted _)) = True
  isDefaulted (Mortgage _ _ _ _ _) = False

  projCashflow m@(Mortgage (OriginalInfo ob or ot p sd prinPayType) cb cr rt Current) asOfDay assumps =
    CF.CashFlowFrame $ projectMortgageFlow
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
                            prinPayType  -- `debug` ("Payment dates=>"++show(cf_dates))
    where
      cf_dates = take (rt+recovery_lag) $ filter (> asOfDay) (getPaymentDates m recovery_lag) --  `debug` ("CF Dates"++show(recovery_lag))
      last_pay_date = previousDate (head cf_dates) p -- `debug` ("RT->"++show rt++" cf-dates "++show cf_dates)
      cf_dates_length = length cf_dates --  `debug` ("CF dates=>"++show(cf_dates))
      rate_vector = case or of
                      Fix r ->  replicate cf_dates_length r
                      Floater idx sprd _orate p mfloor ->
                              case getRateAssumption assumps idx of
                                Just (A.InterestRateCurve idx ps) ->  map (\x -> sprd + (fromRational x))   $ getValByDates (mkRateTs ps) cf_dates
                                Just (A.InterestRateConstant idx v) ->  map (\x -> sprd + x) $ replicate cf_dates_length v
                                Nothing -> (replicate cf_dates_length 0.0)

      (def_rates,ppy_rates,recovery_rate,recovery_lag) = buildAssumptionRate (last_pay_date:cf_dates) assumps
                               (replicate cf_dates_length 0.0)
                               (replicate cf_dates_length 0.0) 
                               0
                               0

  projCashflow m@(Mortgage (OriginalInfo ob or ot p sd prinPayType) cb cr rt (Defaulted _) ) asOfDay assumps
    = CF.CashFlowFrame $ [CF.MortgageFlow asOfDay cb 0 0 0 0 0 0 cr]

  projCashflow (ScheduleMortgageFlow flows) asOfDay assumps
    = CF.CashFlowFrame $ projectScheduleFlow
                             []
                             1.0
                             beg_bal
                             flows
                             def_rates
                             ppy_rates
                             (replicate curve_dates_length 0.0)
                             (replicate curve_dates_length 0.0)
                             (recovery_lag,recovery_rate)

       where
        beg_bal = (CF.mflowPrincipal (head flows) + CF.mflowBalance (head flows))
        (ppy_rates,def_rates,recovery_rate,recovery_lag) = buildAssumptionRate (last_pay_date:cf_dates) assumps [] [] 0 0
        curve_dates_length =  recovery_lag + length flows
        temp_p = Lib.Monthly
        cf_dates = (map CF.tsDate flows) ++ (genDates
                                              (CF.tsDate (last flows))
                                              temp_p
                                              recovery_lag)
        last_pay_date = previousDate (head cf_dates) temp_p -- `debug` ("CF Dates"++show(cf_dates))

_calc_p_i_flow :: Amount -> [Balance] -> [Amount] -> [Amount] -> [IRate] -> (CF.Balances,CF.Principals,CF.Interests)
_calc_p_i_flow pmt bals ps is [] = (bals,ps,is)
_calc_p_i_flow pmt bals ps is (r:rs)
  | last bals < 0.01  =  (bals,ps,is)
  | otherwise
    = _calc_p_i_flow pmt (bals++[new_bal]) (ps++[new_prin]) (is++[new_int]) rs
      where
        new_int = mulBI (last bals) r
        new_prin = pmt - new_int
        new_bal = last bals - new_prin

calc_p_i_flow :: Balance -> Amount -> Dates -> IRate -> (CF.Balances,CF.Principals,CF.Interests)
calc_p_i_flow bal pmt dates r =
  _calc_p_i_flow pmt [bal] [] [] period_r
    where
      size = length dates
      period_r = [ calcIntRate (dates!!d) (dates!!(d+1)) r ACT_360 | d <- [0..size-2]]

_calc_p_i_flow_even :: Amount -> Balance -> [Balance] -> [Amount] -> [Amount] -> [IRate] -> (CF.Balances,CF.Principals,CF.Interests)
_calc_p_i_flow_even evenPrin last_bal bals ps is [] = (bals,ps,is) -- `debug` ("Return->"++show(bals)++show(is))
_calc_p_i_flow_even evenPrin last_bal bals ps is (r:rs)
  | last_bal < 0.01 = (bals,ps,is)
  | otherwise
    = _calc_p_i_flow_even evenPrin new_bal (bals++[new_bal]) (ps++[evenPrin]) (is++[new_int]) rs -- `debug` ("new bal"++show(new_bal)++"INT"++show(new_int)++">>R"++show(rs))
      where
        new_int = mulBI last_bal r
        new_bal = last_bal - evenPrin

calc_p_i_flow_even :: Amount -> Balance -> Dates -> IRate -> (CF.Balances,CF.Principals,CF.Interests)
calc_p_i_flow_even evenPrin bal dates r
  = _calc_p_i_flow_even evenPrin bal [] [] [] period_r  -- `debug` ("SIze of rates"++show(length period_r))
    where
      size = length dates
      period_r = [ calcIntRate (dates!!d) (dates!!(d+1)) r ACT_360 | d <- [0..size-2]]

calc_p_i_flow_i_p :: Balance -> Dates -> IRate -> (CF.Balances,CF.Principals,CF.Interests)
calc_p_i_flow_i_p bal dates r
  = (_bals,_prins,_ints)
    where
      size = length dates
      period_rs = [ calcIntRate (dates!!d) (dates!!(d+1)) r ACT_360 | d <- [0..size-2]]
      _ints = [  mulBI bal _r | _r <- period_rs ]
      _bals = (replicate (size - 1) bal ) ++ [ 0 ]
      _prins = (replicate (size - 1) 0 ) ++ [ bal ]

instance Asset ConsumerCredit where
  calcCashflow pl@(PersonalLoan (OriginalInfo ob or ot p sd ptype ) _bal _rate _term _ ) d =
    CF.CashFlowFrame $ zipWith9
                         CF.PersonalLoanFlow
                           cf_dates
                           b_flow
                           prin_flow
                           int_flow
                           (replicate l 0.0)
                           (replicate l 0.0)
                           (replicate l 0.0)
                           (replicate l 0.0)
                           (replicate l _rate)
    where
      orate = getOriginRate pl
      pmt = calcPmt ob (periodRateFromAnnualRate p orate) ot
      cf_dates = getPaymentDates pl 0
      previous_paydate = d
      l = length cf_dates
      (b_flow,prin_flow,int_flow) = case ptype of
                                     Level -> calc_p_i_flow _bal pmt cf_dates _rate
                                     Even  -> calc_p_i_flow_even (ob / fromIntegral ot) ob cf_dates _rate
                                     I_P   -> calc_p_i_flow_i_p _bal cf_dates _rate

  getCurrentBal pl@(PersonalLoan (OriginalInfo ob or ot p sd ptype ) _bal _rate _term _ )
    = _bal

  getOriginRate pl@(PersonalLoan (OriginalInfo ob or ot p sd ptype ) _bal _rate _term _ )
    = case or of
       Fix _r -> _r
       Floater _ _ _r _ Nothing -> _r
       Floater _ _ _r _ (Just floor) -> max _r floor

  getOriginBal pl@(PersonalLoan (OriginalInfo ob or ot p sd ptype ) _bal _rate _term _ )
    = ob

  isDefaulted pl@(PersonalLoan (OriginalInfo ob or ot p sd ptype ) _bal _rate _term  (Defaulted _))
    = True

  isDefaulted pl@(PersonalLoan (OriginalInfo ob or ot p sd ptype ) _bal _rate _term _ )
    = False

  getPaymentDates pl@(PersonalLoan (OriginalInfo ob or ot p sd ptype ) _bal _rate _term (Defaulted _))  extra
    = genDates sd p (ot+_term+extra)

  projCashflow pl@(PersonalLoan (OriginalInfo ob or ot p sd ptype ) _bal _rate _term _ )  asOfDay assumps
    = calcCashflow pl asOfDay  -- TODO To fix



runPool2 :: Pool Mortgage -> [A.AssumptionBuilder]-> [CF.CashFlowFrame]
runPool2 (Pool as (Just cf) asof _) [] = [cf]
runPool2 (Pool as (Just (CF.CashFlowFrame mfs)) asof _) assumps
   =
    let
      smf = ScheduleMortgageFlow mfs
      -- existing_default_bal = foldl (\acc x -> (acc + (P.getCurrentBal x)))
      --                                     0.0 $
      --                                     filter (\a-> P.isDefaulted a ) as
    in
    [ projCashflow smf asof assumps] -- `debug` ("MFS"++show(mfs))
runPool2 (Pool as _ asof _) [] = map (\x -> calcCashflow x asof) as
runPool2 (Pool as _ asof _) assumps = map (\x -> projCashflow x asof assumps) as

aggPool :: [CF.CashFlowFrame]  -> CF.CashFlowFrame
aggPool xs = foldr1 CF.combine xs  -- `debug` ("XS"++show(xs))

data AggregationRule = Regular T.Day Period
                     | Custom T.Day [T.Day]

projPoolCFs :: Pool Mortgage -> [A.AssumptionBuilder] -> AggregationRule -> CF.CashFlowFrame
projPoolCFs pool as (Regular cutoffDay p)
     = CF.CashFlowFrame $ CF.aggTsByDates rightCfs intervals
     where
       -- groupedCfs = groupBy (\x -> periodsBetween cutoffDay (CF.tsDate x) p) rightCfs
       intervals = genDates cutoffDay p $ fromIntegral periodsInRange
       periodsInRange = (periodsBetween cutoffDay (CF.tsDate (last rightCfs)) p) + 1
       -- rightCfs = CF.getTsCashFlowFrame $ fromMaybe [] mRightCfs
       rightCfs = case mRightCfs of
                     Just (CF.CashFlowFrame txns) -> txns
                     Nothing -> []
       (mLeftCfs,mRightCfs) = CF.splitCashFlowFrameByDate aggCf cutoffDay
       aggCf = aggPool poolCfs
       poolCfs = runPool2 pool as

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
$(deriveJSON defaultOptions ''AmortPlan)
$(deriveJSON defaultOptions ''IssuanceFields)
$(deriveJSON defaultOptions ''AggregationRule)
