{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module AssetClass.Mortgage
  (Mortgage(..),)
  where

import qualified Data.Time as T
import qualified Cashflow as CF -- (Cashflow,Amount,Interests,Principals)
import qualified Assumptions as A
import Asset
import Types
import Lib
import Util

import qualified Data.Map as Map
import Data.List
import Data.Ratio
import Data.Maybe
import Data.Aeson hiding (json)
import Language.Haskell.TH
import Data.Aeson.TH
import Data.Aeson.Types

import Debug.Trace
debug = flip trace
projectMortgageFlow :: [CF.TsRow] -> Balance -> Maybe Rational -> Date -> Dates -> [DefaultRate] -> [PrepaymentRate] -> [Amount] -> [Amount] -> [IRate] -> (Int,Rate) -> Period -> AmortPlan -> [CF.TsRow]
projectMortgageFlow trs _bal mbn _last_date (_pdate:_pdates) (_def_rate:_def_rates) (_ppy_rate:_ppy_rates) _rec_vector@(_rec_amt:_rec_amts) _loss_vector@(_loss_amt:_loss_amts) (_rate:_rates) (recovery_lag,recovery_rate) p pt
  | _bal > 0.01 = projectMortgageFlow
                  (trs++[tr])
                  _end_bal
                  ( toRational <$> _new_mbn)
                  _pdate
                  _pdates
                  _def_rates
                  _ppy_rates
                  (tail _current_rec) -- (replace _rec_vector recovery_lag  _new_rec) -- `debug` ("Adding TR->>>"++show(tr))
                  (tail _current_loss) -- (replace _loss_vector recovery_lag _new_loss) -- `debug` ("Adding TR->>>"++show(tr))
                  _rates
                  (recovery_lag,recovery_rate)
                  p
                  pt -- `debug` ("New Rate"++ show _rates)
                where
               _remain_terms = 1 + max 0 ((length _pdates) - recovery_lag) -- `debug` ("IN mortgage flow"++ show _remain_terms)
               _new_default = mulBR _bal _def_rate
               _new_bal_after_default = _bal - _new_default
               _new_prepay = mulBR _new_bal_after_default _ppy_rate
               _new_bal_after_ppy = _new_bal_after_default - _new_prepay
               _new_int = mulBI _new_bal_after_ppy (periodRateFromAnnualRate p _rate)  -- `debug` ("Balance"++show(_new_bal_after_ppy))
               _pmt = calcPmt _new_bal_after_ppy (periodRateFromAnnualRate p _rate) _remain_terms
               _new_prin = case pt of
                              Level -> _pmt - _new_int -- `debug` ("PMT->"++ show _pmt)
                              Even ->  _new_bal_after_ppy / fromIntegral _remain_terms --(ob / (fromIntegral ot)) * (_new_bal_after_ppy / ob)

               _new_rec = mulBR _new_default recovery_rate
               _new_loss = mulBR _new_default (1 - recovery_rate)

               _current_rec = replace _rec_vector recovery_lag _new_rec
               _current_loss = replace _loss_vector recovery_lag _new_loss

               _end_bal = _new_bal_after_ppy - _new_prin
               _survive_rate = ((1 - _def_rate) * (1 - _ppy_rate))  
               --_temp = _survive_rate * (1 - (_new_prin % _new_bal_after_ppy))
               _temp = _survive_rate * (toRational (1 - _new_prin / _new_bal_after_ppy))
               _new_mbn = (\y -> fromInteger (round (_temp * (toRational y)))) <$> mbn
               tr = CF.MortgageFlow _pdate _end_bal _new_prin _new_int _new_prepay _new_default (head _current_rec) (head _current_loss) _rate _new_mbn

projectMortgageFlow trs _b mbn _last_date (_pdate:_pdates) _  _ (_rec_amt:_rec_amts) (_loss_amt:_loss_amts) _ _lag_rate _p _pt
 = projectMortgageFlow
    (trs++[tr])
    _b
    mbn
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
    tr = CF.MortgageFlow _pdate _b 0 0 0 0 _rec_amt _loss_amt 0.0 Nothing

projectMortgageFlow trs _ _ _ [] _ _ [] [] _ _ _ _ = trs   -- `debug` ("Ending trs=>"++show(trs))

projectScheduleFlow :: [CF.TsRow] -> Rate -> Balance -> [CF.TsRow] -> [DefaultRate] -> [PrepaymentRate] -> [Amount] -> [Amount] -> (Int, Rate) -> [CF.TsRow]
projectScheduleFlow trs bal_factor last_bal (flow:flows) (_def_rate:_def_rates) (_ppy_rate:_ppy_rates) _rec _loss (recovery_lag,recovery_rate)
  = projectScheduleFlow (trs++[tr]) _survive_rate _end_bal flows _def_rates _ppy_rates (tail _rec_vector) (tail _loss_vector) (recovery_lag,recovery_rate) -- `debug` ("===>C")
     where
       _start_bal = last_bal
       _def_amt = mulBR _start_bal _def_rate
       _ppy_amt = mulBR (_start_bal - _def_amt) _ppy_rate -- `debug` (">>>"++ show (_start_bal - _def_amt)++">>>"++ show (fromRational _ppy_rate) ++">>>"++ show ((_start_bal - _def_amt) * (fromRational _ppy_rate)))      -- `debug` (show _start_bal ++">>"++ show (fromRational _def_rate) ++ ">>" ++"DEF AMT"++ show _def_amt)-- `debug` ("Def amt"++show(_def_amt)++"Def rate"++show(_def_rate))
       _after_bal = _start_bal - _def_amt - _ppy_amt   -- `debug` ("PPY AMT"++ show _ppy_amt ++ ">>>" ++ show (fromRational _ppy_rate))
       _survive_rate = (1 - _def_rate) * (1 - _ppy_rate) * bal_factor -- `debug` ("Bal factor"++show(bal_factor))

       _schedule_bal = CF.mflowBalance flow

       _schedule_prin = (mulBR (CF.mflowPrincipal flow) _survive_rate) --TODO round trip  -- `debug` ("Schedule Principal"++(printf "%.2f" (CF.mflowPrincipal flow))++" Rate"++show(_schedule_rate))
       _schedule_int = (mulBR (CF.mflowInterest flow) _survive_rate)

       _new_rec = mulBR _def_amt recovery_rate
       _new_loss = mulBR _def_amt (1 - recovery_rate)

       _rec_vector = replace _rec recovery_lag _new_rec
       _loss_vector = replace _loss recovery_lag _new_loss

       _end_bal = max 0 $ _after_bal - _schedule_prin

       tr = CF.MortgageFlow (CF.getDate flow) _end_bal _schedule_prin _schedule_int _ppy_amt _def_amt (head _rec_vector) (head _loss_vector) 0.0 Nothing

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
      last_date = CF.getDate (last trs)
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
             Nothing

projectScheduleFlow trs _ last_bal [] _ _ [] [] (_,_) = trs -- `debug` ("===>C") --  `debug` ("End at "++show(trs))


data Mortgage = Mortgage OriginalInfo Balance IRate RemainTerms (Maybe BorrowerNum) Status
              | ScheduleMortgageFlow Date [CF.TsRow]
              deriving (Show)

instance Asset Mortgage  where
  calcCashflow m@(Mortgage (MortgageOriginalInfo ob or ot p sd ptype)  _bal _rate _term _mbn _) d =
      CF.CashFlowFrame $ zipWith10
                            CF.MortgageFlow
                              cf_dates
                              b_flow
                              prin_flow
                              int_flow
                              (replicate l 0.0)
                              (replicate l 0.0)
                              (replicate l 0.0)
                              (replicate l 0.0)
                              rate_used
                              --(replicate l Nothing)
                              bnflow
    where
      orate = getOriginRate m
      pmt = calcPmt _bal (periodRateFromAnnualRate p _rate) _term
      cf_dates = take _term $ filter (>= d) $ getPaymentDates m 0
      last_pay_date = previousDate (head cf_dates) p
      l = length cf_dates
      rate_used = case or of
                    Fix _r -> replicate l _r
                    Floater _ _ _ _ _ -> replicate l _rate

      (b_flow,prin_flow,int_flow) = case ptype of
                                     Level -> calc_p_i_flow 
                                                _bal 
                                                pmt 
                                                ([last_pay_date]++cf_dates) 
                                                _rate
                                     Even ->  calc_p_i_flow_even 
                                                (_bal / fromIntegral _term) 
                                                _bal 
                                                ([last_pay_date]++cf_dates) 
                                                _rate  
      bnflow = [ (\y -> (fromInteger (round ((toRational y) * (toRational (b / _bal)))))) <$> _mbn  | b <- b_flow ]
--      _new_mbn = (\y -> fromInteger (round (_temp * (toRational y)))) <$> mbn

  calcCashflow s@(ScheduleMortgageFlow beg_date flows)  d = CF.CashFlowFrame flows

  getCurrentBal (Mortgage x _bal _ _ _ _) = _bal

  getOriginBal (Mortgage (MortgageOriginalInfo _bal _ _ _ _ _ ) _ _ _ _ _ ) = _bal

  getOriginRate (Mortgage (MortgageOriginalInfo _ or _ _ _ _ ) _ _ _ _ _ )
    = case or of
       Fix _r -> _r
       Floater _ _ _r _ Nothing -> _r
       Floater _ _ _r _ (Just floor) -> max _r floor

  getPaymentDates (Mortgage (MortgageOriginalInfo _ _ ot p sd _) _ _ ct _ _) extra
    = genDates sd p (ot+ct+extra)

  isDefaulted (Mortgage _ _ _ _ _ (Defaulted _)) = True
  isDefaulted (Mortgage _ _ _ _ _ _) = False

  projCashflow m@(Mortgage (MortgageOriginalInfo ob or ot p sd prinPayType) cb cr rt mbn Current) asOfDay assumps =
    CF.CashFlowFrame $ projectMortgageFlow
                            []
                            cb
                            (toRational <$> mbn)
                            last_pay_date
                            cf_dates
                            def_rates
                            ppy_rates
                            (replicate cf_dates_length 0.0)
                            (replicate cf_dates_length 0.0)
                            rate_vector
                            (recovery_lag,recovery_rate)
                            p
                            prinPayType 
    where
      cf_dates = take (rt+recovery_lag) $ filter (> asOfDay) (getPaymentDates m recovery_lag) --  `debug` ("CF Dates"++show(recovery_lag))
      last_pay_date = previousDate (head cf_dates) p 
      cf_dates_length = length cf_dates 
      rate_vector = case or of
                      Fix r ->  replicate cf_dates_length r
                      Floater idx sprd _orate p mfloor ->
                              case getRateAssumption assumps idx of
                                Just (A.InterestRateCurve idx ps) ->  map (\x -> sprd + (fromRational x))   $ getValByDates (mkRateTs ps) Exc cf_dates
                                Just (A.InterestRateConstant idx v) ->  map (\x -> sprd + x) $ replicate cf_dates_length v
                                Nothing -> replicate cf_dates_length 0.0

      (ppy_rates,def_rates,recovery_rate,recovery_lag) = buildAssumptionRate (last_pay_date:cf_dates) assumps
                               (replicate cf_dates_length 0.0)
                               (replicate cf_dates_length 0.0)
                               0
                               0

  projCashflow m@(Mortgage (MortgageOriginalInfo ob or ot p sd prinPayType) cb cr rt mbn (Defaulted _) ) asOfDay assumps
    = CF.CashFlowFrame $ [CF.MortgageFlow asOfDay cb 0 0 0 0 0 0 cr mbn]

  projCashflow (ScheduleMortgageFlow beg_date flows) asOfDay assumps
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
                             -- `debug` ("PPY Rate for cf table"++show ppy_rates++"DEF"++show def_rates)

       where
        beg_bal =  CF.mflowBegBalance $ head flows
        (ppy_rates,def_rates,recovery_rate,recovery_lag) = buildAssumptionRate (beg_date:cf_dates) assumps [] [] 0 0 -- `debug` ("Assumpt"++ show assumps)
        curve_dates_length =  recovery_lag + length flows
        temp_p = Lib.Monthly -- TODO to fix this hard code
        cf_dates = (map CF.getDate flows) ++ (genDates (CF.getDate (last flows)) temp_p recovery_lag)

  getBorrowerNum m@(Mortgage (MortgageOriginalInfo ob or ot p sd prinPayType) cb cr rt mbn _ ) 
    = fromMaybe 1 mbn


$(deriveJSON defaultOptions ''Mortgage)
