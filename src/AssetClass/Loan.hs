{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module AssetClass.Loan 
  (projectLoanFlow)
  where

import qualified Data.Time as T
import qualified Cashflow as CF -- (Cashflow,Amount,Interests,Principals)
import qualified Assumptions as A
import InterestRate
import Asset
import Lib
import Util
import Types
import qualified Data.Map as Map
import Data.List
import Data.Maybe
import Data.Aeson hiding (json)
import Language.Haskell.TH
import Data.Aeson.TH
import Data.Aeson.Types
import GHC.Generics

import AssetClass.AssetBase

import Debug.Trace
debug = flip trace


-- instance Asset Loan where
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

instance Asset Loan where
  calcCashflow pl@(PersonalLoan (LoanOriginalInfo ob or ot p sd ptype) _bal _rate _term _ ) asOfDay = 
    let 
      (_,futureTxns) = splitByDate txns asOfDay EqToRight
    in 
      CF.CashFlowFrame futureTxns
   where
      orate = getOriginRate pl
      pmt = calcPmt _bal (periodRateFromAnnualRate p _rate) _term
      cf_dates = lastN (_term + 1) $ sd:(getPaymentDates pl 0)
      l = (length cf_dates) - 1
      (b_flow,prin_flow,int_flow) = case ptype of
                                     Level -> calc_p_i_flow _bal pmt cf_dates _rate
                                     Even  -> calc_p_i_flow_even (_bal / fromIntegral _term) _bal cf_dates _rate
                                     I_P   -> calc_p_i_flow_i_p _bal cf_dates _rate
      txns =  zipWith9 CF.LoanFlow (tail cf_dates) b_flow prin_flow int_flow (replicate l 0.0) (replicate l 0.0) (replicate l 0.0) (replicate l 0.0) (replicate l _rate)  -- `debug` ("prin size "++ show (prin_flow)++ "date size"++ show (length cf_dates )++"int"++show (int_flow)++"ds"++ show (cf_dates))


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
 
  getOriginDate (PersonalLoan (LoanOriginalInfo ob or ot p sd I_P) cb cr rt st ) = sd
  
  getRemainTerms (PersonalLoan (LoanOriginalInfo ob or ot p sd I_P) cb cr rt st ) = rt

  updateOriginDate (PersonalLoan (LoanOriginalInfo ob or ot p sd I_P) cb cr rt st ) nd
    = PersonalLoan (LoanOriginalInfo ob or ot p nd I_P) cb cr rt st 

  getPaymentDates pl@(PersonalLoan (LoanOriginalInfo ob _ ot p sd _ ) _bal _rate _term _ )  extra
    = genDates sd p (ot+extra)

  projCashflow pl@(PersonalLoan (LoanOriginalInfo ob or ot p sd I_P) cb cr rt Current) asOfDay assumps =
    let 
      (_,futureTxns) = splitByDate txns asOfDay EqToRight
    in 
      CF.CashFlowFrame futureTxns
    where
      last_pay_date:cf_dates =  lastN (1 + rt + recovery_lag) $ sd:(getPaymentDates pl recovery_lag)
      cf_dates_length = length cf_dates  --  `debug` ("incoming assumption "++ show assumps)
      rate_vector = case or of
                      Fix r ->  replicate cf_dates_length cr   --calcIntRateCurve DC_ACT_360 r (last_pay_date:cf_dates) -- replicate cf_dates_length cr
                      Floater idx sprd _orate p mfloor ->
                              case A.getRateAssumption assumps idx of
                                Just (A.InterestRateCurve idx ps) ->  map (\x -> sprd + (fromRational x))   $ getValByDates ps Exc cf_dates
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
      adjusted_def_rates = map (\x -> (toRational x) * lifetime_default_pct) cf_factor -- `debug` ("Factors"++ show cf_factor ++ "SUM UP"++ show (sum cf_factor))
      txns = projectLoanFlow [] cb last_pay_date cf_dates adjusted_def_rates ppy_rates (replicate cf_dates_length 0.0) (replicate cf_dates_length 0.0) rate_vector (recovery_lag,recovery_rate) p I_P
      
      -- adjusted_ppy_rates = map (\x -> (toRational x) * lifetime_prepayment_pct) cf_factor

  projCashflow pl@(PersonalLoan (LoanOriginalInfo ob or ot p sd prinPayType) cb cr rt Current) asOfDay assumps =
    let 
      (_,futureTxns) = splitByDate txns asOfDay EqToRight
    in 
      CF.CashFlowFrame futureTxns
    where
      last_pay_date:cf_dates = lastN (rt + recovery_lag + 1) $ sd:(getPaymentDates pl recovery_lag)
      cf_dates_length = length cf_dates  --  `debug` ("incoming assumption "++ show assumps)
      rate_vector = case or of
                      Fix r ->  replicate cf_dates_length cr   --calcIntRateCurve DC_ACT_360 r (last_pay_date:cf_dates) -- replicate cf_dates_length cr
                      Floater idx sprd _orate p mfloor ->
                              case A.getRateAssumption assumps idx of
                                Just (A.InterestRateCurve idx ps) ->  map (\x -> sprd + (fromRational x))   $ getValByDates ps Exc cf_dates
                                Just (A.InterestRateConstant idx v) ->  map (\x -> sprd + x) $ replicate cf_dates_length v
                                Nothing -> replicate cf_dates_length 0.0
      (ppy_rates,def_rates,recovery_rate,recovery_lag) = buildAssumptionRate (last_pay_date:cf_dates) assumps
                               (replicate cf_dates_length 0.0)
                               (replicate cf_dates_length 0.0)
                               0
                               0
      txns = projectLoanFlow [] cb last_pay_date cf_dates def_rates ppy_rates (replicate cf_dates_length 0.0) (replicate cf_dates_length 0.0) rate_vector (recovery_lag,recovery_rate) p prinPayType  -- `debug` ("rate"++show rate_vector)

  projCashflow m@(PersonalLoan (LoanOriginalInfo ob or ot p sd prinPayType) cb cr rt (Defaulted (Just defaultedDate))) asOfDay assumps
    = case find f assumps of 
        Nothing -> CF.CashFlowFrame $ [CF.LoanFlow asOfDay cb 0 0 0 0 0 0 cr]
        Just (A.DefaultedRecovery rr lag timing) -> 
          let 
            (cf_dates1,cf_dates2) = splitAt lag $ genDates defaultedDate p (lag+ length timing)
            beforeRecoveryTxn = [  CF.LoanFlow d cb 0 0 0 0 0 0 cr | d <- cf_dates1 ]
            recoveries = calcRecoveriesFromDefault cb rr timing
            bals = scanl (-) cb recoveries
            _txns = [  CF.LoanFlow d b 0 0 0 0 r 0 cr | (b,d,r) <- zip3 bals cf_dates2 recoveries ]
            (_, txns) = splitByDate (beforeRecoveryTxn++_txns) asOfDay EqToRight -- `debug` ("AS OF Date"++show asOfDay)
          in 
            CF.CashFlowFrame txns
       where 
           f x = case x of 
                   A.DefaultedRecovery _ _ _ ->True 
                   _ -> False 

  projCashflow m@(PersonalLoan (LoanOriginalInfo ob or ot p sd prinPayType) cb cr rt (Defaulted Nothing)) asOfDay assumps
    = CF.CashFlowFrame $ [CF.LoanFlow asOfDay cb 0 0 0 0 0 0 cr]
