{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module AssetClass.Loan 
  (projectLoanFlow,updateOriginDate)
  where

import qualified Data.Time as T
import qualified Cashflow as CF -- (Cashflow,Amount,Interests,Principals)
import qualified Assumptions as A
import InterestRate
import Asset
import Lib
import Util
import DateUtil
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
import Assumptions (AssetDefaultAssumption(DefaultCDR))
debug = flip trace


-- instance Asset Loan where
projectLoanFlow :: [CF.TsRow] -> Rational -> Balance -> Date -> Dates
                -> [DefaultRate] -> [PrepaymentRate] -> [Amount] -> [Amount] -> [IRate] -> (Int,Rate) -> Period
                -> AmortPlan -> [CF.TsRow]
projectLoanFlow trs factor _bal _last_date (_pdate:_pdates)
                (_def_rate:_def_rates) (_ppy_rate:_ppy_rates) _rec_vector@(_rec_amt:_rec_amts) _loss_vector@(_loss_amt:_loss_amts) (_rate:_rates) (recovery_lag,recovery_rate)
                p pt
  | length _pdates >= recovery_lag = projectLoanFlow
                  (trs++[tr])
                  newFactor
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
               _remain_terms = 1 + max 0 (length _pdates - recovery_lag)
               _new_default = mulBR _bal _def_rate  --  `debug` ("REMAIN TERM>"++ show _remain_terms)
               _new_bal_after_default = _bal - _new_default -- `debug` ("DEF AMT"++ show _bal ++"D R"++ show _def_rate)
               _new_prepay = mulBR _new_bal_after_default _ppy_rate
               _new_bal_after_ppy = _new_bal_after_default - _new_prepay
               _int_rate = calcIntRate _last_date _pdate _rate DC_ACT_360
               _new_int = case pt of
                            F_P -> 0
                            _ -> mulBI _new_bal_after_ppy _int_rate -- `debug` ("Balance"++show(_new_bal_after_ppy)++"Rate>>"++show _int_rate )
               -- _pmt = calcPmt _new_bal_after_ppy _int_rate _remain_terms
               newFactor = factor * (1- _def_rate) * (1-_ppy_rate) 
               _new_prin = case pt of
                             I_P -> case _remain_terms of
                                      1 -> _new_bal_after_ppy
                                      _ -> 0
                             F_P -> divideBI _new_bal_after_ppy _remain_terms
                             ScheduleRepayment _ts _ ->  mulBR (fromRational (getValByDate _ts Inc _pdate)) newFactor `debug` ("Factor"++ show factor)

               _new_rec = mulBR _new_default recovery_rate
               _new_loss = mulBR _new_default (1 - recovery_rate)

               _current_rec = replace _rec_vector recovery_lag _new_rec
               _current_loss = replace _loss_vector recovery_lag _new_loss

               _end_bal = _new_bal_after_ppy - _new_prin
               tr = CF.LoanFlow _pdate _end_bal _new_prin _new_int _new_prepay _new_default (head _current_rec) (head _current_loss) _rate Nothing

projectLoanFlow trs _factor _b _last_date (_pdate:_pdates) _  _ (_rec_amt:_rec_amts) (_loss_amt:_loss_amts) _ _lag_rate _p _pt
 = projectLoanFlow (trs++[tr]) _factor _b _pdate _pdates [] [] _rec_amts _loss_amts [0.0] _lag_rate _p _pt  -- `debug` (">>> in recovery & Loss"++"pdates>"++show (length _pdates)++"rec>"++ show (length _rec_amts))
  where
    tr = CF.LoanFlow _pdate _b 0 0 0 0 _rec_amt _loss_amt 0.0 Nothing

projectLoanFlow trs _ _ _ [] _ _ [] [] _ _ _ _ = trs -- `debug` ("===>C") --  `debug` ("End at "++show(trs))

instance Asset Loan where
  calcCashflow pl@(PersonalLoan (LoanOriginalInfo ob or ot p sd ptype) bal rate term _ ) asOfDay mRates 
    = CF.CashFlowFrame $ cutBy Inc Future asOfDay txns 
      where
        orate = getOriginRate pl
        pmt = calcPmt bal (periodRateFromAnnualRate p rate) term
        cfDates = lastN (term + 1) $ sd:getPaymentDates pl 0
        l = pred (length cfDates)
        ratesUsed = A.projRates rate or mRates cfDates
        dc = getDayCount or
        (bFlow,prinFlow,intFlow) = case ptype of
                                        Level -> calcPiFlow dc bal pmt cfDates ratesUsed
                                        Even  -> calc_p_i_flow_even (bal / fromIntegral term) bal cfDates rate
                                        I_P   -> calc_p_i_flow_i_p bal cfDates rate
                                        ScheduleRepayment cf _ -> 
                                          let 
                                            periodIntervals = getIntervalFactors (sd:getTsDates cf)
                                            schedulePrin = fromRational <$> getTsVals cf
                                            bals = scanl (-) ob schedulePrin
                                            ints = [  mulBIR (mulBR b f) r   | (b,f,r) <- zip3 bals periodIntervals ratesUsed ]
                                          in 
                                            (lastN term bals,lastN term schedulePrin,lastN term ints)
        txns =  zipWith10 CF.LoanFlow (tail cfDates) bFlow prinFlow intFlow (replicate l 0.0) (replicate l 0.0) (replicate l 0.0) (replicate l 0.0) (replicate l rate) (replicate l Nothing) -- `debug` ("prin size "++ show (prin_flow)++ "date size"++ show (length cf_dates )++"int"++show (int_flow)++"ds"++ show (cf_dates))

  getCurrentBal pl@(PersonalLoan (LoanOriginalInfo ob or ot p sd ptype ) _bal _rate _term _ )
    = _bal

  getOriginRate pl@(PersonalLoan (LoanOriginalInfo ob or ot p sd ptype ) _bal _rate _term _ )
    = case or of
        Fix _ _r -> _r
        Floater _ _ _ _r _ _ _ _ -> _r 

  getOriginBal pl@(PersonalLoan (LoanOriginalInfo ob _ _ _ _ _) _ _ _ _ ) = ob

  isDefaulted pl@(PersonalLoan _ _ _ _ (Defaulted _)) = True
  isDefaulted PersonalLoan {} = False
 
  getOriginInfo (PersonalLoan oi cb cr rt st) = oi
  getOriginDate (PersonalLoan (LoanOriginalInfo ob or ot p sd I_P) cb cr rt st ) = sd
  
  getRemainTerms (PersonalLoan (LoanOriginalInfo ob or ot p sd I_P) cb cr rt st ) = rt

  updateOriginDate (PersonalLoan (LoanOriginalInfo ob or ot p sd I_P) cb cr rt st ) nd
    = PersonalLoan (LoanOriginalInfo ob or ot p nd I_P) cb cr rt st 

  getPaymentDates pl@(PersonalLoan (LoanOriginalInfo ob _ ot p sd (ScheduleRepayment ts mDp) ) _bal _rate _term _ ) extra
    = let 
        pdays = getTsDates ts 
        extraDates = genSerialDates (fromMaybe MonthEnd mDp) (last pdays) extra
      in 
        pdays ++ extraDates
  
  getPaymentDates pl@(PersonalLoan (LoanOriginalInfo ob _ ot p sd _ ) _bal _rate _term _ )  extra
    = genDates sd p (ot+extra)
  
  -- ^ <Special Case> Projection cashflow for loan with Interest only and bullet principal at end
  projCashflow pl@(PersonalLoan (LoanOriginalInfo ob or ot p sd I_P) cb cr rt Current) 
               asOfDay
               (A.LoanAssump defaultAssump prepayAssump recoveryAssump ams,_,_)
               mRates 
    = let 
        (futureTxns,historyM) = CF.cutoffTrs asOfDay txns 
      in 
        (applyHaircut ams (CF.CashFlowFrame futureTxns)
        ,historyM)
    where
      (ppyRates,defRates,recoveryRate,recoveryLag) = buildAssumptionPpyDefRecRate 
                                                       (lastPayDate:cfDates) 
                                                       (A.LoanAssump defaultAssump prepayAssump recoveryAssump Nothing)

      lastPayDate:cfDates =  lastN (1 + rt + recoveryLag) $ sd:getPaymentDates pl recoveryLag -- `debug` ("Pdays for IP" ++ show (getPaymentDates pl 0))
      cfDatesLength = length cfDates  --  `debug` ("incoming assumption "++ show assumps)
      rateVector = A.projRates cr or mRates cfDates
      
      txns = projectLoanFlow [] 1.0 cb lastPayDate cfDates defRates ppyRates (replicate cfDatesLength 0.0) (replicate cfDatesLength 0.0) rateVector (recoveryLag,recoveryRate) p I_P -- `debug` ("length>>"++show (length cf_dates)++ show (length adjusted_def_rates)++ show (length ppy_rates))
      
      -- adjusted_ppy_rates = map (\x -> (toRational x) * lifetime_prepayment_pct) cf_factor

  -- ^ Project cashflow for loans with schedule repayment
  projCashflow pl@(PersonalLoan (LoanOriginalInfo ob or ot p sd sr@(ScheduleRepayment ts _)) cb cr rt Current) 
               asOfDay 
               (A.LoanAssump defaultAssump prepayAssump recoveryAssump ams,_,_)
               mRate 
    = let 
        (futureTxns,historyM) = CF.cutoffTrs asOfDay txns 
      in 
        (applyHaircut ams (CF.CashFlowFrame futureTxns)
        ,historyM)
    where
      lastPayDate:cfDates = lastN (rt + recoveryLag + 1) $ sd:getPaymentDates pl recoveryLag
      cfDatesLength = length cfDates  --  `debug` ("incoming assumption "++ show assumps)
      rateVector = A.projRates cr or mRate cfDates
      (ppyRates,defRates,recoveryRate,recoveryLag) = buildAssumptionPpyDefRecRate 
                                                           (lastPayDate:cfDates) 
                                                           (A.LoanAssump defaultAssump prepayAssump recoveryAssump ams)
      scheduleBal = mulBR ob ((sum (lastN rt $ getTsVals ts)) / sum (getTsVals ts))
                 
      txns = projectLoanFlow [] (divideBB cb scheduleBal) cb lastPayDate cfDates defRates ppyRates 
                                (replicate cfDatesLength 0.0) (replicate cfDatesLength 0.0) rateVector (recoveryLag,recoveryRate) 
                                p sr  -- `debug` ("rate"++show rate_vector)


  -- ^ Project cashflow for loans with prepayment/default/loss and interest rate assumptions
  projCashflow pl@(PersonalLoan (LoanOriginalInfo ob or ot p sd prinPayType) cb cr rt Current) 
               asOfDay 
               (A.LoanAssump defaultAssump prepayAssump recoveryAssump ams,_,_)
               mRate 
    = let 
        (futureTxns,historyM) = CF.cutoffTrs asOfDay txns 
      in 
        (applyHaircut ams (CF.CashFlowFrame futureTxns)
        ,historyM)
    where
      last_pay_date:cf_dates = lastN (rt + recovery_lag + 1) $ sd:getPaymentDates pl recovery_lag
      cf_dates_length = length cf_dates  --  `debug` ("incoming assumption "++ show assumps)
      rate_vector = A.projRates cr or mRate cf_dates
      (ppy_rates,def_rates,recovery_rate,recovery_lag) = buildAssumptionPpyDefRecRate (last_pay_date:cf_dates) (A.LoanAssump defaultAssump prepayAssump recoveryAssump ams)
      txns = projectLoanFlow [] 1.0 cb last_pay_date cf_dates def_rates ppy_rates (replicate cf_dates_length 0.0) (replicate cf_dates_length 0.0) rate_vector (recovery_lag,recovery_rate) p prinPayType  -- `debug` ("rate"++show rate_vector)

  -- ^ Project cashflow for defautled loans 
  projCashflow m@(PersonalLoan (LoanOriginalInfo ob or ot p sd prinPayType) cb cr rt (Defaulted (Just defaultedDate))) 
               asOfDay 
               (_,_,(A.DefaultedRecovery rr lag timing))
               _
    = let 
        (cf_dates1,cf_dates2) = splitAt (pred lag) $ genDates defaultedDate p (lag+ length timing)
        beforeRecoveryTxn = [  CF.LoanFlow d cb 0 0 0 0 0 0 cr Nothing| d <- cf_dates1 ]
        recoveries = calcRecoveriesFromDefault cb rr timing
        _txns = [  CF.LoanFlow d 0 0 0 0 0 r 0 cr Nothing | (d,r) <- zip cf_dates2 recoveries ]
        (_, txns) = splitByDate (beforeRecoveryTxn++_txns) asOfDay EqToRight -- `debug` ("AS OF Date"++show asOfDay)
        (futureTxns,historyM) = CF.cutoffTrs asOfDay txns 
      in 
        (CF.CashFlowFrame futureTxns, historyM)

  projCashflow m@(PersonalLoan (LoanOriginalInfo ob or ot p sd prinPayType) cb cr rt (Defaulted Nothing)) asOfDay assumps _
    = (CF.CashFlowFrame [CF.LoanFlow asOfDay 0 0 0 0 0 0 0 cr Nothing],Map.empty)
  
  splitWith l@(PersonalLoan (LoanOriginalInfo ob or ot p sd prinPayType) cb cr rt st) rs
    = [ PersonalLoan (LoanOriginalInfo (mulBR ob ratio) or ot p sd prinPayType) (mulBR cb ratio) cr rt st | ratio <- rs ]
