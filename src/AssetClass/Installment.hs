{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module AssetClass.Installment 
  (projectInstallmentFlow, updateOriginDate)
  where

import qualified Data.Time as T
import Data.Ratio

import Data.Aeson hiding (json)
import Language.Haskell.TH
import Data.Maybe
import Data.List
import Data.Aeson.TH
import Data.Aeson.Types
import GHC.Generics

import Asset
import InterestRate
import qualified Assumptions as A
import Types 
import Lib
import Util
import qualified Cashflow as CF

import AssetClass.AssetBase

import Debug.Trace
debug = flip trace

calc_p_i_flow_f_p :: Balance -> Balance -> Balance -> Amount -> Dates -> Period -> IRate -> ([Balance],CF.Principals,CF.Interests)
calc_p_i_flow_f_p ob cb sb amt ds p r 
  = (_bals, _prins,_fees)
    where 
      size = length ds
      factor = toRational $ cb / sb
      _prins = replicate size $ (mulBR amt factor)
      _period_fee =  mulBR (mulBI ob r) factor
      _bals = tail $ scanl (-) cb _prins
      _fees = replicate size _period_fee


projectInstallmentFlow :: [CF.TsRow] -> (Amount,Amount) -> Balance -> Date -> [Date] -> [Balance] -> [DefaultRate] -> [PrepaymentRate] -> [Amount] -> [Amount] -> (Int,Rate) -> Period -> [CF.TsRow]
projectInstallmentFlow trs _ _ _ [] _ _ _ _ _ _ _ = trs 
projectInstallmentFlow trs (opmt,ofee) cb last_pay_date (pdate:pdates) (sb:sbs) (def_rate:def_rates) (ppy_rate:ppy_rates) rec_vec@(rec_amt:rec_amts) loss_vec@(loss_amt:loss_amts) (recovery_lag,recovery_rate) p
  | _remain_terms >= 0 = projectInstallmentFlow
                  (trs++[tr])
                  (opmt,ofee)
                  end_bal 
                  pdate 
                  pdates
                  sbs
                  def_rates 
                  ppy_rates 
                  _recs
                  _losses
                  (recovery_lag, recovery_rate)
                  p  -- `debug` ("remain->"++show pdates++"remain-terms"++ show _remain_terms)
                 where 
                  _remain_terms = length pdates - recovery_lag
                  _new_default = mulBR cb def_rate
                  _b_after_default = cb - _new_default  -- `debug` ("D:"++show _new_default)
                  _new_ppy = mulBR _b_after_default ppy_rate
                  _b_after_ppy = _b_after_default - _new_ppy -- `debug` ("D:"++show _b_after_default)

                  _new_int = if _remain_terms >= 0 then 
                               (_b_after_ppy / sb) * ofee
                             else
                               0 
                  _prin = if _remain_terms >= 0 then  
                            (_b_after_ppy / sb) * opmt  -- `debug` ("perf balance"++show _b_after_ppy++"schedule balance"++show sb)
                          else 
                            0--   `debug` ("Schedule Balance"++show sb++"_b_f"++show _b_after_ppy++"PMT"++show opmt)
                  _new_rec = mulBR _new_default recovery_rate
                  _new_loss = mulBR _new_default (1 - recovery_rate)
                  _rec:_recs = replace rec_vec recovery_lag _new_rec 
                  _loss:_losses = replace loss_vec recovery_lag _new_loss 
                  end_bal = _b_after_ppy - _prin  -- `debug` ("D:"++show _b_after_ppy)
                  tr = CF.LoanFlow pdate end_bal _prin _new_int _new_ppy _new_default _rec _loss 0.0


projectInstallmentFlow trs oi cb last_pay_date (pdate:pdates) _sbs defVec  ppyVec  (rec_amt:rec_amts) (loss_amt:loss_amts) recVec p
   = projectInstallmentFlow 
                  (trs++[tr])
                  oi
                  cb
                  pdate
                  pdates 
                  _sbs
                  defVec
                  ppyVec
                  rec_amts
                  loss_amts
                  recVec
                  p
                 where 
                   tr = CF.LoanFlow pdate cb 0 0 0 0 rec_amt loss_amt 0.0


instance Asset Installment where
  calcCashflow inst@(Installment (LoanOriginalInfo ob or ot p sd ptype) cb rt st) asOfDay _
    = CF.CashFlowFrame flows 
     where 
        -- cf_dates = filter (> asOfDay) $ sd:getPaymentDates inst 0 
        last_pay_date:cf_dates = lastN (rt+1) $ sd:getPaymentDates inst 0
        opmt = divideBI ob ot  
        schedule_balances = scanl (-) ob (replicate ot opmt) -- `debug` (show ot++">>"++show rt)
        -- current_schedule_bal =  schedule_balances !! (ot - rt)   
        current_schedule_bal =  schedule_balances !! (ot - rt)   
        
        ofee = mulBIR ob (getOriginRate inst)

        factor =  cb / current_schedule_bal 
        cpmt = opmt * factor -- `debug` ("Current B"++show cb++">> schedule bal"++ show current_schedule_bal)
        cfee = ofee * factor 
        
        orate = getOriginRate inst

        stressed_bal_flow = map (* factor)  $ lastN rt schedule_balances
        prin_flow = replicate rt cpmt 
        int_flow =  replicate rt cfee
        _flows = zipWith9 CF.LoanFlow cf_dates stressed_bal_flow prin_flow int_flow (replicate rt 0.0) (replicate rt 0.0) (replicate rt 0.0) (replicate rt 0.0) (replicate rt orate) 
        (_,flows) = splitByDate 
                      _flows
                      asOfDay
                      EqToRight -- `debug` ("5"++show bals++">>"++show pmts)


  getCurrentBal (Installment _ b _ _ ) = b
  
  getOriginBal (Installment (LoanOriginalInfo ob _ _ _ _ _) _ _ _) = ob

  getOriginRate (Installment (LoanOriginalInfo _ or _ _ _ _) _ _ _) 
    = case or of
       Fix _r -> _r
       Floater _ _ _r _ _ _ _ -> _r

  isDefaulted (Installment _ _ _ (Defaulted _)) = True
  isDefaulted (Installment {}) = False

  getPaymentDates (Installment (LoanOriginalInfo _ _ ot p sd _) _ _ _) extra 
    = genDates sd p (ot+extra)

  getOriginDate (Installment (LoanOriginalInfo _ _ ot p sd _) _ _ _) = sd
  
  getRemainTerms (Installment (LoanOriginalInfo _ _ ot p sd _) _ rt _) = rt

  updateOriginDate (Installment (LoanOriginalInfo ob or ot p sd _type) cb rt st) nd
    = Installment (LoanOriginalInfo ob or ot p nd _type) cb rt st

  projCashflow inst@(Installment (LoanOriginalInfo ob or ot p sd _) cb rt Current) 
               asOfDay 
               pAssump@(A.InstallmentAssump defaultAssump prepayAssump recoveryAssump extraStressAssump)
               mRates
    = CF.CashFlowFrame $ cutBy Inc Future asOfDay flows 
      where 
          -- last_pay_date:cf_dates = sliceDates (SliceAfterKeepPrevious asOfDay) $ sd:getPaymentDates inst recovery_lag
          last_pay_date:cf_dates = lastN (rt + recovery_lag +1) $ sd:getPaymentDates inst recovery_lag
          cf_dates_length = length cf_dates  -- `debug` ("Dates->>"++show cf_dates)
          rt_with_lag = rt - recovery_lag -- `debug` ("CF Length"++ show (length cf_dates))
          opmt = divideBI ob ot
          ofee = mulBIR ob (getOriginRate inst)
          orate = getOriginRate inst
          
          schedule_balances = scanl (-) ob (replicate ot opmt)
          current_schedule_bal = schedule_balances !! (ot - rt) -- `debug` ("RT->"++show rt)

          (ppy_rates,def_rates,recovery_rate,recovery_lag) = buildAssumptionPpyDefRecRate (last_pay_date:cf_dates) pAssump
          flows = projectInstallmentFlow 
                           []
                           (opmt,ofee)
                           cb
                           last_pay_date
                           cf_dates
                           (lastN (succ rt) schedule_balances)
                           (paddingDefault 0.0 def_rates cf_dates_length)
                           (paddingDefault 0.0 ppy_rates cf_dates_length)
                           (replicate cf_dates_length 0.0)
                           (replicate cf_dates_length 0.0)
                           (recovery_lag,recovery_rate)
                           p 


  projCashflow inst@(Installment (LoanOriginalInfo ob or ot p sd ptype) cb rt (Defaulted (Just defaultedDate))) 
               asOfDay 
               (A.DefaultedRecovery rr lag timing)
               mRates
    = let 
         (cf_dates1,cf_dates2) = splitAt lag $ genDates defaultedDate p (lag+length timing)
         beforeRecoveryTxn = [  CF.LoanFlow d cb 0 0 0 0 0 0 cr | d <- cf_dates1 ]
         recoveries = calcRecoveriesFromDefault cb rr timing
         bals = scanl (-) cb recoveries
         _txns = [  CF.LoanFlow d b 0 0 0 0 r 0 cr | (b,d,r) <- zip3 bals cf_dates2 recoveries ]
       in 
         CF.CashFlowFrame $ cutBy Inc Future asOfDay (beforeRecoveryTxn++_txns)
      where 
        cr = getOriginRate inst
  
  projCashflow inst@(Installment _ cb rt (Defaulted Nothing)) asOfDay assumps _
    = CF.CashFlowFrame $ [CF.LoanFlow asOfDay cb 0 0 0 0 0 0 (getOriginRate inst)]
        
  splitWith (Installment (LoanOriginalInfo ob or ot p sd _type) cb rt st) rs
    = [ Installment (LoanOriginalInfo (mulBR ob ratio) or ot p sd _type) (mulBR cb ratio) rt st | ratio <- rs ]


