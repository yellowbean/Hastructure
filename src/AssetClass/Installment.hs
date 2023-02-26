{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module AssetClass.Installment 
  (Installment(..))
  where

import qualified Data.Time as T
import Data.Ratio

import Data.Aeson hiding (json)
import Language.Haskell.TH
import Data.Maybe
import Data.Aeson.TH
import Data.Aeson.Types

import Asset
import Types 
import Lib
import Util
import qualified Cashflow as CF

import Debug.Trace
debug = flip trace

data Installment = Installment OriginalInfo Balance Status
                 | Dummy
     deriving (Show)

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


projectInstallmentFlow :: [CF.TsRow] -> (Balance,Balance) -> Balance -> Date -> [Date] -> [Balance] -> [DefaultRate] -> [PrepaymentRate] -> [Amount] -> [Amount] -> (Int,Rate) -> Period -> [CF.TsRow]
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
                            (_b_after_ppy / sb) * opmt
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
  calcCashflow inst@(Installment (LoanOriginalInfo ob or ot p sd ptype) cb st) asOfDay
    = CF.CashFlowFrame $ zipWith9
                          CF.LoanFlow
                            cf_dates
                            stressed_bal_flow
                            prin_flow
                            int_flow
                            (replicate rt 0.0)
                            (replicate rt 0.0)
                            (replicate rt 0.0)
                            (replicate rt 0.0)
                            (replicate rt orate) 
      where 
        cf_dates = filter (> asOfDay) $ sd:getPaymentDates inst 0 
        cf_dates_length = length cf_dates 
        rt = length cf_dates
        opmt = divideBI ob ot  
        schedule_balances = scanl (-) ob (replicate ot opmt) -- `debug` (show ot++">>"++show rt)
        current_schedule_bal =  schedule_balances !! (ot - rt)   
        
        ofee = mulBIR ob (getOriginRate inst)

        factor =  cb / current_schedule_bal 
        cpmt = opmt * factor -- `debug` ("Current B"++show cb++">> schedule bal"++ show current_schedule_bal)
        cfee = ofee * factor 
        
        orate = getOriginRate inst

        stressed_bal_flow = map (* factor)  $ lastN rt schedule_balances
        prin_flow = replicate rt cpmt 
        int_flow =  replicate rt cfee

  getCurrentBal (Installment _ b _ ) = b
  
  getOriginBal (Installment (LoanOriginalInfo ob _ _ _ _ _) _ _) = ob

  getOriginRate (Installment (LoanOriginalInfo _ or _ _ _ _) _ _) 
    = case or of
       Fix _r -> _r
       Floater _ _ _r _ Nothing -> _r
       Floater _ _ _r _ (Just floor) -> max _r floor

  isDefaulted (Installment _ _ (Defaulted _)) = True
  isDefaulted (Installment _ _ _) = False

  getPaymentDates (Installment (LoanOriginalInfo _ _ ot p sd _) _ _) extra 
    = genDates sd p (ot+extra)

  projCashflow inst@(Installment (LoanOriginalInfo ob or ot p sd _) _ Current) asOfDay assumps
    = CF.CashFlowFrame $ projectInstallmentFlow 
                           []
                           (opmt,ofee)
                           current_schedule_bal
                           last_pay_date
                           cf_dates
                           schedule_balances
                           (paddingDefault 0.0 def_rates cf_dates_length)
                           (paddingDefault 0.0 ppy_rates cf_dates_length)
                           (replicate cf_dates_length 0.0)
                           (replicate cf_dates_length 0.0)
                           (recovery_lag,recovery_rate)
                           p 
      where 

          last_pay_date:cf_dates = sliceDates (SliceAfterKeepPrevious asOfDay) $ sd:getPaymentDates inst recovery_lag
          cf_dates_length = length cf_dates  -- `debug` ("Dates->>"++show cf_dates)
          rt = length cf_dates - recovery_lag -- `debug` ("CF Length"++ show (length cf_dates))
          opmt = divideBI ob ot
          schedule_balances = scanl (-) ob (replicate ot opmt)
          current_schedule_bal = schedule_balances !! (ot - rt) -- `debug` ("RT->"++show rt)
          ofee = mulBIR ob (getOriginRate inst)
          orate = getOriginRate inst


          (ppy_rates,def_rates,recovery_rate,recovery_lag) = buildAssumptionRate (last_pay_date:cf_dates) assumps
                                                              (replicate cf_dates_length 0.0)
                                                              (replicate cf_dates_length 0.0) 
                                                              0
                                                              0

  projCashflow inst@(Installment _ _ (Defaulted _)) asOfDay assumps
    = CF.CashFlowFrame $ []  -- TODO defaulted asset may have recoveries

$(deriveJSON defaultOptions ''Installment)
