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

data Installment = Installment OriginalInfo Balance RemainTerms Status
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


projectInstallmentFlow :: [CF.TsRow] -> (Balance,Balance,Balance) -> Balance -> Date -> [Date] -> [Balance] -> [DefaultRate] -> [PrepaymentRate] -> [Amount] -> [Amount] -> (Int,Rate) -> Period -> [CF.TsRow]
projectInstallmentFlow trs (ob,opmt,ofee) cb last_pay_date (pdate:pdates) sbs (def_rate:def_rates) (ppy_rate:ppy_rates) rec_vec@(rec_amt:rec_amts) loss_vec@(loss_amt:loss_amts) (recovery_lag,recovery_rate) p
  | cb > 0.01 = projectInstallmentFlow
                  (trs++[tr])
                  (ob,opmt,ofee)
                  end_bal 
                  pdate 
                  pdates
                  (tail sbs)
                  def_rates 
                  ppy_rates 
                  (tail current_rec)
                  (tail current_loss)
                  (recovery_lag, recovery_rate)
                  p
                 where 
                  _remain_terms = 1 + max 0 ((length pdates) - recovery_lag)
                  _new_default = mulBR cb def_rate
                  _b_after_default = cb - _new_default -- `debug` ("D:"++show _new_default)
                  _new_ppy = mulBR _b_after_default ppy_rate
                  _b_after_ppy = _b_after_default - _new_ppy -- `debug` ("D:"++show _b_after_default)

                  sb = head sbs
                  _new_int = (_b_after_ppy / sb) * ofee
                  _prin = (_b_after_ppy / sb) * opmt   -- `debug` ("Schedule Balance"++show sbs)
                  
                  _new_rec = mulBR _new_default recovery_rate
                  _new_loss = mulBR _new_default (1 - recovery_rate)
                  current_rec = replace rec_vec recovery_lag _new_rec 
                  current_loss = replace loss_vec recovery_lag _new_loss 
                  end_bal = _b_after_ppy - _prin  -- `debug` ("D:"++show _b_after_ppy)
                  tr = CF.LoanFlow pdate end_bal _prin _new_int _new_ppy _new_default (head current_rec) (head current_loss) 0.0


projectInstallmentFlow trs oi cb last_pay_date (pdate:pdates) _sbs (def_rate:def_rates) (ppy_rate:ppy_rates) rec_vec@(rec_amt:rec_amts) loss_vec@(loss_amt:loss_amts) (recovery_lag,recovery_rate) p
   = projectInstallmentFlow 
                  (trs++[tr])
                  oi
                  cb
                  pdate
                  pdates 
                  _sbs
                  []
                  []
                  rec_amts
                  loss_amts
                  (recovery_lag,recovery_rate)
                  p
                 where 
                   tr = CF.LoanFlow pdate cb 0 0 0 0 rec_amt loss_amt 0.0

projectInstallmentFlow trs _ _ _ [] _ _ _ _ _ _ _ = trs 


instance Asset Installment where
  calcCashflow inst@(Installment (LoanOriginalInfo ob or ot p sd ptype) cb rt st) asOfDay
    = CF.CashFlowFrame $ zipWith9
                          CF.LoanFlow
                            (tail cf_dates)
                            b_flow
                            prin_flow
                            int_flow
                            (replicate l 0.0)
                            (replicate l 0.0)
                            (replicate l 0.0)
                            (replicate l 0.0)
                            (replicate l orate)
      where 
        orate = getOriginRate inst
        cf_dates = take (succ ot) $ sliceDates (SliceAfterKeepPrevious asOfDay) $ getPaymentDates inst 0
        l = pred (length cf_dates)
        pmt = divideBI ob ot
        schedule_bal = fromRational $ (toRational ob) * (toRational (rt % ot))
        (b_flow,prin_flow,int_flow) = case ptype of 
                                        F_P -> calc_p_i_flow_f_p ob cb schedule_bal pmt cf_dates p orate

  getCurrentBal (Installment _ b _ _) = b
  
  getOriginBal (Installment (LoanOriginalInfo ob _ _ _ _ _) _ _ _) = ob

  getOriginRate (Installment (LoanOriginalInfo _ or _ _ _ _) _ _ _) 
    = case or of
       Fix _r -> _r
       Floater _ _ _r _ Nothing -> _r
       Floater _ _ _r _ (Just floor) -> max _r floor

  isDefaulted (Installment _ _ _ (Defaulted _)) = True
  isDefaulted (Installment _ _ _ _) = False

  getPaymentDates (Installment (LoanOriginalInfo _ _ ot p sd _) _ _ _) extra 
    = genDates sd p (ot+extra)

  projCashflow inst@(Installment (LoanOriginalInfo ob or ot p _ _) cb rt _) asOfDay assumps
    = CF.CashFlowFrame $ projectInstallmentFlow 
                           []
                           (ob,pmt,fee_per_period)
                           cb 
                           last_pay_date
                           cf_dates
                           schedule_balances
                           def_rates
                           ppy_rates
                           (replicate cf_dates_length 0.0)
                           (replicate cf_dates_length 0.0)
                           (recovery_lag,recovery_rate)
                           p
      where 
          (_,_cf_dates) = splitAt (pred (ot-rt)) $ getPaymentDates inst recovery_lag
          last_pay_date:cf_dates = _cf_dates
          pmt = divideBI ob ot
          fee_per_period = mulBIR ob (getOriginRate inst)
          cf_dates_length = length cf_dates
          (_,schedule_balances) = splitAt (ot - rt) $ scanl (-) ob (replicate ot pmt)
          (ppy_rates,def_rates,recovery_rate,recovery_lag) = buildAssumptionRate (last_pay_date:cf_dates) assumps
                                                              (replicate cf_dates_length 0.0)
                                                              (replicate cf_dates_length 0.0) 
                                                              0
                                                              0


$(deriveJSON defaultOptions ''Installment)
