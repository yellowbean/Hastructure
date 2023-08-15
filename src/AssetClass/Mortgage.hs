{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module AssetClass.Mortgage
  (projectMortgageFlow,projectScheduleFlow,updateOriginDate)
  where

import qualified Data.Time as T
import qualified Cashflow as CF 
import qualified Assumptions as A
import Asset as Ast
import Types
import Lib
import Util
import InterestRate as IR

import qualified Data.Map as Map
import Data.List
import Data.Ratio
import Data.Maybe
import GHC.Generics
import Data.Aeson hiding (json)
import Language.Haskell.TH
import Data.Aeson.TH
import Data.Aeson.Types

import AssetClass.AssetBase

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
                    pt -- `debug` ("remain terms"++ show _remain_terms++">> new_bal_after_ppy"++ show _new_bal_after_ppy)
                  where
                    _remain_terms = 1 + max 0 (length _pdates - recovery_lag) -- `debug` ("IN mortgage flow"++ show _remain_terms)
                    _new_default = mulBR _bal _def_rate
                    _new_bal_after_default = _bal - _new_default
                    _new_prepay = mulBR _new_bal_after_default _ppy_rate
                    _new_bal_after_ppy = _new_bal_after_default - _new_prepay
                    _new_int = mulBI _new_bal_after_ppy (periodRateFromAnnualRate p _rate)  -- `debug` ("Balance"++show(_new_bal_after_ppy))
                    _pmt = calcPmt _new_bal_after_ppy (periodRateFromAnnualRate p _rate) _remain_terms -- `debug` ("pmt->bal"++show _new_bal_after_ppy++"rate"++show _rate++"term"++show _remain_terms)
                    _new_prin = case pt of
                                    Level -> _pmt - _new_int -- `debug` ("PMT->"++ show _pmt)
                                    Even ->  _new_bal_after_ppy / fromIntegral _remain_terms -- `debug` ("Dividing _remain"++show _remain_terms ) --(ob / (fromIntegral ot)) * (_new_bal_after_ppy / ob)

                    _new_rec = mulBR _new_default recovery_rate
                    _new_loss = mulBR _new_default (1 - recovery_rate)

                    _current_rec = replace _rec_vector recovery_lag _new_rec
                    _current_loss = replace _loss_vector recovery_lag _new_loss

                    _end_bal = _new_bal_after_ppy - _new_prin
                    _survive_rate = ((1 - _def_rate) * (1 - _ppy_rate))  
                    _temp = _survive_rate * (toRational (1 - _new_prin / _new_bal_after_ppy))
                    _new_mbn = (\y -> fromInteger (round (_temp * (toRational y)))) <$> mbn
                    tr = CF.MortgageFlow _pdate _end_bal _new_prin _new_int _new_prepay _new_default (head _current_rec) (head _current_loss) _rate _new_mbn Nothing --TODO missing ppy-penalty here

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
    tr = CF.MortgageFlow _pdate _b 0 0 0 0 _rec_amt _loss_amt 0.0 Nothing Nothing

projectMortgageFlow trs _ _ _ [] _ _ [] [] _ _ _ _ _ = trs   -- `debug` ("Ending trs=>"++show(trs))

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

       _schedule_prin = mulBR (CF.mflowPrincipal flow) _survive_rate --TODO round trip  -- `debug` ("Schedule Principal"++(printf "%.2f" (CF.mflowPrincipal flow))++" Rate"++show(_schedule_rate))
       _schedule_int = mulBR (CF.mflowInterest flow) _survive_rate

       _new_rec = mulBR _def_amt recovery_rate
       _new_loss = mulBR _def_amt (1 - recovery_rate)

       _rec_vector = replace _rec recovery_lag _new_rec
       _loss_vector = replace _loss recovery_lag _new_loss

       _end_bal = max 0 $ _after_bal - _schedule_prin

       tr = CF.MortgageFlow (CF.getDate flow) _end_bal _schedule_prin _schedule_int _ppy_amt _def_amt (head _rec_vector) (head _loss_vector) 0.0 Nothing Nothing --TODO missing ppy-penalty here

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
             Nothing

projectScheduleFlow trs _ last_bal [] _ _ [] [] (_,_) = trs -- `debug` ("===>C") --  `debug` ("End at "++show(trs))


instance Ast.Asset Mortgage where
  calcCashflow m@(Mortgage (MortgageOriginalInfo ob or ot p sd ptype)  _bal _rate _term _mbn _) d =
      let 
        (_,futureTxns) = splitByDate txns d EqToRight  -- 
      in 
        CF.CashFlowFrame futureTxns  -- `debug` ("Future txn"++show futureTxns)
    where
      orate = getOriginRate m
      pmt = calcPmt _bal (periodRateFromAnnualRate p _rate) _term
      last_pay_date:cf_dates = lastN (1 + _term) $ sd:getPaymentDates m 0 -- `debug` ("CF dates--->"++ show (sd:getPaymentDates m 0)++">> laastN"++show (1+_term))
      l = length cf_dates
      rate_used = case or of
                    IR.Fix _r -> replicate l _r
                    IR.Floater _ _ _ _ _ -> replicate l _rate

      (b_flow,prin_flow,int_flow) = case ptype of
                                     Level -> calc_p_i_flow _bal pmt (last_pay_date:cf_dates) _rate
                                     Even ->  calc_p_i_flow_even (_bal / fromIntegral _term) _bal (last_pay_date:cf_dates) _rate

      bnflow = [ (\y -> (fromInteger (round ((toRational y) * (toRational (b / _bal)))))) <$> _mbn  | b <- b_flow ] -- borrower number
      txns = zipWith10 CF.MortgageFlow cf_dates b_flow prin_flow int_flow (replicate l 0.0) (replicate l 0.0) (replicate l 0.0) (replicate l 0.0) rate_used bnflow (replicate l Nothing)

  calcCashflow s@(ScheduleMortgageFlow beg_date flows)  d = CF.CashFlowFrame flows
  calcCashflow m@(AdjustRateMortgage _origin _arm  _bal _rate _term _mbn _status) d = projCashflow m d [] 
  
  getCurrentBal (Mortgage _ _bal _ _ _ _) = _bal
  getCurrentBal (AdjustRateMortgage _ _ _bal _ _ _ _) = _bal

  getOriginBal (Mortgage (MortgageOriginalInfo _bal _ _ _ _ _ ) _ _ _ _ _ ) = _bal
  getOriginBal (AdjustRateMortgage (MortgageOriginalInfo _bal _ _ _ _ _ ) _ _ _ _ _ ) = _bal

  getOriginRate (Mortgage (MortgageOriginalInfo _ or _ _ _ _ ) _ _ _ _ _ )
    = case or of
       IR.Fix _r -> _r
       IR.Floater _ _ _r _ Nothing -> _r
       IR.Floater _ _ _r _ (Just floor) -> max _r floor
  getOriginRate (AdjustRateMortgage (MortgageOriginalInfo _ or _ _ _ _ ) _ _ _ _ _ _ )
    = case or of
       IR.Fix _r -> _r
       IR.Floater _ _ _r _ Nothing -> _r
       IR.Floater2 _ _ _r _ _ _ _ -> _r 

  getPaymentDates (Mortgage (MortgageOriginalInfo _ _ ot p sd _) _ _ ct _ _) extra
    = genDates sd p (ot+extra)
  
  getPaymentDates (AdjustRateMortgage (MortgageOriginalInfo _ _ ot p sd _) _ _ _ ct _ _) extra
    = genDates sd p (ot+extra)

  isDefaulted (Mortgage _ _ _ _ _ (Defaulted _)) = True
  isDefaulted (AdjustRateMortgage _ _ _ _ _ _ (Defaulted _)) = True
  isDefaulted (Mortgage _ _ _ _ _ _) = False
  isDefaulted (AdjustRateMortgage _ _ _ _ _ _ _) = False
  
  getOriginDate (Mortgage (MortgageOriginalInfo _ _ ot p sd _) _ _ ct _ _) = sd
  getOriginDate (AdjustRateMortgage (MortgageOriginalInfo _ _ ot p sd _) _ _ _ ct _ _) = sd

  getRemainTerms (Mortgage (MortgageOriginalInfo _ _ ot p sd _) _ _ ct _ _) = ct
  getRemainTerms (AdjustRateMortgage (MortgageOriginalInfo _ _ ot p sd _) _ _ _ ct _ _) = ct

  updateOriginDate (Mortgage (MortgageOriginalInfo ob or ot p sd _type) cb cr ct mbn st) nd 
    = (Mortgage (MortgageOriginalInfo ob or ot p nd _type) cb cr ct mbn st) 
  updateOriginDate (AdjustRateMortgage (MortgageOriginalInfo ob or ot p sd _type) arm cb cr ct mbn st) nd 
    = (AdjustRateMortgage (MortgageOriginalInfo ob or ot p nd _type) arm cb cr ct mbn st)
  
  projCashflow m@(Mortgage (MortgageOriginalInfo ob or ot p sd prinPayType) cb cr rt mbn Current) asOfDay assumps =
    let 
      (_,futureTxns) = splitByDate txns asOfDay EqToRight
    in 
      CF.CashFlowFrame futureTxns
    where
      last_pay_date:cf_dates = lastN (recovery_lag + rt + 1) $ sd:(getPaymentDates m recovery_lag)  
      cf_dates_length = length cf_dates  -- `debug` ("Last Pay Date\n"++ show last_pay_date++"SD\n"++ show sd++"ot,ct\n"++show ot++","++show rt)
      rate_vector = case or of
                      IR.Fix r ->  replicate cf_dates_length r
                      IR.Floater idx sprd _orate p mfloor ->
                              case A.getRateAssumption assumps idx of
                                Just (A.InterestRateCurve idx ps) ->  map (\x -> sprd + (fromRational x)) $ getValByDates ps Exc cf_dates
                                Just (A.InterestRateConstant idx v) ->  map (\x -> sprd + x) $ replicate cf_dates_length v
                                Nothing -> replicate cf_dates_length 0.0

      (ppy_rates,def_rates,recovery_rate,recovery_lag) = buildAssumptionRate (last_pay_date:cf_dates) assumps
                               (replicate cf_dates_length 0.0)
                               (replicate cf_dates_length 0.0)
                               0
                               0
      txns = projectMortgageFlow [] cb (toRational <$> mbn) last_pay_date cf_dates def_rates ppy_rates (replicate cf_dates_length 0.0) (replicate cf_dates_length 0.0) rate_vector (recovery_lag,recovery_rate) p prinPayType 

  projCashflow m@(Mortgage (MortgageOriginalInfo ob or ot p sd prinPayType) cb cr rt mbn (Defaulted (Just defaultedDate)) ) asOfDay assumps
    = case find f assumps of 
        Nothing -> CF.CashFlowFrame $ [CF.MortgageFlow asOfDay cb 0 0 0 0 0 0 cr mbn Nothing]
        Just (A.DefaultedRecovery rr lag timing) -> 
          let 
            (cf_dates1,cf_dates2) = splitAt lag $ genDates defaultedDate p (lag+ length timing)
            beforeRecoveryTxn = [  CF.MortgageFlow d cb 0 0 0 0 0 0 cr mbn Nothing | d <- cf_dates1 ]
            recoveries = calcRecoveriesFromDefault cb rr timing
            bals = scanl (-) cb recoveries
            _txns = [  CF.MortgageFlow d b 0 0 0 0 r 0 cr mbn Nothing | (b,d,r) <- zip3 bals cf_dates2 recoveries ]
            (_, txns) = splitByDate (beforeRecoveryTxn++_txns) asOfDay EqToRight -- `debug` ("AS OF Date"++show asOfDay)
          in 
            CF.CashFlowFrame txns
       where 
           f x = case x of 
                   A.DefaultedRecovery _ _ _ ->True 
                   _ -> False 

  projCashflow m@(AdjustRateMortgage mo arm cb cr rt mbn (Defaulted (Just defaultedDate)) ) asOfDay assumps
    = projCashflow (Mortgage mo cb cr rt mbn  (Defaulted (Just defaultedDate))) asOfDay assumps
      
  projCashflow m@(Mortgage _ cb cr rt mbn (Defaulted Nothing) ) asOfDay assumps
    = CF.CashFlowFrame $ [ CF.MortgageFlow asOfDay cb 0 0 0 0 0 0 cr mbn Nothing ]
  
  projCashflow m@(AdjustRateMortgage _ _ cb cr rt mbn (Defaulted Nothing) ) asOfDay assumps
    = CF.CashFlowFrame $ [ CF.MortgageFlow asOfDay cb 0 0 0 0 0 0 cr mbn Nothing ]
      

  projCashflow m@(AdjustRateMortgage (MortgageOriginalInfo ob or ot p sd prinPayType) arm cb cr rt mbn Current) asOfDay assumps =
    let 
      (_,futureTxns) = splitByDate txns asOfDay EqToRight
    in 
      CF.CashFlowFrame futureTxns
    where
      ARM initPeriod initCap periodicCap lifeCap lifeFloor = arm
      passInitPeriod = (ot - rt) >= initPeriod 
      firstResetDate = monthsAfter sd (toInteger (succ initPeriod))

      last_pay_date:cf_dates = sliceDates (SliceOnAfterKeepPrevious asOfDay)  $ lastN (rt + recovery_lag + 1) $ sd:(getPaymentDates m recovery_lag) 
      
      cf_dates_length = length cf_dates -- `debug` (" cf dates >>" ++ show (last_pay_date:cf_dates ))
      rate_curve = case or of
                      IR.Fix r ->  error "ARM should have floater rate"
                      IR.Floater2 idx sprd initRate dp _ _ mRoundBy ->
                        let 
                          resetDates = genSerialDatesTill2 IE firstResetDate dp (last cf_dates)
                          projectFutureActualCurve = runInterestRate2 arm (sd,getOriginRate m) or resetDates
                        in 
                          case A.getRateAssumption assumps idx of
                            Just (A.InterestRateCurve idx curve) 
                              -> projectFutureActualCurve curve -- `debug` ("Curve")
                            Just (A.InterestRateConstant idx v) 
                              -> projectFutureActualCurve (mkRateTs [(getOriginDate m,v),(last cf_dates,v)]) -- `debug` ("lpd"++show last_pay_date++"lpd"++ show (last cf_dates))
                            Nothing -> error $ "Failed to find index"++ show idx

      rate_vector = fromRational <$> getValByDates rate_curve Inc cf_dates -- `debug` ("RateCurve"++ show rate_curve)

      (ppy_rates,def_rates,recovery_rate,recovery_lag) = buildAssumptionRate (last_pay_date:cf_dates) assumps
                               (replicate cf_dates_length 0.0)
                               (replicate cf_dates_length 0.0)
                               0
                               0
      txns = projectMortgageFlow [] cb (toRational <$> mbn) last_pay_date cf_dates def_rates ppy_rates (replicate cf_dates_length 0.0) (replicate cf_dates_length 0.0) rate_vector (recovery_lag,recovery_rate) p prinPayType 

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
                             (recovery_lag,recovery_rate) -- `debug` ("ppy rate"++ show ppy_rates)
       where
        beg_bal =  CF.mflowBegBalance $ head flows -- `debug` ("beg date"++show beg_date)
        (ppy_rates,def_rates,recovery_rate,recovery_lag) = buildAssumptionRate (beg_date:cf_dates) assumps [] [] 0 0 -- `debug` ("Assumpt"++ show assumps)
        curve_dates_length =  recovery_lag + length flows
        temp_p = Lib.Monthly -- TODO to fix this hard code
        cf_dates = (map CF.getDate flows) ++ (genDates (CF.getDate (last flows)) temp_p recovery_lag)

  getBorrowerNum m@(Mortgage (MortgageOriginalInfo ob or ot p sd prinPayType) cb cr rt mbn _ ) = fromMaybe 1 mbn
  getBorrowerNum m@(AdjustRateMortgage (MortgageOriginalInfo ob or ot p sd prinPayType) _ cb cr rt mbn _ ) = fromMaybe 1 mbn

  splitWith (Mortgage (MortgageOriginalInfo ob or ot p sd prinPayType) cb cr rt mbn st ) rs 
    = [ Mortgage (MortgageOriginalInfo (mulBR ob ratio) or ot p sd prinPayType) (mulBR cb ratio) cr rt mbn st 
       | ratio <- rs ]
  
  splitWith (AdjustRateMortgage (MortgageOriginalInfo ob or ot p sd prinPayType) arm cb cr rt mbn st ) rs 
    = [ AdjustRateMortgage (MortgageOriginalInfo (mulBR ob ratio) or ot p sd prinPayType) arm (mulBR cb ratio) cr rt mbn st 
       | ratio <- rs ]
  

