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
import Assumptions (AssetPerfAssumption(MortgageAssump))
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
                    (tail _current_rec) 
                    (tail _current_loss) 
                    _rates
                    (recovery_lag,recovery_rate)
                    p
                    pt 
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
                    tr = CF.MortgageFlow _pdate _end_bal _new_prin _new_int _new_prepay 0 _new_default (head _current_rec) (head _current_loss) _rate _new_mbn Nothing --TODO missing ppy-penalty here

projectMortgageFlow trs _b mbn _last_date (_pdate:_pdates) _  _ (_rec_amt:_rec_amts) (_loss_amt:_loss_amts) _ _lag_rate _p _pt
 = projectMortgageFlow (trs++[tr]) _b mbn _pdate _pdates [] [] _rec_amts _loss_amts [0.0] _lag_rate _p _pt
  where
    tr = CF.MortgageFlow _pdate _b 0 0 0 0 0 _rec_amt _loss_amt 0.0 Nothing Nothing
projectMortgageFlow trs _ _ _ [] _ _ [] [] _ _ _ _ = trs  


projectDelinqMortgageFlow :: [CF.TsRow] -> Balance -> Maybe Rational -> Date -> [Date] -> [Rate] -> [PrepaymentRate] -> [IRate] -> (Rate,Lag,Rate,Lag,Period,AmortPlan) -> ([Balance],[Balance],[Balance]) -> [CF.TsRow]
projectDelinqMortgageFlow trs _ _ _ [] _ _ _ _ _ = sort trs --TODO to be sorted , merged need to recalc balance
projectDelinqMortgageFlow trs beginBal mBorrowerNum lastDate (pDate:pDates) (delinqRate:delinqRates) (ppyRate:ppyRates) (rate:rates) (defaultPct,defaultLag,recoveryRate,recoveryLag,p,prinType) (dBal:defaultVec,rAmt:recoveryVec,lAmt:lossVec)
  = projectDelinqMortgageFlow (trs++[tr]++backToPerfCfs) endingBal mNewBn pDate pDates delinqRates ppyRates rates (defaultPct,defaultLag,recoveryRate,recoveryLag,p,prinType) (newDefaultVec,newRecoveryVec,newLossVec) 
  where 
    remainTerms = 1 + max 0 (length pDates - recoveryLag) 
    delinqBal = mulBR beginBal delinqRate
    
    defaultBal = mulBR delinqBal defaultPct 
    backToPerf = mulBR delinqBal (1 - defaultPct)
    
    restPerfVector = replicate (length delinqRates) 0
    restPerfBal = fromRational <$> restPerfVector
    backToPerfCfs = projectDelinqMortgageFlow [] backToPerf Nothing pDate (drop defaultLag pDates) 
                                              restPerfVector restPerfVector (rate:(drop defaultLag rates))
                                              (0,0,0,0,p,prinType) 
                                              (restPerfBal,restPerfBal,restPerfBal)

    newDefaultVec = replace defaultVec defaultLag defaultBal
    
    recBal = mulBR dBal recoveryRate
    lossBal = mulBR dBal (1 - recoveryRate)
    newRecoveryVec = replace recoveryVec recoveryLag recBal
    newLossVec = replace lossVec recoveryLag lossBal
    
    balAfterDelinq = beginBal - delinqBal
    ppyAmt = mulBR balAfterDelinq ppyRate 
    balAfterPpy  = balAfterDelinq - ppyAmt
    periodRate = periodRateFromAnnualRate p rate
    intAmt = mulBI balAfterPpy periodRate 
    pmt = calcPmt balAfterPpy periodRate remainTerms
    prinAmt = case prinType of
                Level -> pmt - intAmt
                Even ->  balAfterPpy / fromIntegral remainTerms

    endingBal = beginBal - prinAmt - ppyAmt - delinqBal 
    
    
    mNewBn = Nothing --TODO
    tr = CF.MortgageFlow pDate endingBal prinAmt intAmt ppyAmt delinqBal dBal rAmt lAmt rate mNewBn Nothing 


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

       tr = CF.MortgageFlow (CF.getDate flow) _end_bal _schedule_prin _schedule_int _ppy_amt 0 _def_amt (head _rec_vector) (head _loss_vector) 0.0 Nothing Nothing --TODO missing ppy-penalty here

projectScheduleFlow trs b_factor last_bal [] _ _ (r:rs) (l:ls) (recovery_lag,recovery_rate)
  = projectScheduleFlow (trs++[tr]) b_factor last_bal [] [] [] rs ls (recovery_lag - 1,recovery_rate) 
   where
      remain_length = length rs
      last_date = CF.getDate (last trs)
      flow_date = nextDate last_date Lib.Monthly
      tr = CF.MortgageFlow flow_date last_bal 0 0 0 0 0 r l 0.0 Nothing Nothing

projectScheduleFlow trs _ last_bal [] _ _ [] [] (_,_) = trs 

-- | implementation on prepayment penalty, which patch cashflow to cashflow frame
patchPrepayPentalyFlow :: Mortgage -> CF.CashFlowFrame -> CF.CashFlowFrame
patchPrepayPentalyFlow m mflow@(CF.CashFlowFrame trs) 
  = let 
      (MortgageOriginalInfo ob or ot p sd pt mPpyPen) =  getOriginInfo m 
      (startDate,endDate) = CF.getDateRangeCashFlowFrame mflow
      prepaymentFlow = CF.mflowPrepayment <$> trs
      flowSize = CF.sizeCashFlowFrame mflow
    in 
      case mPpyPen of 
        Nothing -> mflow
        Just (ByTerm cutoff rate0 rate1) -> 
          let 
            rs = lastN flowSize $ (replicate cutoff rate0) ++ replicate (ot-cutoff) rate1
          in 
            CF.CashFlowFrame $ CF.setPrepaymentPenaltyFlow (zipWith mulBR prepaymentFlow rs) trs
        Just (FixAmount amt mCutoff) -> 
          let 
            projFlow = case mCutoff of 
                         Nothing -> replicate flowSize amt
                         Just cutoff -> lastN flowSize $ replicate cutoff amt ++ (replicate (ot-cutoff) 0 ) 
            actFlow = [ if ppy > 0 then 
                          f
                        else
                          0
                        | (f,ppy) <- zip projFlow prepaymentFlow]
          in 
            CF.CashFlowFrame $ CF.setPrepaymentPenaltyFlow actFlow trs
        Just (FixPct r mCutoff) ->
          let 
            rs = case mCutoff of 
                   Nothing -> replicate flowSize r
                   Just cutoff -> lastN flowSize $ replicate cutoff r ++ (replicate (ot-cutoff) 0)
          in
            CF.CashFlowFrame $ CF.setPrepaymentPenaltyFlow (zipWith mulBR prepaymentFlow rs) trs
        Just (Sliding sr changeRate) -> 
          let 
            rs = lastN flowSize $ paddingDefault 0 [sr,(sr-changeRate)..0] ot
          in
            CF.CashFlowFrame $ CF.setPrepaymentPenaltyFlow (zipWith mulBR prepaymentFlow rs) trs
        Just (StepDown ps) ->
          let 
            rs = lastN flowSize $ concat [ replicate n r | (n,r) <- ps]
          in 
            CF.CashFlowFrame $ CF.setPrepaymentPenaltyFlow (zipWith mulBR prepaymentFlow rs) trs


instance Ast.Asset Mortgage where
  calcCashflow m@(Mortgage (MortgageOriginalInfo ob or ot p sd ptype _)  _bal _rate _term _mbn _) d mRates
    = projCashflow m d (MortgageAssump Nothing Nothing Nothing Nothing)  mRates

  calcCashflow s@(ScheduleMortgageFlow beg_date flows)  d _ = CF.CashFlowFrame flows
  calcCashflow m@(AdjustRateMortgage _origin _arm  _bal _rate _term _mbn _status) d mRates = error "TBD"
  
  getCurrentBal (Mortgage _ _bal _ _ _ _) = _bal
  getCurrentBal (AdjustRateMortgage _ _ _bal _ _ _ _) = _bal

  getOriginBal (Mortgage (MortgageOriginalInfo _bal _ _ _ _ _ _) _ _ _ _ _ ) = _bal
  getOriginBal (AdjustRateMortgage (MortgageOriginalInfo _bal _ _ _ _ _ _) _ _ _ _ _ _ ) = _bal
  
  getOriginRate m
    = let 
        (MortgageOriginalInfo _ or _ _ _ _ _) = getOriginInfo m
      in  
        case or of
          IR.Fix _ _r -> _r
          IR.Floater _ _ _ _r _ _ _ _ -> _r 

  getPaymentDates (Mortgage (MortgageOriginalInfo _ _ ot p sd _ _) _ _ ct _ _) extra
    = genDates sd p (ot+extra)
  
  getPaymentDates (AdjustRateMortgage (MortgageOriginalInfo _ _ ot p sd _ _) _ _ _ ct _ _) extra
    = genDates sd p (ot+extra)

  isDefaulted (Mortgage _ _ _ _ _ (Defaulted _)) = True
  isDefaulted (AdjustRateMortgage _ _ _ _ _ _ (Defaulted _)) = True
  isDefaulted (Mortgage {}) = False
  isDefaulted (AdjustRateMortgage {}) = False
  
  getOriginDate (Mortgage (MortgageOriginalInfo _ _ ot p sd _ _) _ _ ct _ _) = sd
  getOriginDate (AdjustRateMortgage (MortgageOriginalInfo _ _ ot p sd _ _) _ _ _ ct _ _) = sd

  getRemainTerms (Mortgage (MortgageOriginalInfo _ _ ot p sd _ _) _ _ ct _ _) = ct
  getRemainTerms (AdjustRateMortgage (MortgageOriginalInfo _ _ ot p sd _ _) _ _ _ ct _ _) = ct

  getOriginInfo (Mortgage oi _ _ _ _ _) = oi
  getOriginInfo (AdjustRateMortgage oi _ _ _ _ _ _) = oi

  updateOriginDate (Mortgage (MortgageOriginalInfo ob or ot p sd _type mpn) cb cr ct mbn st) nd 
    = Mortgage (MortgageOriginalInfo ob or ot p nd _type mpn) cb cr ct mbn st 
  updateOriginDate (AdjustRateMortgage (MortgageOriginalInfo ob or ot p sd _type mpn) arm cb cr ct mbn st) nd 
    = AdjustRateMortgage (MortgageOriginalInfo ob or ot p nd _type mpn) arm cb cr ct mbn st
  
  -- project current mortgage(without delinq)
  projCashflow m@(Mortgage (MortgageOriginalInfo ob or ot p sd prinPayType mpn) cb cr rt mbn Current) 
               asOfDay 
               mars@(A.MortgageAssump amd amp amr ams) 
               mRates =
    let 
      futureTxns = cutBy Inc Future asOfDay txns 
    in 
      patchPrepayPentalyFlow m (CF.CashFlowFrame futureTxns)
    where
      last_pay_date:cf_dates = lastN (recovery_lag + rt + 1) $ sd:(getPaymentDates m recovery_lag)  
      cf_dates_length = length cf_dates 
      
      rate_vector = A.projRates or mRates cf_dates
      
      (ppy_rates,def_rates,recovery_rate,recovery_lag) = Ast.buildAssumptionPpyDefRecRate (last_pay_date:cf_dates) mars
      txns = projectMortgageFlow [] cb (toRational <$> mbn) last_pay_date cf_dates def_rates ppy_rates (replicate cf_dates_length 0.0) (replicate cf_dates_length 0.0) rate_vector (recovery_lag,recovery_rate) p prinPayType 

  -- project current mortgage(with delinq)
  projCashflow m@(Mortgage (MortgageOriginalInfo ob or ot p sd prinPayType mpn) cb cr rt mbn Current) 
               asOfDay 
               mars
               mRates =
    let 
      futureTxns = cutBy Inc Future asOfDay txns 
    in 
      patchPrepayPentalyFlow m (CF.CashFlowFrame futureTxns)
    where
      last_pay_date:cf_dates = lastN (recoveryLag + rt + 1) $ sd:(getPaymentDates m recoveryLag)  
      cf_dates_length = length cf_dates 
      
      rate_vector = A.projRates or mRates cf_dates
      
      (ppyRates, delinqRates,(defaultPct,defaultLag),recoveryRate,recoveryLag) = Ast.buildAssumptionPpyDelinqDefRecRate (last_pay_date:cf_dates) mars
      
      txns = projectDelinqMortgageFlow [] cb (toRational <$> mbn) last_pay_date cf_dates delinqRates  ppyRates rate_vector 
                                       (defaultPct,defaultLag,recoveryRate,recoveryLag,p,prinPayType) 
                                       ((replicate cf_dates_length 0.0),(replicate cf_dates_length 0.0),(replicate cf_dates_length 0.0))
  -- project defaulted Mortgage    
  projCashflow m@(Mortgage (MortgageOriginalInfo ob or ot p sd prinPayType mpn) cb cr rt mbn (Defaulted (Just defaultedDate)) ) asOfDay (A.DefaultedRecovery rr lag timing) _ =
    let 
      (cf_dates1,cf_dates2) = splitAt lag $ genDates defaultedDate p (lag+ length timing)
      beforeRecoveryTxn = [ CF.MortgageFlow d cb 0 0 0 0 0 0 0 cr mbn Nothing | d <- cf_dates1 ]
      recoveries = calcRecoveriesFromDefault cb rr timing
      bals = scanl (-) cb recoveries
      txns = [ CF.MortgageFlow d b 0 0 0 0 0 r 0 cr mbn Nothing | (b,d,r) <- zip3 bals cf_dates2 recoveries ]
    in 
      CF.CashFlowFrame $ cutBy Inc Future asOfDay (beforeRecoveryTxn ++ txns)

  -- project defaulted adjMortgage with a defaulted Date   
  projCashflow m@(AdjustRateMortgage mo arm cb cr rt mbn (Defaulted (Just defaultedDate)) ) asOfDay assumps mRates
    = projCashflow (Mortgage mo cb cr rt mbn  (Defaulted (Just defaultedDate))) asOfDay assumps mRates
  -- project defaulted adjMortgage without a defaulted Date   
  projCashflow m@(AdjustRateMortgage _ _ cb cr rt mbn (Defaulted Nothing) ) asOfDay assumps _
    = CF.CashFlowFrame $ [ CF.MortgageFlow asOfDay cb 0 0 0 0 0 0 0 cr mbn Nothing ]
  -- project defaulted Mortgage    
  projCashflow m@(Mortgage _ cb cr rt mbn (Defaulted Nothing) ) asOfDay assumps _
    = CF.CashFlowFrame $ [ CF.MortgageFlow asOfDay cb 0 0 0 0 0 0 0 cr mbn Nothing ]

  -- project current AdjMortgage
  projCashflow m@(AdjustRateMortgage (MortgageOriginalInfo ob or ot p sd prinPayType mpn) arm cb cr rt mbn Current) 
               asOfDay 
               mars@(A.MortgageAssump amd amp amr ams) 
               mRates =
    let 
      futureTxns = cutBy Inc Future asOfDay txns 
    in 
      patchPrepayPentalyFlow m (CF.CashFlowFrame futureTxns) 
    where
      ARM initPeriod initCap periodicCap lifeCap lifeFloor = arm
      passInitPeriod = (ot - rt) >= initPeriod 
      firstResetDate = monthsAfter sd (toInteger (succ initPeriod))

      last_pay_date:cf_dates = sliceDates (SliceOnAfterKeepPrevious asOfDay)  $ lastN (rt + recovery_lag + 1) $ sd:(getPaymentDates m recovery_lag) 
      
      cf_dates_length = length cf_dates -- `debug` (" cf dates >>" ++ show (last_pay_date:cf_dates ))
      rate_curve = case or of
                      IR.Fix _ r ->  error "ARM should have floater rate"
                      IR.Floater _ idx sprd initRate dp _ _ mRoundBy ->
                        let 
                          resetDates = genSerialDatesTill2 IE firstResetDate dp (last cf_dates)
                          projectFutureActualCurve = runInterestRate2 arm (sd,getOriginRate m) or resetDates
                        in 
                          case A.getRateAssumption (fromMaybe [] mRates) idx of
                            Just (RateCurve idx curve) 
                              -> projectFutureActualCurve curve -- `debug` ("Curve")
                            Just (RateFlat idx v) 
                              -> projectFutureActualCurve (mkRateTs [(getOriginDate m,v),(last cf_dates,v)]) -- `debug` ("lpd"++show last_pay_date++"lpd"++ show (last cf_dates))
                            Nothing -> error $ "Failed to find index"++ show idx

      rate_vector = fromRational <$> getValByDates rate_curve Inc cf_dates -- `debug` ("RateCurve"++ show rate_curve)

      (ppy_rates,def_rates,recovery_rate,recovery_lag) = buildAssumptionPpyDefRecRate (last_pay_date:cf_dates) mars 
      txns = projectMortgageFlow [] cb (toRational <$> mbn) last_pay_date cf_dates def_rates ppy_rates (replicate cf_dates_length 0.0) (replicate cf_dates_length 0.0) rate_vector (recovery_lag,recovery_rate) p prinPayType 

  projCashflow (ScheduleMortgageFlow beg_date flows) asOfDay assumps _
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
        (ppy_rates,def_rates,recovery_rate,recovery_lag) = buildAssumptionPpyDefRecRate (beg_date:cf_dates) assumps 
        curve_dates_length =  recovery_lag + length flows
        temp_p = Lib.Monthly -- TODO to fix this hard code
        cf_dates = (map CF.getDate flows) ++ (genDates (CF.getDate (last flows)) temp_p recovery_lag)

  getBorrowerNum m@(Mortgage (MortgageOriginalInfo ob or ot p sd prinPayType _) cb cr rt mbn _ ) = fromMaybe 1 mbn
  getBorrowerNum m@(AdjustRateMortgage (MortgageOriginalInfo ob or ot p sd prinPayType _) _ cb cr rt mbn _ ) = fromMaybe 1 mbn

  splitWith (Mortgage (MortgageOriginalInfo ob or ot p sd prinPayType mpn) cb cr rt mbn st ) rs 
    = [ Mortgage (MortgageOriginalInfo (mulBR ob ratio) or ot p sd prinPayType mpn) (mulBR cb ratio) cr rt mbn st 
       | ratio <- rs ]
  
  splitWith (AdjustRateMortgage (MortgageOriginalInfo ob or ot p sd prinPayType mpn) arm cb cr rt mbn st ) rs 
    = [ AdjustRateMortgage (MortgageOriginalInfo (mulBR ob ratio) or ot p sd prinPayType mpn) arm (mulBR cb ratio) cr rt mbn st 
       | ratio <- rs ]
  

