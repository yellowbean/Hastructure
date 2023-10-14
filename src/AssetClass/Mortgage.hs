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
import DateUtil
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
import GHC.Float.RealFracMethods (truncateFloatInteger)
debug = flip trace

projectMortgageFlow :: [CF.TsRow] -> Balance -> Maybe Rational -> Date -> Dates -> [DefaultRate] -> [PrepaymentRate] -> [Amount] -> [Amount] -> [IRate] -> (Int,Rate) -> Period -> AmortPlan -> [CF.TsRow]
projectMortgageFlow trs _bal mbn _last_date (pDate:pDates) (_def_rate:_def_rates) (_ppy_rate:_ppy_rates) _rec_vector@(_rec_amt:_rec_amts) _loss_vector@(_loss_amt:_loss_amts) (_rate:_rates) (recovery_lag,recovery_rate) p pt
  | _bal > 0.01 = projectMortgageFlow
                    (trs++[tr])
                    endBal
                    ( toRational <$> newMbn)
                    pDate
                    pDates
                    _def_rates
                    _ppy_rates
                    (tail currentRec) 
                    (tail currentLoss) 
                    _rates
                    (recovery_lag,recovery_rate)
                    p
                    pt 
                  where
                    remainTerms = 1 + max 0 (length pDates - recovery_lag) -- `debug` ("IN mortgage flow"++ show remainTerms)
                    newDefault = mulBR _bal _def_rate
                    newBalAfterDefault = _bal - newDefault
                    newPrepay = mulBR newBalAfterDefault _ppy_rate
                    newBalAfterPpy = newBalAfterDefault - newPrepay
                    newInt = mulBI newBalAfterPpy (periodRateFromAnnualRate p _rate)  -- `debug` ("Balance"++show(newBalAfterPpy))
                    pmt = calcPmt newBalAfterPpy (periodRateFromAnnualRate p _rate) remainTerms -- `debug` ("pmt->bal"++show newBalAfterPpy++"rate"++show _rate++"term"++show remainTerms)
                    newPrin = case pt of
                                    Level -> pmt - newInt -- `debug` ("PMT->"++ show pmt)
                                    Even ->  newBalAfterPpy / fromIntegral remainTerms -- `debug` ("Dividing _remain"++show remainTerms ) --(ob / (fromIntegral ot)) * (newBalAfterPpy / ob)

                    newRec = mulBR newDefault recovery_rate
                    newLoss = mulBR newDefault (1 - recovery_rate)

                    currentRec = replace _rec_vector recovery_lag newRec
                    currentLoss = replace _loss_vector recovery_lag newLoss

                    endBal = newBalAfterPpy - newPrin
                    _survive_rate = ((1 - _def_rate) * (1 - _ppy_rate))  
                    _temp = _survive_rate * (toRational (1 - newPrin / newBalAfterPpy))
                    newMbn = (\y -> fromInteger (round (_temp * (toRational y)))) <$> mbn
                    tr = CF.MortgageFlow pDate endBal newPrin newInt newPrepay newDefault (head currentRec) (head currentLoss) _rate newMbn Nothing --TODO missing ppy-penalty here

projectMortgageFlow trs _b mbn _last_date (pDate:_pdates) _  _ (_rec_amt:_rec_amts) (_loss_amt:_loss_amts) _ _lag_rate _p _pt
 = projectMortgageFlow (trs++[tr]) _b mbn pDate _pdates [] [] _rec_amts _loss_amts [0.0] _lag_rate _p _pt
  where
    tr = CF.MortgageFlow pDate _b 0 0 0 0 _rec_amt _loss_amt 0.0 Nothing Nothing
projectMortgageFlow trs _ _ _ [] _ _ [] [] _ _ _ _ = trs  


projectDelinqMortgageFlow :: ([CF.TsRow],[CF.TsRow]) -> Balance -> Maybe Rational -> Date -> [Date] -> [Rate] -> [PrepaymentRate] -> [IRate] -> (Rate,Lag,Rate,Lag,Period,AmortPlan) -> ([Balance],[Balance],[Balance]) -> [CF.TsRow]
projectDelinqMortgageFlow (trs,[]) _ _ _ [] _ _ _ _ _ = trs
projectDelinqMortgageFlow (trs,backToPerfs) _ _ _ [] _ _ _ _ _ = 
  let 
    consolTxn = sort backToPerfs -- `debug` ("Hit pay dates = []")
    (trsKeep,trsMerge) = splitByDate trs (getDate (head backToPerfs)) EqToRight
    mergedTrs = CF.combineTss [] trsMerge consolTxn -- `debug` ("before Merge for delinq Mortgage \n >>> "++ show trs++"Back to Perf"++ show backToPerfs)
  in 
    trsKeep ++ mergedTrs -- `debug` ("\n MergedTrs \n"++ show mergedTrs)

projectDelinqMortgageFlow (trs,backToPerfs) beginBal mbn lastDate (pDate:pDates) (delinqRate:delinqRates) (ppyRate:ppyRates) (rate:rates) 
                          (defaultPct,defaultLag,recoveryRate,recoveryLag,p,prinType) 
                          (dBal:defaultVec,rAmt:recoveryVec,lAmt:lossVec)
  -- | beginBal < 0.01 = let
  --                       (trsKeep,trsMerge) = splitByDate trs (getDate (head backToPerfs)) EqToRight
  --                       mergedTrs = CF.combineTss [] trsMerge backToPerfs `debug` ("trsMerge"++show trsMerge++"defalt/rec/loss"++show (dBal,rAmt,lAmt))
  --                     in 
  --                       trsKeep ++ mergedTrs `debug` ("MErgeed \n \n "++ show mergedTrs)
  | otherwise = projectDelinqMortgageFlow (trs++[tr],CF.combineTss [] backToPerfs newPerfCfs) endingBal ( (downFactor *) <$> mbn) pDate pDates delinqRates ppyRates rates 
                              (defaultPct,defaultLag,recoveryRate,recoveryLag,p,prinType) 
                              (newDefaultVec,newRecoveryVec,newLossVec) -- `debug` ("\n calc Date"++ show pDate ++"\n from new perf"++ show backToPerfBal ++"\n new cfs >>> \n"++ show newPerfCfs)
                where 
                  remainTerms = 1 + max 0 (length pDates - recoveryLag - defaultLag) 
                  -- remainTerms = 1 + length (pDates) - recoveryLag - defaultLag
                  delinqBal = mulBR beginBal delinqRate
                  
                  defaultBal = mulBR delinqBal defaultPct 
                  recBal = mulBR defaultBal recoveryRate
                  lossBal = mulBR defaultBal (1 - recoveryRate)
                  
                  newDefaultVec = replace defaultVec (pred defaultLag) defaultBal
                  newRecoveryVec = replace recoveryVec (pred recoveryLag + defaultLag) recBal
                  newLossVec = replace lossVec (pred recoveryLag + defaultLag) lossBal
                  
                  backToPerfBal = mulBR delinqBal (1 - defaultPct)
                  
                  restPerfVector = replicate (succ (length delinqRates)) 0
                  restPerfBal = fromRational <$> restPerfVector -- `debug` ("Dates"++show (pDate:pDates))
                  newPerfCfs = if backToPerfBal > 0.0 then
                                 projectDelinqMortgageFlow ([],[]) backToPerfBal Nothing (pDates!!(defaultLag)) (drop defaultLag (pDate:pDates))
                                                           restPerfVector restPerfVector 
                                                           (drop defaultLag (rate:rates))
                                                           (0,0,0,0,p,prinType)
                                                           (restPerfBal,restPerfBal,restPerfBal) -- `debug` ("\nStarting new perf >>> \n"++ show backToPerfBal)
                               else
                                 []
                  
                  balAfterDelinq = beginBal - delinqBal
                  ppyAmt = mulBR balAfterDelinq ppyRate 
                  balAfterPpy  = balAfterDelinq - ppyAmt
                  periodRate = periodRateFromAnnualRate p rate
                  intAmt = mulBI balAfterPpy periodRate
                  pmt = calcPmt balAfterPpy periodRate remainTerms
                  prinAmt = case prinType of
                              Level -> pmt - intAmt  -- `debug` ("Pmt>> \n"++show pmt++">>used bal"++show balAfterPpy++">>"++show periodRate++">>remain term"++show remainTerms)
                              Even ->  balAfterPpy / fromIntegral remainTerms

                  endingBal = beginBal - prinAmt - ppyAmt - delinqBal -- `debug` ("DATE"++show pDate++">>>"++ show beginBal++">>"++show prinAmt ++ ">>" ++ show ppyAmt ++ ">>"++ show delinqBal)
                  downFactor = divideBB beginBal endingBal
                  newMbn = case mbn of 
                             Just bn -> Just (((fromInteger . round) ((divideBB beginBal endingBal) * bn))::Int)
                             Nothing -> Nothing
                  -- mNewBorrowerNum = (fromIntegral . toInteger . fromRational) <$> ((divideBB beginBal endingBal) *) <$> mBorrowerNum
                  tr = CF.MortgageDelinqFlow pDate endingBal prinAmt intAmt ppyAmt delinqBal dBal rAmt lAmt rate newMbn Nothing -- `debug` ("Date"++ show pDate ++ "ENDING BAL AT"++ show endingBal)


projectScheduleFlow :: [CF.TsRow] -> Rate -> Balance -> [CF.TsRow] -> [DefaultRate] -> [PrepaymentRate] -> [Amount] -> [Amount] -> (Int, Rate) -> [CF.TsRow]
projectScheduleFlow trs _ last_bal [] _ _ [] [] (_,_) = trs 
projectScheduleFlow trs bal_factor last_bal (flow:flows) (_def_rate:_def_rates) (_ppy_rate:_ppy_rates) _rec _loss (recoveryLag,recoveryRate)
  = projectScheduleFlow (trs++[tr]) _survive_rate endBal flows _def_rates _ppy_rates (tail recVector) (tail lossVector) (recoveryLag,recoveryRate) -- `debug` ("===>C")
     where
       startBal = last_bal
       defAmt = mulBR startBal _def_rate
       ppyAmt = mulBR (startBal - defAmt) _ppy_rate -- `debug` (">>>"++ show (_start_bal - _def_amt)++">>>"++ show (fromRational _ppy_rate) ++">>>"++ show ((_start_bal - _def_amt) * (fromRational _ppy_rate)))      -- `debug` (show _start_bal ++">>"++ show (fromRational _def_rate) ++ ">>" ++"DEF AMT"++ show _def_amt)-- `debug` ("Def amt"++show(_def_amt)++"Def rate"++show(_def_rate))
       afterBal = startBal - defAmt - ppyAmt   -- `debug` ("PPY AMT"++ show _ppy_amt ++ ">>>" ++ show (fromRational _ppy_rate))
       
       _survive_rate = (1 - _def_rate) * (1 - _ppy_rate) * bal_factor -- `debug` ("Bal factor"++show(bal_factor))
       _schedule_bal = CF.mflowBalance flow
       _schedule_prin = mulBR (CF.mflowPrincipal flow) _survive_rate --TODO round trip  -- `debug` ("Schedule Principal"++(printf "%.2f" (CF.mflowPrincipal flow))++" Rate"++show(_schedule_rate))
       _schedule_int = mulBR (CF.mflowInterest flow) _survive_rate

       newRec = mulBR defAmt recoveryRate
       newLoss = mulBR defAmt (1 - recoveryRate)

       recVector = replace _rec recoveryLag newRec
       lossVector = replace _loss recoveryLag newLoss

       endBal = max 0 $ afterBal - _schedule_prin

       tr = CF.MortgageFlow (CF.getDate flow) endBal _schedule_prin _schedule_int ppyAmt defAmt (head recVector) (head lossVector) 0.0 Nothing Nothing --TODO missing ppy-penalty here

projectScheduleFlow trs b_factor lastBal [] _ _ (r:rs) (l:ls) (recovery_lag,recovery_rate)
  = projectScheduleFlow (trs++[tr]) b_factor lastBal [] [] [] rs ls (recovery_lag - 1,recovery_rate) 
   where
      remain_length = length rs
      lastDate = CF.getDate (last trs)
      flowDate = nextDate lastDate Lib.Monthly
      tr = CF.MortgageFlow flowDate lastBal 0 0 0 0 r l 0.0 Nothing Nothing

type DelinqRate = Rate
projectScheduleDelinqFlow :: ([CF.TsRow],[CF.TsRow]) -> Rate -> Balance -> [CF.TsRow] -> [DelinqRate] -> [PrepaymentRate] -> [Amount] -> [Amount] -> [Amount] -> (Rate,Int,Rate,Int) -> [CF.TsRow]
projectScheduleDelinqFlow (trs,[]) _ begBal flows [] [] defaults recoveries losses _ = 
  let 
    patchedFlows = [ CF.MortgageDelinqFlow d begBal prin int prepay delinq defVal recVal lossVal rate mB mPPN   
                    | (CF.MortgageDelinqFlow d bal prin int prepay delinq _ _ _ rate mB mPPN,defVal,recVal,lossVal) <- zip4 flows defaults recoveries losses] -- `debug` ("Length of default"++ show defaults++">>recovery>>"++ show recoveries++">>loss>>"++ show losses)
    r1 = sort $ trs ++ patchedFlows -- `debug` ("Patched rows\n"++show patchedFlows)
  in 
    r1
    
projectScheduleDelinqFlow (trs,newPerfs) _ begBal flows [] [] defaults recoveries losses _ = 
  let 
    patchedFlows = [ CF.MortgageDelinqFlow d begBal prin int prepay delinq defVal recVal lossVal rate mB mPPN   
                    | (CF.MortgageDelinqFlow d bal prin int prepay delinq _ _ _ rate mB mPPN,defVal,recVal,lossVal) <- zip4 flows defaults recoveries losses] -- `debug` ("Length of default"++ show defaults++">>recovery>>"++ show recoveries++">>loss>>"++ show losses)
    r1 = sort $ trs ++ patchedFlows -- `debug` ("Patched rows\n"++show patchedFlows)
    r3 = CF.aggregateTsByDate [] $ sort newPerfs -- `debug` ("New Perfs\n"++ show newPerfs)
    (r1keep, r1merge) = splitByDate r1 (getDate  (head r3)) EqToRight  -- `debug` ("r3 \n"++ show r3)
    r4 = CF.combineTss [] r1merge r3 -- `debug` ("r1keep \n"++ show r1keep++"\n r1merge \n"++ show r1merge)
  in 
    r1keep ++ r4 -- `debug` ("r4 \n"++ show r4)

projectScheduleDelinqFlow (trs,backToPerfCfs) surviveRate begBal (flow:flows) (delinqRate:delinqRates) (ppyRate:ppyRates) (defaultBal:defaultBals) (recoveryBal:recoveryBals) (lossBal:lossBals) (defaultPct,defaultLag,recoveryRate,recoveryLag)
  = projectScheduleDelinqFlow (trs++[tr],CF.combineTss [] backToPerfCfs currentBackToPerfCfs) newSurviveRate endBal flows delinqRates ppyRates newDefaultBals newRecoveryBals newLossBals (defaultPct,defaultLag,recoveryRate,recoveryLag) -- `debug` ("new back to perf flow"++ show backToPerfCfs)
    where 
      delinqAmt = mulBR begBal delinqRate -- `debug` ("delinq Rate"++ show delinqRate)
      ppyAmt = mulBR (begBal - delinqAmt) ppyRate -- `debug` ("begbal"++ show begBal++">>"++ show delinqAmt)
      newSurviveRate = (1-delinqRate) * (1-ppyRate) * surviveRate

      scheduleBal = CF.mflowBalance flow
      schedulePrin = mulBR (CF.mflowPrincipal flow) surviveRate
      scheduleInt = mulBR (CF.mflowInterest flow) surviveRate

      newDefaultBal = mulBR delinqAmt defaultPct
      endBal = max 0 $ (begBal - delinqAmt - ppyAmt - schedulePrin)
      currentBackToPerfCfs = let 
                               futureDs = drop (defaultLag+recoveryLag) $ getDates (flow:flows)
                               splitPct = divideBB (mulBR delinqAmt (1-defaultPct)) begBal
                               perfFlows = take (length flows - defaultLag - recoveryLag + 1) $ CF.splitTrs splitPct (flow:flows)
                             in 
                               [ CF.tsSetDate f d | (d,f) <- zip futureDs perfFlows ]

      newDefaultBals = replace defaultBals (pred defaultLag) newDefaultBal  
      newRecoveryBals = replace recoveryBals (recoveryLag + pred defaultLag) (mulBR newDefaultBal recoveryRate)  
      newLossBals =  replace lossBals (recoveryLag + pred defaultLag) (mulBR newDefaultBal (1-recoveryRate)) -- `debug` ("new loss def"++ show defaultBal++">>rate"++ show (1-recoveryRate) )
      tr = CF.MortgageDelinqFlow (CF.getDate flow) endBal schedulePrin scheduleInt ppyAmt delinqAmt defaultBal recoveryBal lossBal (CF.mflowRate flow) Nothing 
                                 Nothing -- `debug` ("|||>>> proj at date"++ show (CF.getDate flow))

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
  calcCashflow m@(Mortgage (MortgageOriginalInfo ob or ot p sd ptype _)  _bal _rate _term _mbn Current) d mRates
    = fst (projCashflow m d (MortgageAssump Nothing Nothing Nothing Nothing,A.DummyDelinqAssump,A.DummyDefaultAssump) mRates)


  calcCashflow s@(ScheduleMortgageFlow beg_date flows _)  d _ = CF.CashFlowFrame flows
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
  isDefaulted Mortgage {} = False
  isDefaulted AdjustRateMortgage {} = False
  
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
               mars@(A.MortgageAssump amd amp amr ams
                     ,_
                     ,_) 
               mRates =
    let 
      (futureTxns,historyM)= CF.cutoffTrs asOfDay txns 
    in 
      (applyHaircut ams $ patchPrepayPentalyFlow m (CF.CashFlowFrame futureTxns)
      ,historyM)
    where
      (ppy_rates,def_rates,recoveryRate,recoveryLag) = Ast.buildAssumptionPpyDefRecRate (lastPayDate:cfDates) (A.MortgageAssump amd amp amr ams) -- `debug` ("Rate vector"++ show rate_vector)
      lastPayDate:cfDates = lastN (recoveryLag + rt + 1) $ sd:getPaymentDates m recoveryLag  
      cfDatesLength = length cfDates 
      
      rateVector = A.projRates cr or mRates cfDates 
      
      txns = projectMortgageFlow [] cb (toRational <$> mbn) lastPayDate cfDates def_rates ppy_rates 
                                 (replicate cfDatesLength 0.0) (replicate cfDatesLength 0.0) rateVector 
                                 (recoveryLag,recoveryRate) p prinPayType   

  -- project current mortgage(with delinq)
  projCashflow m@(Mortgage (MortgageOriginalInfo ob or ot p sd prinPayType mpn) cb cr rt mbn Current) 
               asOfDay 
               mars@(A.MortgageDeqAssump amd amp amr ams
                    ,_
                    ,_) 
               mRates =
    let 
      (futureTxns,historyM)= CF.cutoffTrs asOfDay txns 
    in 
      (applyHaircut ams $ patchPrepayPentalyFlow m (CF.CashFlowFrame futureTxns)
      ,historyM)
    where
      lastPayDate:cfDates = lastN (recoveryLag + defaultLag + rt + 1) $ sd:(getPaymentDates m (recoveryLag+defaultLag))
      cfDatesLength = length cfDates + recoveryLag + defaultLag

      rateVector = A.projRates cr or mRates cfDates

      (ppyRates,delinqRates,(defaultPct,defaultLag),recoveryRate,recoveryLag) = Ast.buildAssumptionPpyDelinqDefRecRate (lastPayDate:cfDates) (A.MortgageDeqAssump amd amp amr ams)
      
      txns = projectDelinqMortgageFlow ([],[]) cb (toRational <$> mbn) lastPayDate cfDates delinqRates ppyRates rateVector 
                                       (defaultPct,defaultLag,recoveryRate,recoveryLag,p,prinPayType) 
                                       (replicate cfDatesLength 0.0,replicate cfDatesLength 0.0,replicate cfDatesLength 0.0)
  -- project defaulted Mortgage    
  projCashflow m@(Mortgage (MortgageOriginalInfo ob or ot p sd prinPayType mpn) cb cr rt mbn (Defaulted (Just defaultedDate)) ) 
               asOfDay
               (_,_,A.DefaultedRecovery rr lag timing) _ =
    let 
      (emptyDates,recoveryDates) = splitAt (pred lag) $ genDates defaultedDate p (lag + length timing)
      beforeRecoveryTxn = [ CF.MortgageFlow d 0 0 0 0 0 0 0 cr mbn Nothing | d <- emptyDates ]
      recoveries = calcRecoveriesFromDefault cb rr timing
      txns = [ CF.MortgageFlow d 0 0 0 0 0 r 0 cr mbn Nothing | (d,r) <- zip recoveryDates recoveries ]
    in 
      (CF.CashFlowFrame $ cutBy Inc Future asOfDay (beforeRecoveryTxn ++ txns)
      ,Map.empty)

  -- project defaulted adjMortgage with a defaulted Date   
  projCashflow m@(AdjustRateMortgage mo arm cb cr rt mbn (Defaulted (Just defaultedDate)) ) asOfDay assumps mRates
    = projCashflow (Mortgage mo cb cr rt mbn  (Defaulted (Just defaultedDate))) asOfDay assumps mRates
  -- project defaulted adjMortgage without a defaulted Date   
  projCashflow m@(AdjustRateMortgage _ _ cb cr rt mbn (Defaulted Nothing) ) asOfDay assumps _
    = (CF.CashFlowFrame $ [ CF.MortgageFlow asOfDay 0 0 0 0 0 0 0 cr mbn Nothing ]
      ,Map.empty)
  -- project defaulted Mortgage    
  projCashflow m@(Mortgage _ cb cr rt mbn (Defaulted Nothing) ) asOfDay assumps _
    = (CF.CashFlowFrame $ [ CF.MortgageFlow asOfDay 0 0 0 0 0 0 0 cr mbn Nothing ]
      ,Map.empty)

  -- project current AdjMortgage
  projCashflow m@(AdjustRateMortgage (MortgageOriginalInfo ob or ot p sd prinPayType mpn) arm cb cr rt mbn Current) 
               asOfDay 
               mars@(A.MortgageAssump amd amp amr ams,_,_) 
               mRates =
    let 
      (futureTxns,historyM)= CF.cutoffTrs asOfDay txns 
    in 
      (applyHaircut ams $ patchPrepayPentalyFlow m (CF.CashFlowFrame futureTxns)
      ,historyM)
    where
      ARM initPeriod initCap periodicCap lifeCap lifeFloor = arm
      passInitPeriod = (ot - rt) >= initPeriod 
      firstResetDate = monthsAfter sd (toInteger (succ initPeriod))

      lastPayDate:cfDates = sliceDates (SliceOnAfterKeepPrevious asOfDay)  $ lastN (rt + recoveryLag + 1) $ sd:(getPaymentDates m recoveryLag) 
      
      cfDatesLength = length cfDates -- `debug` (" cf dates >>" ++ show (last_pay_date:cf_dates ))
      rateCurve = case or of
                      IR.Fix _ r ->  error "ARM should have floater rate"
                      IR.Floater _ idx sprd initRate dp _ _ mRoundBy ->
                        let 
                          resetDates = genSerialDatesTill2 IE firstResetDate dp (last cfDates)
                          projectFutureActualCurve = runInterestRate2 arm (sd,getOriginRate m) or resetDates
                        in 
                          case A.getRateAssumption (fromMaybe [] mRates) idx of
                            Just (RateCurve idx curve) 
                              -> projectFutureActualCurve curve -- `debug` ("Curve")
                            Just (RateFlat idx v) 
                              -> projectFutureActualCurve (mkRateTs [(getOriginDate m,v),(last cfDates,v)]) -- `debug` ("lpd"++show last_pay_date++"lpd"++ show (last cf_dates))
                            Nothing -> error $ "Failed to find index"++ show idx

      rate_vector = fromRational <$> getValByDates rateCurve Inc cfDates -- `debug` ("RateCurve"++ show rate_curve)

      (ppyRates,defRates,recoveryRate,recoveryLag) = buildAssumptionPpyDefRecRate (lastPayDate:cfDates) (A.MortgageAssump amd amp amr ams)
      txns = projectMortgageFlow [] cb (toRational <$> mbn) lastPayDate cfDates defRates ppyRates (replicate cfDatesLength 0.0) (replicate cfDatesLength 0.0) rate_vector (recoveryLag,recoveryRate) p prinPayType 

  -- project current AdjMortgage with delinq
  projCashflow m@(AdjustRateMortgage (MortgageOriginalInfo ob or ot p sd prinPayType mpn) arm cb cr rt mbn Current) 
               asOfDay 
               mars@(A.MortgageAssump amd amp amr ams,_,_) 
               mRates =
    let 
      (futureTxns,historyM)= CF.cutoffTrs asOfDay txns 
    in 
      (applyHaircut ams $ patchPrepayPentalyFlow m (CF.CashFlowFrame futureTxns)
      ,historyM)
    where
      ARM initPeriod initCap periodicCap lifeCap lifeFloor = arm
      passInitPeriod = (ot - rt) >= initPeriod 
      firstResetDate = monthsAfter sd (toInteger (succ initPeriod))
      lastPayDate:cfDates = lastN (recoveryLag + defaultLag + rt + 1) $ sd:(getPaymentDates m recoveryLag)  
      cfDatesLength = length cfDates 
      rateCurve = case or of
                      IR.Fix _ r ->  error "ARM should have floater rate"
                      IR.Floater _ idx sprd initRate dp _ _ mRoundBy ->
                        let 
                          resetDates = genSerialDatesTill2 IE firstResetDate dp (last cfDates)
                          projectFutureActualCurve = runInterestRate2 arm (sd,getOriginRate m) or resetDates
                        in 
                          case A.getRateAssumption (fromMaybe [] mRates) idx of
                            Just (RateCurve idx curve) 
                              -> projectFutureActualCurve curve -- `debug` ("Curve")
                            Just (RateFlat idx v) 
                              -> projectFutureActualCurve (mkRateTs [(getOriginDate m,v),(last cfDates,v)]) -- `debug` ("lpd"++show last_pay_date++"lpd"++ show (last cf_dates))
                            Nothing -> error $ "Failed to find index"++ show idx
      
      rate_vector = fromRational <$> getValByDates rateCurve Inc cfDates -- `debug` ("RateCurve"++ show rate_curve)                                  

      (ppyRates, delinqRates,(defaultPct,defaultLag),recoveryRate,recoveryLag) = Ast.buildAssumptionPpyDelinqDefRecRate (lastPayDate:cfDates) (A.MortgageAssump amd amp amr ams)
      
      txns = projectDelinqMortgageFlow ([],[]) cb (toRational <$> mbn) lastPayDate cfDates delinqRates ppyRates rate_vector 
                                       (defaultPct,defaultLag,recoveryRate,recoveryLag,p,prinPayType) 
                                       ((replicate cfDatesLength 0.0),(replicate cfDatesLength 0.0),(replicate cfDatesLength 0.0))
  -- schedule mortgage flow without delinq
  projCashflow (ScheduleMortgageFlow begDate flows dp) asOfDay assumps@(pAssump@(A.MortgageAssump _ _ _ ams ),dAssump,fAssump) _
    = let 
        (futureTxns,historyM) = CF.cutoffTrs asOfDay txns 
      in 
        (applyHaircut ams (CF.CashFlowFrame futureTxns)
        ,historyM)
      where
        begBal =  CF.mflowBegBalance $ head flows 
        (ppyRates,defRates,recoveryRate,recoveryLag) = buildAssumptionPpyDefRecRate (begDate:cfDates) pAssump 
        curveDatesLength =  recoveryLag + length flows
        extraPeriods = recoveryLag
        endDate = CF.getDate (last flows)
        extraDates = genSerialDates dp endDate extraPeriods
        cfDates = (CF.getDate <$> flows) ++ extraDates
        txns = projectScheduleFlow
                 []
                 1.0
                 begBal
                 flows
                 defRates
                 ppyRates
                 (replicate curveDatesLength 0.0)
                 (replicate curveDatesLength 0.0)
                 (recoveryLag,recoveryRate) 

  
  -- schedule mortgage flow WITH delinq
  projCashflow (ScheduleMortgageFlow begDate flows dp) asOfDay assumps@(pAssump@(A.MortgageDeqAssump _ _ _ ams),dAssump,fAssump) mRates
    = let 
        (futureTxns,historyM) = CF.cutoffTrs asOfDay txns 
      in 
        (applyHaircut ams (CF.CashFlowFrame futureTxns)
        ,historyM)
      where
        begBal =  CF.mflowBegBalance $ head flows -- `debug` ("beg date"++show beg_date)
        (ppyRates, delinqRates,(defaultPct,defaultLag),recoveryRate,recoveryLag) = Ast.buildAssumptionPpyDelinqDefRecRate (begDate:getDates flows) pAssump
        curveDatesLength = defaultLag + recoveryLag + length flows -- `debug` ("Length of rates"++show (length delinqRates)++">>"++show (length ppyRates))
        extraPeriods = defaultLag + recoveryLag -- `debug` ("lags "++show defaultLag++">>"++show recoveryLag)
        endDate = CF.getDate (last flows) 
        extraDates = genSerialDates dp endDate extraPeriods
        -- extra empty cashflow with dates proj from `date pattern` from pool 
        extraFlows = [ CF.emptyTsRow d r | (d,r) <- zip extraDates (replicate extraPeriods (last flows)) ] 
        flowWithExtraDates = flows ++ extraFlows
        cfDates = getDates flowWithExtraDates -- `debug` ("CF dates"++ show flowWithExtraDates)
        txns = projectScheduleDelinqFlow 
                 ([],[])
                 1.0
                 begBal
                 flowWithExtraDates
                 delinqRates
                 ppyRates
                 (replicate curveDatesLength 0.0)
                 (replicate curveDatesLength 0.0)
                 (replicate curveDatesLength 0.0)
                 (defaultPct,defaultLag,recoveryRate,recoveryLag)  -- `debug` ("Delinq rates"++ show delinqRates++">>ppy rates"++ show ppyRates)
  
  projCashflow a b c d = error $ "Failed to match when proj mortgage>>" ++ show a ++ show b ++ show c ++ show d


  getBorrowerNum m@(Mortgage (MortgageOriginalInfo ob or ot p sd prinPayType _) cb cr rt mbn _ ) = fromMaybe 1 mbn
  getBorrowerNum m@(AdjustRateMortgage (MortgageOriginalInfo ob or ot p sd prinPayType _) _ cb cr rt mbn _ ) = fromMaybe 1 mbn

  splitWith (Mortgage (MortgageOriginalInfo ob or ot p sd prinPayType mpn) cb cr rt mbn st ) rs 
    = [ Mortgage (MortgageOriginalInfo (mulBR ob ratio) or ot p sd prinPayType mpn) (mulBR cb ratio) cr rt mbn st 
       | ratio <- rs ]
  
  splitWith (AdjustRateMortgage (MortgageOriginalInfo ob or ot p sd prinPayType mpn) arm cb cr rt mbn st ) rs 
    = [ AdjustRateMortgage (MortgageOriginalInfo (mulBR ob ratio) or ot p sd prinPayType mpn) arm (mulBR cb ratio) cr rt mbn st 
       | ratio <- rs ]
  

