{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module AssetClass.Mortgage  
  (projectMortgageFlow,projectScheduleFlow,updateOriginDate,getOriginInfo
  ,buildARMrates)
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
import AssetClass.AssetCashflow
import Debug.Trace
import Assumptions (AssetPerfAssumption(MortgageAssump))
import GHC.Float.RealFracMethods (truncateFloatInteger)
import Cashflow (extendTxns)
debug = flip trace

projectMortgageFlow :: (Balance, Date, Maybe BorrowerNum, AmortPlan, DayCount, IRate, Period, Int) -> (Dates, [DefaultRate],[PrepaymentRate],[IRate],[Int]) -> [CF.TsRow]
projectMortgageFlow (startBal, lastPayDate, mbn, pt, dc, startRate, p, oTerms) (cfDates, defRates, ppyRates, rateVector, remainTerms) = 
  let 
    initRow = CF.MortgageFlow lastPayDate startBal 0.0 0.0 0.0 0.0 0.0 0.0 startRate Nothing Nothing Nothing
 in 
    foldl 
      (\acc (pDate, defRate, ppyRate, intRate, rt)
          -> let 
               begBal = CF.mflowBalance (last acc)
               lastPaidDate = getDate (last acc)
               newDefault = mulBR begBal defRate 
               newPrepay = mulBR (begBal - newDefault) ppyRate
               _balAfterPpy = begBal - newDefault - newPrepay
               (newInt,newPrin) = calcAssetPrinInt pt _balAfterPpy (periodRateFromAnnualRate p intRate) oTerms rt
               endBal = _balAfterPpy - newPrin
               newMbn = decreaseBorrowerNum begBal endBal mbn
             in 
               acc ++ [CF.MortgageFlow pDate endBal newPrin newInt newPrepay newDefault 0.0 0.0 intRate newMbn Nothing Nothing])
      [initRow]
      (zip5 cfDates defRates ppyRates rateVector remainTerms)            
             

projectDelinqMortgageFlow :: ([CF.TsRow],[CF.TsRow]) -> Balance -> Maybe Int -> Date -> [Date] -> [Rate] -> [PrepaymentRate] -> [IRate] -> (Rate,Lag,Rate,Lag,Period,AmortPlan,Int) -> ([Balance],[Balance],[Balance]) -> [CF.TsRow]
projectDelinqMortgageFlow (trs,[]) _ _ _ [] _ _ _ _ _ = CF.dropTailEmptyTxns trs
projectDelinqMortgageFlow (trs,backToPerfs) _ _ _ [] _ _ _ _ _ = 
  let 
    consolTxn = sort backToPerfs -- `debug` ("Hit pay dates = []")
    (trsKeep,trsMerge) = splitByDate trs (getDate (head backToPerfs)) EqToRight
    mergedTrs = CF.combineTss [] trsMerge consolTxn -- `debug` ("before Merge for delinq Mortgage \n >>> "++ show trs++"Back to Perf"++ show backToPerfs)
  in 
    trsKeep ++ mergedTrs -- `debug` ("\n MergedTrs \n"++ show mergedTrs)

projectDelinqMortgageFlow (trs,backToPerfs) beginBal mbn lastDate (pDate:pDates) (delinqRate:delinqRates) (ppyRate:ppyRates) (rate:rates) 
                          (defaultPct,defaultLag,recoveryRate,recoveryLag,p,prinType,ot) 
                          (dBal:defaultVec,rAmt:recoveryVec,lAmt:lossVec)
   = projectDelinqMortgageFlow (trs++[tr],CF.combineTss [] backToPerfs newPerfCfs) endingBal newMbn pDate pDates delinqRates ppyRates rates 
                   (defaultPct,defaultLag,recoveryRate,recoveryLag,p,prinType,ot) 
                   (newDefaultVec,newRecoveryVec,newLossVec) -- `debug` ("\n calc Date"++ show pDate ++"\n from new perf"++ show backToPerfBal ++"\n new cfs >>> \n"++ show newPerfCfs)
     where 
       remainTerms = succ $ max 0 (length pDates - recoveryLag - defaultLag) 
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
                      projectDelinqMortgageFlow ([],[]) backToPerfBal Nothing (pDates!!defaultLag) (drop defaultLag (pDate:pDates))
                                                restPerfVector restPerfVector 
                                                (drop defaultLag (rate:rates))
                                                (0,0,0,0,p,prinType,ot)
                                                (restPerfBal,restPerfBal,restPerfBal) -- `debug` ("\nStarting new perf >>> \n"++ show backToPerfBal)
                    else
                      []
       
       balAfterDelinq = beginBal - delinqBal
       ppyAmt = mulBR balAfterDelinq ppyRate 
       balAfterPpy  = balAfterDelinq - ppyAmt
       periodRate = periodRateFromAnnualRate p rate
       (intAmt, prinAmt) = calcAssetPrinInt prinType balAfterPpy periodRate ot remainTerms

       endingBal = beginBal - prinAmt - ppyAmt - delinqBal -- `debug` ("DATE"++show pDate++">>>"++ show beginBal++">>"++show prinAmt ++ ">>" ++ show ppyAmt ++ ">>"++ show delinqBal)
       downFactor = divideBB beginBal endingBal
       newMbn = decreaseBorrowerNum beginBal endingBal mbn
       tr = CF.MortgageDelinqFlow pDate endingBal prinAmt intAmt ppyAmt delinqBal dBal rAmt lAmt rate newMbn Nothing Nothing-- `debug` ("Date"++ show pDate ++ "ENDING BAL AT"++ show endingBal)


projectScheduleFlow :: [CF.TsRow] -> Rate -> Balance -> [CF.TsRow] -> [DefaultRate] -> [PrepaymentRate] -> [Amount] -> [Amount] -> (Int, Rate) -> [CF.TsRow]
projectScheduleFlow trs _ last_bal [] _ _ [] [] (_,_) = trs 
projectScheduleFlow trs bal_factor last_bal (flow:flows) (defRate:defRates) (ppyRate:ppyRates) recV lossV (recoveryLag,recoveryRate)
  = projectScheduleFlow (trs++[tr]) surviveRate endBal flows defRates ppyRates (tail recVector) (tail lossVector) (recoveryLag,recoveryRate) -- `debug` ("===>C")
     where
       startBal = last_bal
       defAmt = mulBR startBal defRate
       ppyAmt = mulBR (startBal - defAmt) ppyRate 
       afterBal = startBal - defAmt - ppyAmt   
       
       surviveRate = (1 - defRate) * (1 - ppyRate) * bal_factor 
       schedulePrin = mulBR (CF.mflowPrincipal flow) surviveRate --TODO round trip  -- `debug` ("Schedule Principal"++(printf "%.2f" (CF.mflowPrincipal flow))++" Rate"++show(_schedule_rate))
       scheduleInt = mulBR (CF.mflowInterest flow) surviveRate

       newRec = mulBR defAmt recoveryRate
       newLoss = mulBR defAmt (1 - recoveryRate)

       recVector = replace recV recoveryLag newRec
       lossVector = replace lossV recoveryLag newLoss

       endBal = max 0 $ afterBal - schedulePrin

       tr = CF.MortgageFlow (CF.getDate flow) endBal schedulePrin scheduleInt ppyAmt defAmt (head recVector) (head lossVector) 0.0 Nothing Nothing Nothing--TODO missing ppy-penalty here

projectScheduleFlow trs b_factor lastBal [] _ _ (r:rs) (l:ls) (recovery_lag,recovery_rate)
  = projectScheduleFlow (trs++[tr]) b_factor lastBal [] [] [] rs ls (recovery_lag - 1,recovery_rate) 
   where
      remain_length = length rs
      lastDate = CF.getDate (last trs)
      flowDate = nextDate lastDate Lib.Monthly
      tr = CF.MortgageFlow flowDate lastBal 0 0 0 0 r l 0.0 Nothing Nothing Nothing

type DelinqRate = Rate
projectScheduleDelinqFlow :: ([CF.TsRow],[CF.TsRow]) -> Rate -> Balance -> [CF.TsRow] -> [DelinqRate] -> [PrepaymentRate] -> [Amount] -> [Amount] -> [Amount] -> (Rate,Int,Rate,Int) -> [CF.TsRow]
projectScheduleDelinqFlow (trs,[]) _ begBal flows [] [] defaults recoveries losses _ = 
  let 
    patchedFlows = [ CF.MortgageDelinqFlow d begBal prin int prepay delinq defVal recVal lossVal rate mB mPPN Nothing
                    | (CF.MortgageDelinqFlow d bal prin int prepay delinq _ _ _ rate mB mPPN Nothing,defVal,recVal,lossVal) <- zip4 flows defaults recoveries losses] -- `debug` ("Length of default"++ show defaults++">>recovery>>"++ show recoveries++">>loss>>"++ show losses)
    r1 = sort $ trs ++ patchedFlows -- `debug` ("Patched rows\n"++show patchedFlows)
  in 
    r1
    
projectScheduleDelinqFlow (trs,newPerfs) _ begBal flows [] [] defaults recoveries losses _ = 
  let 
    patchedFlows = [ CF.MortgageDelinqFlow d begBal prin int prepay delinq defVal recVal lossVal rate mB mPPN Nothing  
                    | (CF.MortgageDelinqFlow d bal prin int prepay delinq _ _ _ rate mB mPPN Nothing,defVal,recVal,lossVal) <- zip4 flows defaults recoveries losses] -- `debug` ("Length of default"++ show defaults++">>recovery>>"++ show recoveries++">>loss>>"++ show losses)
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
                                 Nothing Nothing -- `debug` ("|||>>> proj at date"++ show (CF.getDate flow))

-- | implementation on projection via default balance amount
projCashflowByDefaultAmt :: (Balance, Date, AmortPlan, Period,IRate,Maybe BorrowerNum) -> (Dates, ([Balance],[Balance]), [Rate], [IRate], [Int]) -> [CF.TsRow]
projCashflowByDefaultAmt (cb,lastPayDate,pt,p,cr,mbn) (cfDates,(expectedDefaultBals,unAppliedDefaultBals), ppyRates, rateVector, remainTerms) = 
  let 
    initRow = CF.MortgageFlow lastPayDate cb 0.0 0.0 0.0 0.0 0.0 0.0 cr mbn Nothing Nothing
  in 
    foldl
       (\acc (pDate, (defaultBal,futureDefualtBal), ppyRate, rate, rt)
         -> let 
             begBal = CF.mflowBalance (last acc)  
             mBorrower = CF.mflowBorrowerNum (last acc)   
             newDefault = if begBal <= (defaultBal+futureDefualtBal) then
                             begBal  
                           else
                             defaultBal   
             newPrepay = mulBR (max 0 (begBal - newDefault)) ppyRate  -- `debug` ("mb from last"++ show mBorrower) 
             newInt = mulBI (max 0 (begBal - newDefault - newPrepay)) (periodRateFromAnnualRate p rate)
             intBal = max 0 $ begBal - newDefault - newPrepay -- `debug` ("using rt"++ show rt)
             newPrin = case (rt,pt) of 
                         (0,_) -> intBal
                         (_,Level) -> let 
                                    pmt = calcPmt intBal (periodRateFromAnnualRate p rate) rt -- `debug` ("PMT with rt"++ show rt)
                                  in 
                                    pmt - newInt
                         (_,Even) -> intBal / fromIntegral rt
                         _ -> error ("Unsupport Prin type for mortgage"++ show pt)
             endBal = intBal - newPrin
             newMbn = decreaseBorrowerNum begBal endBal mBorrower  -- `debug` (">>> pdate"++ show pDate)
           in 
             acc ++ [CF.MortgageFlow pDate endBal newPrin newInt newPrepay newDefault 0.0 0.0 rate newMbn Nothing Nothing]                    
         )
       [initRow]
       (zip5 cfDates (zip expectedDefaultBals unAppliedDefaultBals) ppyRates rateVector remainTerms)

-- | implementation on projection via delinq balance amount
-- projCashflowByDelinqAmt :: (Balance, Date, AmortPlan, Period,IRate,Maybe BorrowerNum) -> (Dates, ([Balance],[Balance]), [Rate], [IRate], [Int]) -> [CF.TsRow]
-- projCashflowByDelinqAmt (cb,lastPayDate,pt,p,cr,mbn) (cfDates,(expectedDelinqBals,unAppliedDelinqBals), ppyRates, rateVector, remainTerms) = 
--   let 
--     initRow = CF.MortgageFlow lastPayDate cb 0.0 0.0 0.0 0.0 0.0 0.0 cr mbn Nothing Nothing
--   in 
--     foldl
--        (\acc (pDate, (defaultBal,futureDefualtBal), ppyRate, rate, rt)
--          -> let 
--              begBal = CF.mflowBalance (last acc)  
--              mBorrower = CF.mflowBorrowerNum (last acc)   
--              newDefault = if begBal <= (defaultBal+futureDefualtBal) then
--                              begBal  
--                            else
--                              defaultBal   
--              newPrepay = mulBR (max 0 (begBal - newDefault)) ppyRate  -- `debug` ("mb from last"++ show mBorrower) 
--              newInt = mulBI (max 0 (begBal - newDefault - newPrepay)) (periodRateFromAnnualRate p rate)
--              intBal = max 0 $ begBal - newDefault - newPrepay
--              newPrin = case pt of 
--                          Level -> let 
--                                      pmt = calcPmt intBal (periodRateFromAnnualRate p rate) rt -- `debug` ("PMT with rt"++ show rt)
--                                   in 
--                                     pmt - newInt
--                          Even -> intBal / fromIntegral rt
--                          _ -> error ("Unsupport Prin type for mortgage"++ show pt)
--              endBal = intBal - newPrin
--              newMbn = decreaseBorrowerNum begBal endBal mBorrower 
--            in 
--              acc ++ [CF.MortgageFlow pDate endBal newPrin newInt newPrepay newDefault 0.0 0.0
--                        rate newMbn Nothing Nothing]                    
--          )
--        [initRow]
--        (zip5 cfDates (zip expectedDelinqBals unAppliedDelinqBals) ppyRates rateVector remainTerms)




-- | implementation on projection via default balance amount
projScheduleCashflowByDefaultAmt :: (Balance, Date,IRate,Maybe BorrowerNum) -> ([CF.TsRow], ([Balance],[Balance]), [Rate] ) -> ([CF.TsRow], Rate)
projScheduleCashflowByDefaultAmt (cb,lastPayDate,cr,mbn) (scheduleFlows,(expectedDefaultBals,unAppliedDefaultBals), ppyRates) = 
  let 
    initRow = CF.MortgageFlow lastPayDate cb 0.0 0.0 0.0 0.0 0.0 0.0 cr mbn Nothing Nothing
  in 
    foldl
       (\(acc,factor) (cflow, (defaultBal,futureDefualtBal), ppyRate)
         -> let 
             pDate = getDate cflow
             
             begBal = CF.mflowBalance (last acc)  
             mBorrower = CF.mflowBorrowerNum (last acc)

             newDefault = if begBal <= (defaultBal+futureDefualtBal) then
                            begBal  
                          else
                            defaultBal   
             newPrepay = mulBR (max 0 (begBal - newDefault)) ppyRate  -- `debug` ("mb from last"++ show mBorrower) 
             
             intBal = max 0 $ begBal - newDefault - newPrepay
             defRate = if (begBal - newPrepay) /= 0 then 
                         divideBB newDefault (begBal - newPrepay)
                       else
                         0
             newFactor = (1 - ppyRate) * (1 - defRate) * factor
             newInt = mulBR (CF.mflowInterest cflow) newFactor
             newPrin = mulBR (CF.mflowPrincipal cflow) newFactor
             
             endBal = intBal - newPrin
             newMbn = decreaseBorrowerNum begBal endBal mBorrower 
           in 
             (acc ++ [CF.MortgageFlow pDate endBal newPrin newInt newPrepay newDefault 0.0 0.0
                       cr newMbn Nothing Nothing]                    
              ,newFactor)
         )
       ([initRow],1.0)
       (zip3 scheduleFlows (zip expectedDefaultBals unAppliedDefaultBals) ppyRates)

buildARMrates :: IR.RateType -> (ARM,Date,Date,Date,IRate) -> Maybe [RateAssumption] -> Ts
buildARMrates (IR.Fix _ _ ) _ _ = error "ARM should have floater rate"
buildARMrates or@(IR.Floater _ idx sprd initRate dp _ _ mRoundBy ) 
              (arm, startDate, firstResetDate, lastCfDate, beginRate) mRates
  = let 
      resetDates = genSerialDatesTill2 IE firstResetDate dp lastCfDate
      projectFutureActualCurve = runInterestRate2 arm (startDate,beginRate) or resetDates
    in 
      case A.getRateAssumption (fromMaybe [] mRates) idx of
        Just (RateCurve idx curve) 
          -> projectFutureActualCurve curve 
        Just (RateFlat idx v) 
          -> projectFutureActualCurve (mkRateTs [(startDate, v),(lastCfDate,v)]) -- `debug` ("lpd"++show last_pay_date++"lpd"++ show (last cf_dates))
        Nothing -> error $ "Failed to find index"++ show idx

instance Ast.Asset Mortgage where
  calcCashflow m@(Mortgage (MortgageOriginalInfo ob or ot p sd ptype _)  _bal _rate _term _mbn Current) d mRates
    = fst (projCashflow m d (MortgageAssump Nothing Nothing Nothing Nothing,A.DummyDelinqAssump,A.DummyDefaultAssump) mRates)


  calcCashflow s@(ScheduleMortgageFlow beg_date flows _)  d _ = CF.CashFlowFrame ( (CF.mflowBalance . head) flows, beg_date, Nothing ) flows
  calcCashflow m@(AdjustRateMortgage _origin _arm  _bal _rate _term _mbn _status) d mRates = error "to be implement on adjust rate mortgage"
  
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

  getPaymentDates (Mortgage (MortgageOriginalInfo _ _ ot p sd _ _) _ _ _ _ _) extra = genDates sd p (ot+extra)
  getPaymentDates (AdjustRateMortgage (MortgageOriginalInfo _ _ ot p sd _ _) _ _ _ _ _ _) extra = genDates sd p (ot+extra)
  getPaymentDates (ScheduleMortgageFlow begDate flows dp) extra 
    = 
      let 
        lastPayDay = (getDate . last) flows
        extDates =genSerialDates dp Exc lastPayDay extra 
      in 
        getDates flows ++ extDates

  isDefaulted (Mortgage _ _ _ _ _ (Defaulted _)) = True
  isDefaulted (AdjustRateMortgage _ _ _ _ _ _ (Defaulted _)) = True
  isDefaulted Mortgage {} = False
  isDefaulted AdjustRateMortgage {} = False
  
  getOriginDate (Mortgage (MortgageOriginalInfo _ _ ot p sd _ _) _ _ ct _ _) = sd
  getOriginDate (AdjustRateMortgage (MortgageOriginalInfo _ _ ot p sd _ _) _ _ _ ct _ _) = sd
  getOriginDate (ScheduleMortgageFlow begDate _ _) = begDate

  getRemainTerms (Mortgage (MortgageOriginalInfo _ _ ot p sd _ _) _ _ ct _ _) = ct
  getRemainTerms (AdjustRateMortgage (MortgageOriginalInfo _ _ ot p sd _ _) _ _ _ ct _ _) = ct

  getOriginInfo (Mortgage oi _ _ _ _ _) = oi
  getOriginInfo (AdjustRateMortgage oi _ _ _ _ _ _) = oi

  updateOriginDate (Mortgage (MortgageOriginalInfo ob or ot p sd _type mpn) cb cr ct mbn st) nd 
    = Mortgage (MortgageOriginalInfo ob or ot p nd _type mpn) cb cr ct mbn st 
  updateOriginDate (AdjustRateMortgage (MortgageOriginalInfo ob or ot p sd _type mpn) arm cb cr ct mbn st) nd 
    = AdjustRateMortgage (MortgageOriginalInfo ob or ot p nd _type mpn) arm cb cr ct mbn st

  -- project current mortgage with total default amt 
  projCashflow m@(Mortgage (MortgageOriginalInfo ob or ot p sd prinPayType mpn) cb cr rt mbn Current) 
               asOfDay 
               mars@(A.MortgageAssump (Just (A.DefaultByAmt (dBal,vs))) amp amr ams ,_ ,_) 
               mRates =
      (applyHaircut ams $ patchPrepayPenaltyFlow (ot,mpn) (CF.CashFlowFrame (cb,asOfDay,Nothing) futureTxns) ,historyM)
      where
        recoveryLag = maybe 0 getRecoveryLag amr
        lastPayDate:cfDates = lastN (succ (recoveryLag + rt)) $ sd:getPaymentDates m recoveryLag
        ppyRates = Ast.buildPrepayRates (lastPayDate:cfDates) amp
        rateVector = A.projRates cr or mRates cfDates 
        expectedDefaultBals = paddingDefault 0 (mulBR dBal <$> vs) (length cfDates)
        unAppliedDefaultBals = tail $ scanl (-) dBal expectedDefaultBals
        remainTerms = paddingDefault 0 (reverse [0..(length cfDates - recoveryLag)]) (length cfDates)
        txns = projCashflowByDefaultAmt (cb,lastPayDate,prinPayType,p,cr,mbn) 
                                        (cfDates,(expectedDefaultBals,unAppliedDefaultBals),ppyRates,rateVector,remainTerms)
        (futureTxns,historyM)= CF.cutoffTrs asOfDay (patchLossRecovery txns amr)
  
  -- project current adjMortgage with total default amt
  projCashflow m@(AdjustRateMortgage (MortgageOriginalInfo ob or ot p sd prinPayType mpn) arm cb cr rt mbn Current) 
               asOfDay 
               mars@(A.MortgageAssump (Just (A.DefaultByAmt (dBal,vs))) amp amr ams,_,_) 
               mRates =
      (applyHaircut ams $ patchPrepayPenaltyFlow (ot,mpn) (CF.CashFlowFrame (cb,asOfDay,Nothing) futureTxns) ,historyM)
      where
        ARM initPeriod initCap periodicCap lifeCap lifeFloor = arm
        passInitPeriod = (ot - rt) >= initPeriod 
        firstResetDate = monthsAfter sd (toInteger (succ initPeriod))

        lastPayDate:cfDates = sliceDates (SliceOnAfterKeepPrevious asOfDay)  $ lastN (rt + recoveryLag + 1) $ sd:getPaymentDates m recoveryLag 
        ppyRates = Ast.buildPrepayRates (lastPayDate:cfDates) amp
        rateCurve = buildARMrates or (arm, sd, firstResetDate, last cfDates, getOriginRate m) mRates
        rateVector = fromRational <$> getValByDates rateCurve Inc cfDates 
        expectedDefaultBals = paddingDefault 0 (mulBR dBal <$> vs) (length cfDates)
        unAppliedDefaultBals = tail $ scanl (-) dBal expectedDefaultBals
        recoveryLag = maybe 0 getRecoveryLag amr
        remainTerms = paddingDefault 0 (reverse [0..(length cfDates - recoveryLag)]) (length cfDates)
        txns = projCashflowByDefaultAmt (cb,lastPayDate,prinPayType,p,cr,mbn) (cfDates,(expectedDefaultBals,unAppliedDefaultBals),ppyRates,rateVector,remainTerms)
        (futureTxns,historyM)= CF.cutoffTrs asOfDay (patchLossRecovery txns amr)
  
  -- project schedule cashflow with total default amount
  projCashflow (ScheduleMortgageFlow begDate flows dp) asOfDay 
              assumps@(pAssump@(A.MortgageAssump (Just (A.DefaultByAmt (dBal,vs))) amp amr ams ),dAssump,fAssump) _
    = (applyHaircut ams (CF.CashFlowFrame (begBal,begDate,Nothing) futureTxns) ,historyM)  -- `debug` ("Future txn"++ show futureTxns)
      where
        begBal =  CF.mflowBegBalance $ head flows
        begDate = getDate $ head flows 
        begRate = CF.mflowRate $ head flows 
        begMbn = CF.mflowBorrowerNum $ head flows 
        originCfDates = CF.getDate <$> flows 
        originFlowSize = length flows
        recoveryLag = maybe 0 getRecoveryLag amr
        totalLength = recoveryLag + originFlowSize
        ppyRates = paddingDefault 0.0 (Ast.buildPrepayRates (begDate:originCfDates) amp) totalLength
        expectedDefaultBals = paddingDefault 0 (mulBR dBal <$> vs) totalLength
        unAppliedDefaultBals = tail $ scanl (-) dBal expectedDefaultBals
        -- (ppyRates,defRates,recoveryRate,recoveryLag) = buildAssumptionPpyDefRecRate (begDate:cfDates) pAssump 
        endDate = (CF.getDate . last) flows
        extraDates = genSerialDates dp Exc endDate recoveryLag
        -- cfDates = (CF.getDate <$> flows) ++ extraDates
        flowsWithEx = flows ++ extendTxns (last flows) extraDates -- `debug` (">> end date"++ show endDate++">>> extra dates"++show extraDates)
        (txns,_) = projScheduleCashflowByDefaultAmt 
                     (begBal,begDate,begRate,begMbn) 
                     (flowsWithEx,(expectedDefaultBals,unAppliedDefaultBals),ppyRates) -- `debug` ("exted flows"++ show flowsWithEx)
        (futureTxns,historyM) = CF.cutoffTrs asOfDay (patchLossRecovery txns amr) -- `debug` ("txn"++show txns)


  -- project current mortgage(without delinq)
  projCashflow m@(Mortgage (MortgageOriginalInfo ob or ot p sd prinPayType mpn) cb cr rt mbn Current) 
               asOfDay 
               mars@(A.MortgageAssump amd amp amr ams ,_ ,_) 
               mRates =
      (applyHaircut ams $ patchPrepayPenaltyFlow (ot,mpn) (CF.CashFlowFrame (cb,asOfDay,Nothing) futureTxns) ,historyM)
    where
      recoveryLag = maybe 0 getRecoveryLag amr
      lastPayDate:cfDates = lastN (rt + 1) $ sd:getPaymentDates m 0
      
      defRates = Ast.buildDefaultRates (lastPayDate:cfDates) amd
      ppyRates = Ast.buildPrepayRates (lastPayDate:cfDates) amp
      
      cfDatesLength = length cfDates 
      
      rateVector = A.projRates cr or mRates cfDates 
      
      remainTerms = reverse [0..rt]
      dc = getDayCount or
      txns = projectMortgageFlow (cb,lastPayDate,mbn,prinPayType,dc,cr,p,ot) (cfDates, defRates, ppyRates,rateVector,remainTerms)

      recoveryDates = lastN recoveryLag $ sd:getPaymentDates m recoveryLag
      lastProjTxn = last txns
      extraTxns = [ CF.emptyTsRow d lastProjTxn  | d <- recoveryDates ]
      
      (futureTxns,historyM)= CF.cutoffTrs asOfDay (patchLossRecovery (txns++extraTxns) amr)

  -- project current mortgage(with delinq)
  projCashflow m@(Mortgage (MortgageOriginalInfo ob or ot p sd prinPayType mpn) cb cr rt mbn Current) 
               asOfDay 
               mars@(A.MortgageDeqAssump amd amp amr ams
                    ,_
                    ,_) 
               mRates =
      (applyHaircut ams $ patchPrepayPenaltyFlow (ot,mpn) (CF.CashFlowFrame (cb,asOfDay, Nothing) futureTxns) ,historyM)
    where
      lastPayDate:cfDates = lastN (recoveryLag + defaultLag + rt + 1) $ sd:getPaymentDates m (recoveryLag+defaultLag)
      cfDatesLength = length cfDates + recoveryLag + defaultLag

      rateVector = A.projRates cr or mRates cfDates

      (ppyRates,delinqRates,(defaultPct,defaultLag),recoveryRate,recoveryLag) = Ast.buildAssumptionPpyDelinqDefRecRate (lastPayDate:cfDates) (A.MortgageDeqAssump amd amp amr ams)
      
      txns = projectDelinqMortgageFlow ([],[]) cb mbn lastPayDate cfDates delinqRates ppyRates rateVector 
                                       (defaultPct,defaultLag,recoveryRate,recoveryLag,p,prinPayType,ot) 
                                       (replicate cfDatesLength 0.0,replicate cfDatesLength 0.0,replicate cfDatesLength 0.0)
      (futureTxns,historyM)= CF.cutoffTrs asOfDay txns 
  -- project defaulted Mortgage    
  projCashflow m@(Mortgage (MortgageOriginalInfo ob or ot p sd prinPayType mpn) cb cr rt mbn (Defaulted (Just defaultedDate)) ) 
               asOfDay
               (_,_,A.DefaultedRecovery rr lag timing) _ =
    let 
      (emptyDates,recoveryDates) = splitAt (pred lag) $ genDates defaultedDate p (lag + length timing)
      beforeRecoveryTxn = [ CF.MortgageFlow d 0 0 0 0 0 0 0 cr mbn Nothing Nothing | d <- emptyDates ]
      recoveries = calcRecoveriesFromDefault cb rr timing
      txns = [ CF.MortgageFlow d 0 0 0 0 0 r 0 cr mbn Nothing Nothing | (d,r) <- zip recoveryDates recoveries ]
    in 
      (CF.CashFlowFrame (cb,asOfDay,Nothing) $ cutBy Inc Future asOfDay (beforeRecoveryTxn ++ txns) ,Map.empty)

  -- project defaulted adjMortgage with a defaulted Date   
  projCashflow m@(AdjustRateMortgage mo arm cb cr rt mbn (Defaulted (Just defaultedDate)) ) asOfDay assumps mRates
    = projCashflow (Mortgage mo cb cr rt mbn  (Defaulted (Just defaultedDate))) asOfDay assumps mRates
  -- project defaulted adjMortgage without a defaulted Date   
  projCashflow m@(AdjustRateMortgage _ _ cb cr rt mbn (Defaulted Nothing) ) asOfDay assumps _
    = (CF.CashFlowFrame (cb,asOfDay,Nothing) [ CF.MortgageFlow asOfDay 0 0 0 0 0 0 0 cr mbn Nothing Nothing] ,Map.empty)
  -- project defaulted Mortgage    
  projCashflow m@(Mortgage _ cb cr rt mbn (Defaulted Nothing) ) asOfDay assumps _
    = (CF.CashFlowFrame (cb,asOfDay,Nothing) [ CF.MortgageFlow asOfDay 0 0 0 0 0 0 0 cr mbn Nothing Nothing] ,Map.empty)

  -- project current AdjMortgage
  projCashflow m@(AdjustRateMortgage (MortgageOriginalInfo ob or ot p sd prinPayType mpn) arm cb cr rt mbn Current) 
               asOfDay 
               mars@(A.MortgageAssump amd amp amr ams,_,_) 
               mRates =
    (applyHaircut ams $ patchPrepayPenaltyFlow (ot,mpn) (CF.CashFlowFrame (cb,asOfDay,Nothing) futureTxns) ,historyM)
    where
      ARM initPeriod initCap periodicCap lifeCap lifeFloor = arm
      passInitPeriod = (ot - rt) >= initPeriod 
      firstResetDate = monthsAfter sd (toInteger (succ initPeriod))

      lastPayDate:cfDates = sliceDates (SliceOnAfterKeepPrevious asOfDay)  $ lastN (rt + recoveryLag + 1) $ sd:getPaymentDates m recoveryLag 
      
      cfDatesLength = length cfDates -- `debug` (" cf dates >>" ++ show (last_pay_date:cf_dates ))
      rateCurve = buildARMrates or (arm, sd, firstResetDate, last cfDates, getOriginRate m) mRates
      rateVector = fromRational <$> getValByDates rateCurve Inc cfDates -- `debug` ("RateCurve"++ show rate_curve)

      (ppyRates,defRates,recoveryRate,recoveryLag) = buildAssumptionPpyDefRecRate (lastPayDate:cfDates) (A.MortgageAssump amd amp amr ams)
      -- txns = projectMortgageFlow [] cb  mbn lastPayDate cfDates defRates ppyRates (replicate cfDatesLength 0.0) (replicate cfDatesLength 0.0) rateVector (recoveryLag,recoveryRate) p prinPayType 
      remainTerms = reverse $ replicate recoveryLag 0 ++ [0..rt]
      dc = getDayCount or
      txns = projectMortgageFlow (cb,lastPayDate,mbn,prinPayType,dc,cr,p,ot) (cfDates, defRates, ppyRates,rateVector,remainTerms)
      (futureTxns,historyM)= CF.cutoffTrs asOfDay (patchLossRecovery txns amr)

  -- project current AdjMortgage with delinq
  projCashflow m@(AdjustRateMortgage (MortgageOriginalInfo ob or ot p sd prinPayType mpn) arm cb cr rt mbn Current) 
               asOfDay 
               mars@(A.MortgageAssump amd amp amr ams,_,_) 
               mRates =
      (applyHaircut ams $ patchPrepayPenaltyFlow (ot,mpn) (CF.CashFlowFrame (cb,asOfDay,Nothing) futureTxns) ,historyM)
    where
      ARM initPeriod initCap periodicCap lifeCap lifeFloor = arm
      passInitPeriod = (ot - rt) >= initPeriod 
      firstResetDate = monthsAfter sd (toInteger (succ initPeriod))
      lastPayDate:cfDates = lastN (recoveryLag + defaultLag + rt + 1) $ sd:getPaymentDates m recoveryLag  
      cfDatesLength = length cfDates 
      rateCurve = buildARMrates or (arm, sd, firstResetDate, last cfDates, getOriginRate m) mRates
      rateVector = fromRational <$> getValByDates rateCurve Inc cfDates -- `debug` ("RateCurve"++ show rate_curve)                                  

      (ppyRates, delinqRates,(defaultPct,defaultLag),recoveryRate,recoveryLag) = Ast.buildAssumptionPpyDelinqDefRecRate (lastPayDate:cfDates) (A.MortgageAssump amd amp amr ams)
      
      txns = projectDelinqMortgageFlow ([],[]) cb mbn lastPayDate cfDates delinqRates ppyRates rateVector 
                                       (defaultPct,defaultLag,recoveryRate,recoveryLag,p,prinPayType,ot) 
                                       (replicate cfDatesLength 0.0,replicate cfDatesLength 0.0,replicate cfDatesLength 0.0)
      (futureTxns,historyM)= CF.cutoffTrs asOfDay txns 
  -- schedule mortgage flow without delinq
  projCashflow (ScheduleMortgageFlow begDate flows dp) asOfDay 
               assumps@(pAssump@(A.MortgageAssump _ _ _ ams ),dAssump,fAssump) _
    = (applyHaircut ams (CF.CashFlowFrame (begBal,asOfDay,Nothing) futureTxns) ,historyM)
      where
        begBal =  CF.mflowBegBalance $ head flows 
        (ppyRates,defRates,recoveryRate,recoveryLag) = buildAssumptionPpyDefRecRate (begDate:cfDates) pAssump 
        curveDatesLength =  recoveryLag + length flows
        extraPeriods = recoveryLag
        endDate = CF.getDate (last flows)
        extraDates = genSerialDates dp Exc endDate extraPeriods
        cfDates = (CF.getDate <$> flows) ++ extraDates
        txns = projectScheduleFlow [] 1.0 begBal flows defRates ppyRates
                                   (replicate curveDatesLength 0.0)
                                   (replicate curveDatesLength 0.0)
                                   (recoveryLag,recoveryRate) 
        (futureTxns,historyM) = CF.cutoffTrs asOfDay txns 
        cb = (CF.mflowBegBalance . head) futureTxns

  
  -- schedule mortgage flow WITH delinq
  projCashflow (ScheduleMortgageFlow begDate flows dp) asOfDay assumps@(pAssump@(A.MortgageDeqAssump _ _ _ ams),dAssump,fAssump) mRates
    = 
      (applyHaircut ams (CF.CashFlowFrame (begBal, asOfDay,Nothing) futureTxns) ,historyM)
      where
        begBal =  CF.mflowBegBalance $ head flows -- `debug` ("beg date"++show beg_date)
        (ppyRates, delinqRates,(defaultPct,defaultLag),recoveryRate,recoveryLag) = Ast.buildAssumptionPpyDelinqDefRecRate (begDate:getDates flows) pAssump
        curveDatesLength = defaultLag + recoveryLag + length flows -- `debug` ("Length of rates"++show (length delinqRates)++">>"++show (length ppyRates))
        extraPeriods = defaultLag + recoveryLag -- `debug` ("lags "++show defaultLag++">>"++show recoveryLag)
        endDate = CF.getDate (last flows) 
        extraDates = genSerialDates dp Exc endDate extraPeriods
        extraFlows = [ CF.emptyTsRow d r | (d,r) <- zip extraDates (replicate extraPeriods (last flows)) ] 
        flowWithExtraDates = flows ++ extraFlows
        cfDates = getDates flowWithExtraDates -- `debug` ("CF dates"++ show flowWithExtraDates)
        txns = projectScheduleDelinqFlow ([],[]) 1.0 begBal flowWithExtraDates delinqRates ppyRates
                 (replicate curveDatesLength 0.0) (replicate curveDatesLength 0.0)
                 (replicate curveDatesLength 0.0) (defaultPct,defaultLag,recoveryRate,recoveryLag)  -- `debug` ("Delinq rates"++ show delinqRates++">>ppy rates"++ show ppyRates)
        (futureTxns,historyM) = CF.cutoffTrs asOfDay txns 
  
  projCashflow a b c d = error $ "Failed to match when proj mortgage>>" ++ show a ++ show b ++ show c ++ show d

  getBorrowerNum m@(Mortgage (MortgageOriginalInfo ob or ot p sd prinPayType _) cb cr rt mbn _ ) = fromMaybe 1 mbn
  getBorrowerNum m@(AdjustRateMortgage (MortgageOriginalInfo ob or ot p sd prinPayType _) _ cb cr rt mbn _ ) = fromMaybe 1 mbn

  splitWith (Mortgage (MortgageOriginalInfo ob or ot p sd prinPayType mpn) cb cr rt mbn st ) rs 
    = [ Mortgage (MortgageOriginalInfo (mulBR ob ratio) or ot p sd prinPayType mpn) (mulBR cb ratio) cr rt mbn st 
       | ratio <- rs ]
  
  splitWith (AdjustRateMortgage (MortgageOriginalInfo ob or ot p sd prinPayType mpn) arm cb cr rt mbn st ) rs 
    = [ AdjustRateMortgage (MortgageOriginalInfo (mulBR ob ratio) or ot p sd prinPayType mpn) arm (mulBR cb ratio) cr rt mbn st 
       | ratio <- rs ]
  

