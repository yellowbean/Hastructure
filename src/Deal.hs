{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
-- {-# LANGUAGE OverloadedRecordDot, OverloadedRecordUpdate #-}
-- {-# LANGUAGE OverloadedRecordDot #-}

module Deal (run,runPool,getInits,runDeal,ExpectReturn(..)
            ,performAction,queryDeal
            ,populateDealDates,accrueRC
            ,calcTargetAmount,updateLiqProvider
            ,projAssetUnion,priceAssetUnion
            ,removePoolCf,setFutureCF,runPoolType,PoolType
            ,ActionOnDate(..),DateDesp(..),OverrideType(..)
            ) where

import qualified Accounts as A
import qualified Ledger as LD
import qualified Asset as Ast
import qualified Pool as P
import qualified Expense as F
import qualified Liability as L
import qualified CreditEnhancement as CE
import qualified Waterfall as W
import qualified Cashflow as CF
import qualified Assumptions as AP
import qualified Reports as Rpt
import qualified AssetClass.AssetBase as ACM
import AssetClass.Mortgage
import AssetClass.Lease
import AssetClass.Loan
import AssetClass.Installment

import AssetClass.MixedAsset

import qualified Call as C
import qualified InterestRate as IR
import Deal.DealBase
import Deal.DealQuery
import Deal.DealAction
import qualified Deal.DealValidation as V
import Stmt
import Lib
import Util
import DateUtil
import Types
import Revolving
import Triggers

import qualified Data.Map as Map
import qualified Data.Time as T
import qualified Data.Set as S
import qualified Control.Lens as LS
import Data.List
import Data.Fixed
import Data.Time.Clock
import Data.Maybe
import Data.Either
import Data.Aeson hiding (json)
import qualified Data.Aeson.Encode.Pretty as Pretty
import Language.Haskell.TH
import Data.Aeson.TH
import Data.Aeson.Types
import GHC.Generics

import Debug.Trace
import Cashflow (buildBegTsRow)
import Assumptions (NonPerfAssumption(NonPerfAssumption),lookupRate0)
import Asset ()
import Pool (issuanceStat)
import qualified Types as P
import Control.Lens hiding (element)
import Control.Lens.TH
import InterestRate (calcInt)
import Liability (getDayCountFromInfo)
import Hedge (RateCap(..),RateSwapBase(..),RateSwap(rsRefBalance))
import qualified Hedge as HE

debug = flip trace

-- ^ update bond interest rate from rate assumption
setBondNewRate :: Ast.Asset a => TestDeal a -> Date -> [RateAssumption] -> L.Bond -> L.Bond
setBondNewRate t d ras b@(L.Bond _ _ _ ii (Just (L.PassDateSpread _ spd)) bal currentRate _ dueInt _ (Just dueIntDate) _ _ _)
  = b { L.bndRate = currentRate + spd, L.bndDueInt = dueInt + accrueInt, L.bndDueIntDate = Just d}
    where 
      (Just dc) = getDayCountFromInfo ii
      accrueInt = calcInt (bal + dueInt) dueIntDate d currentRate dc

setBondNewRate t d ras b@(L.Bond _ _ _ ii (Just (L.PassDateLadderSpread _ spd _)) bal currentRate _ dueInt _ (Just dueIntDate) _ _ _)
  = b { L.bndRate = currentRate + spd, L.bndDueInt = dueInt + accrueInt, L.bndDueIntDate = Just d}
    where 
      (Just dc) = getDayCountFromInfo ii
      accrueInt = calcInt (bal + dueInt) dueIntDate d currentRate dc

setBondNewRate t d ras b@(L.Bond _ _ _ (L.RefRate sr ds factor _) _ _ _ _ _ _ _ _ _ _) 
  = let 
      rate = queryDealRate t (patchDateToStats d ds)
    in 
      b {L.bndRate = fromRational (toRational rate * toRational factor) }

setBondNewRate t d ras b@(L.Bond _ _ _ ii _ _ _ _ _ _ _ _ _ _) 
  = b { L.bndRate = applyFloatRate ii d ras }

setBondNewRate t d ras bg@(L.BondGroup bMap)
  = L.BondGroup $ Map.map (setBondNewRate t d ras) bMap


updateSrtRate :: Ast.Asset a => TestDeal a -> Date -> [RateAssumption] -> HE.SRT -> HE.SRT
updateSrtRate t d ras srt@HE.SRT{HE.srtPremiumType = rt} 
    = srt { HE.srtPremiumRate = applyFloatRate2 rt d ras }


accrueSrt :: Ast.Asset a => TestDeal a -> Date -> HE.SRT -> HE.SRT
accrueSrt t d srt@HE.SRT{ HE.srtDuePremium = duePrem, HE.srtRefBalance = bal, HE.srtPremiumRate = rate
                        , HE.srtDuePremiumDate = mDueDate,  HE.srtType = st
                        , HE.srtStart = sd } 
  = srt { HE.srtRefBalance = newBal, HE.srtDuePremium = newPremium, HE.srtDuePremiumDate = Just d}
  where 
    newBal = case st of
               HE.SrtByEndDay ds dp -> queryDeal t (patchDateToStats d ds)
               _ -> error "not support new bal type for Srt"
    newPremium = duePrem +  calcInt newBal (fromMaybe sd mDueDate) d rate DC_ACT_365F
    accrueInt = calcInt (HE.srtRefBalance srt + duePrem) (fromMaybe d (HE.srtDuePremiumDate srt)) d (HE.srtPremiumRate srt) DC_ACT_365F
    dueInt = HE.srtDuePremium 


updateLiqProviderRate :: Ast.Asset a => TestDeal a -> Date -> [RateAssumption] -> CE.LiqFacility -> CE.LiqFacility
updateLiqProviderRate t d ras liq@CE.LiqFacility{CE.liqRateType = mRt, CE.liqPremiumRateType = mPrt
                                               , CE.liqRate = mr, CE.liqPremiumRate = mPr }
  = let 
      newMr =  evalFloaterRate d ras <$> mRt
      newMpr = evalFloaterRate d ras <$> mPrt
      -- TODO probably need to accure int when interest rate changes ? 
    in 
      liq {CE.liqRate = newMr, CE.liqPremiumRate = newMpr }

updateLiqProviderRate t d ras liq = liq 

evalFloaterRate :: Date -> [RateAssumption] -> IR.RateType -> IRate 
evalFloaterRate _ _ (IR.Fix _ r) = r 
evalFloaterRate d ras (IR.Floater _ idx spd _r _ mFloor mCap mRounding)
  = let 
      ra = AP.getRateAssumption ras idx 
      flooring (Just f) v = max f v 
      flooring Nothing v = v 
      capping (Just f) v = min f v 
      capping Nothing  v = v 
    in 
      case ra of 
        Nothing -> error "Failed to find index rate in assumption"
        Just (RateFlat _ v) -> capping mCap $ flooring mFloor $ v + spd 
        Just (RateCurve _ curve) -> capping mCap $ flooring mFloor $ fromRational $ getValByDate curve Inc d + toRational spd

applyFloatRate :: L.InterestInfo -> Date -> [RateAssumption] -> IRate
applyFloatRate (L.Floater _ idx spd p dc mf mc) d ras
  = case (mf,mc) of
      (Nothing,Nothing) -> _rate
      (Just f,Nothing) -> max f _rate
      (Just f,Just c) -> min c $ max f _rate
      (Nothing,Just c) -> min c _rate
    where
      idx_rate = case ra of 
        Just (RateCurve _idx _ts) -> fromRational $ getValByDate _ts Exc d
        Just (RateFlat _idx _r) ->   _r
        Nothing -> 0.0
      ra = AP.getRateAssumption ras idx
      _rate = idx_rate + spd -- `debug` ("idx"++show idx_rate++"spd"++show spd)

applyFloatRate (L.CapRate ii _rate) d ras = min _rate (applyFloatRate ii d ras)
applyFloatRate (L.FloorRate ii _rate) d ras = max _rate (applyFloatRate ii d ras)
applyFloatRate (L.Fix r _ ) d ras = r

applyFloatRate2 :: IR.RateType -> Date -> [RateAssumption] -> IRate
applyFloatRate2 (IR.Fix _ r) _ _ = r
applyFloatRate2 (IR.Floater _ idx spd _r _ mFloor mCap mRounding) d ras
  = let 
      rateAtDate = AP.lookupRate0 ras idx d 
      flooring (Just f) v = max f v 
      flooring Nothing v = v 
      capping (Just f) v = min f v 
      capping Nothing  v = v 
    in 
      flooring mFloor $ capping mCap $ rateAtDate + spd

updateRateSwapRate :: [RateAssumption] -> Date -> HE.RateSwap -> HE.RateSwap
updateRateSwapRate rAssumps d rs@HE.RateSwap{ HE.rsType = rt } 
  = rs {HE.rsPayingRate = pRate, HE.rsReceivingRate = rRate }
  where 
      (pRate,rRate) = case rt of 
                     HE.FloatingToFloating flter1 flter2 -> (getRate flter1,getRate flter2)
                     HE.FloatingToFixed flter r -> (getRate flter, r)
                     HE.FixedToFloating r flter -> (r , getRate flter)
      getRate x = AP.lookupRate rAssumps x d

updateRateSwapBal :: Ast.Asset a => TestDeal a -> Date -> HE.RateSwap -> HE.RateSwap
updateRateSwapBal t d rs@HE.RateSwap{ HE.rsNotional = base }
  =  case base of 
       HE.Fixed _ -> rs  
       HE.Base ds -> rs { HE.rsRefBalance = queryDeal t (patchDateToStats d ds) } -- `debug` ("query Result"++ show (patchDateToStats d ds) )
       HE.Schedule ts -> rs { HE.rsRefBalance = fromRational (getValByDate ts Inc d) }

-- ^ accure rate cap 
accrueRC :: Ast.Asset a => TestDeal a -> Date -> [RateAssumption] -> RateCap -> RateCap
accrueRC t d rs rc@RateCap{rcNetCash = amt, rcStrikeRate = strike,rcIndex = index
                       ,rcStartDate = sd, rcEndDate = ed, rcNotional = notional
                       ,rcLastStlDate = mlsd
                       ,rcStmt = mstmt} 
  | d > ed || d < sd = rc 
  | otherwise = rc { rcLastStlDate = Just d ,rcNetCash = newAmt,
                     rcStmt = newStmt }
                where 
                  r = lookupRate0 rs index d
                  balance = case notional of
                              Fixed bal -> bal
                              Base ds -> queryDeal t (patchDateToStats d ds)
                              Schedule ts -> fromRational $ getValByDate ts Inc d

                  accRate = max 0 $ r - fromRational (getValByDate strike Inc d) -- `debug` ("Rate from curve"++show (getValByDate strike Inc d))
                  addAmt = case mlsd of 
                             Nothing -> calcInt balance sd d accRate DC_ACT_365F
                             Just lstD -> calcInt balance lstD d accRate DC_ACT_365F

                  newAmt = amt + addAmt  -- `debug` ("Accrue AMT"++ show addAmt)
                  newStmt = appendStmt mstmt $ IrsTxn d newAmt addAmt 0 0 0 SwapAccrue

-- ^ test if a clean up call should be fired
testCall :: Ast.Asset a => TestDeal a -> Date -> C.CallOption -> Bool 
testCall t d opt = 
    case opt of 
       C.PoolBalance x -> queryDeal t (FutureCurrentPoolBalance Nothing) < x
       C.BondBalance x -> queryDeal t CurrentBondBalance < x
       C.PoolFactor x ->  queryDealRate t (FutureCurrentPoolFactor d Nothing) < fromRational x -- `debug` ("D "++show d++ "Pool Factor query ->" ++ show (queryDealRate t (FutureCurrentPoolFactor d)))
       C.BondFactor x ->  queryDealRate t BondFactor < fromRational x
       C.OnDate x -> x == d 
       C.AfterDate x -> d > x
       C.And xs -> all (testCall t d) xs
       C.Or xs -> any (testCall t d) xs
       C.Pre pre -> testPre d t pre
       _ -> error ("failed to find call options"++ show opt)

-- ^ if any of the call options are satisfied
testCalls :: Ast.Asset a => TestDeal a -> Date -> [C.CallOption] -> Bool
testCalls t d [] = False  
testCalls t d opts = any (testCall t d) opts  


queryTrigger :: Ast.Asset a => TestDeal a -> DealCycle -> [Trigger]
queryTrigger t@TestDeal{ triggers = trgs } wt 
  = case trgs of 
      Nothing -> []
      Just _trgs -> maybe [] Map.elems $ Map.lookup wt _trgs

-- ^ execute effects of trigger: making changes to deal
runEffects :: Ast.Asset a => (TestDeal a, RunContext a, [ActionOnDate]) -> Date -> TriggerEffect -> (TestDeal a, RunContext a, [ActionOnDate])
runEffects (t@TestDeal{accounts = accMap, fees = feeMap ,status=st, bonds = bondMap, pool=pt
                      ,collects = collRules},rc, actions) d te 
  = case te of 
      DealStatusTo _ds -> (t {status = _ds}, rc, actions)
      DoAccrueFee fns -> (t {fees = foldr (Map.adjust (calcDueFee t d)) feeMap fns}, rc, actions)
      ChangeReserveBalance accName rAmt ->
          (t {accounts = Map.adjust (A.updateReserveBalance rAmt) accName accMap }, rc, actions)
      
      TriggerEffects efs -> foldl (`runEffects` d) (t,rc, actions) efs
      
      RunActions wActions -> let 
                              (newT,newRc,newLogs) = foldl (performActionWrap d) (t, rc, []) wActions
                            in 
                              (newT,newRc, actions)
      CloseDeal (offset0,pDp) (offset1,bDp) (pm,accName,mIssuanceBal) mCollectRules
        -> let 
            closingDate = d
            (WarehousingDates _ _ _ endDate) = dates t
            Warehousing nextSt = st
            -- issue bonds 
            newBonds = Map.map (L.setBondOrigDate closingDate) bondMap
            --- TODO for floater rate bond ,need to update rate in the bond 
            draftBondBals = queryDeal t CurrentBondBalance

            ---- determine the balance of issuance
            totalIssuanceBalance = case mIssuanceBal of 
                                    Nothing -> draftBondBals
                                    Just fml -> queryDeal t fml
            scaleFactor = toRational $ totalIssuanceBalance / draftBondBals
            scaledBndMap = Map.map (L.scaleBond scaleFactor) newBonds

            accAfterIssue = Map.adjust (A.deposit totalIssuanceBalance d (IssuanceProceeds "ALL")) accName accMap
            -- sell assets
            ----- valuation on pools
            assetVal = case pt of 
                        SoloPool p -> P.calcLiquidationAmount pm p closingDate 
                        MultiPool pMap -> sum $ Map.map (\p -> P.calcLiquidationAmount pm p closingDate) pMap
            assetBal = queryDeal t (FutureCurrentPoolBalance Nothing)
            accAfterBought = Map.adjust (A.draw assetVal d (PurchaseAsset "ALL" assetBal)) accName accMap

            -- reset pool flow flow
            dealPoolFlowMap = Map.map (maybe 0 ((CF.mflowBalance . head) . (view CF.cashflowTxn)))
                                      $ view dealCashflow t
            newPt = patchIssuanceBalance st dealPoolFlowMap pt 
            ---- reset pool stats
            newPt2 = case newPt of 
                      SoloPool _pt -> SoloPool $ over (P.poolFutureCf2 . CF.cashflowTxn) (CF.patchCumulative (0,0,0,0,0,0) []) _pt
                      MultiPool pm -> MultiPool $ Map.map (over (P.poolFutureCf2 . CF.cashflowTxn) (CF.patchCumulative (0,0,0,0,0,0) [])) pm
                      x -> x
            -- build actions dates
            firstPayDate = T.addDays (toInteger offset0) closingDate
            firstCollectDate = T.addDays (toInteger offset1) closingDate

            distributionDays = [ RunWaterfall _d "" | _d <- genSerialDatesTill2 IE firstPayDate bDp endDate]
            poolCollectionDays = [ PoolCollection _d "" | _d <- genSerialDatesTill2 IE firstCollectDate pDp endDate]
            newActions = (DealClosed closingDate):(sortBy sortActionOnDate (distributionDays++poolCollectionDays))
          in 
            (t {status = fromMaybe Amortizing nextSt, bonds = scaledBndMap, accounts=accAfterBought, pool = newPt2
                ,collects = fromMaybe collRules mCollectRules}
            , rc
            , cutBy Inc Past d actions ++ newActions) --TODO add actions to close deal
      DoNothing -> (t, rc, actions)
      _ -> error $ "Failed to match trigger effects: "++show te

-- ^ test triggers in the deal and add a log if deal status changed
runTriggers :: Ast.Asset a => (TestDeal a, RunContext a, [ActionOnDate]) -> Date -> DealCycle -> (TestDeal a, RunContext a, [ActionOnDate],[ResultComponent])
runTriggers (t@TestDeal{status=oldStatus, triggers = Nothing},rc, actions) d dcycle = (t, rc, actions, [])
runTriggers (t@TestDeal{status=oldStatus, triggers = Just trgM},rc, actions) d dcycle = 
    (newDeal {triggers = Just (Map.insert dcycle newTriggers trgM)}, newRc, newActions, newLogs) -- `debug` ("New logs from trigger"++ show d ++">>>"++show newLogs)
  where 
    -- _trgs = Map.findWithDefault [] dcycle trgM

    -- get triggeres to run at `dealCycle`
    trgsMap = Map.findWithDefault Map.empty dcycle trgM

    -- get a map of triggers to test
    trgsToTest = Map.filter   
                          (\trg -> (not (trgStatus trg) || trgStatus trg && trgCurable trg))
                          trgsMap
    -- test triggers 
    triggeredTrgs = Map.map (testTrigger t d) trgsToTest

    -- extract trigger effects to run, if the trigger is true                   
    triggeredEffects = [ trgEffects _trg | _trg <- Map.elems triggeredTrgs, (trgStatus _trg) ] 

    -- run effects on deals
    -- aka (\_t _te -> runEffects _t d _te)
    (newDeal, newRc,newActions) = foldl (`runEffects` d) (t,rc,actions) triggeredEffects

    -- if deal status changed, then insert to log if changes
    newStatus = status newDeal 
    newLogs = [DealStatusChangeTo d oldStatus newStatus |  newStatus /= oldStatus] -- `debug` (">>"++show d++"trigger : new st"++ show newStatus++"old st"++show oldStatus)

    -- new status of trigger, update status of trigger to True
    triggeredNames = Map.keys triggeredTrgs

    newTriggers = Map.union triggeredTrgs trgsMap
  
 
run :: Ast.Asset a => TestDeal a -> Map.Map PoolId CF.CashFlowFrame -> Maybe [ActionOnDate] -> Maybe [RateAssumption] -> Maybe [C.CallOption] 
        -> Maybe (Map.Map String (RevolvingPool,AP.ApplyAssumptionType))-> [ResultComponent] -> (TestDeal a,[ResultComponent])
run t@TestDeal{status=Ended} pCfM ads _ _ _ log  = (prepareDeal t,log++[EndRun Nothing "By Status:Ended"])
run t pCfM (Just []) _ _ _ log  = (prepareDeal t,log++[EndRun Nothing "No Actions"])
run t pCfM (Just [HitStatedMaturity d]) _ _ _ log  = (prepareDeal t,log++[EndRun (Just d) "Stop: Stated Maturity"])
run t pCfM (Just (StopRunFlag d:_)) _ _ _ log  = (prepareDeal t,log++[EndRun (Just d) "Stop Run Flag"])
run t@TestDeal{accounts=accMap,fees=feeMap,triggers=mTrgMap,bonds=bndMap,status=dStatus,waterfall=waterfallM,name=dealName,pool=pt} 
    poolFlowMap (Just (ad:ads)) rates calls rAssump log
  | all (== 0) futureCashToCollect && (queryDeal t AllAccBalance == 0) && (dStatus /= Revolving)
     = let 
        -- finalDeal = foldl (performAction (getDate ad)) t cleanUpActions 
        runContext = RunContext poolFlowMap rAssump rates
        (finalDeal,_,newLogs) = foldl (performActionWrap (getDate ad)) (t,runContext,log) cleanUpActions 
       in 
        (prepareDeal finalDeal,newLogs++[EndRun (Just (getDate ad)) "No Pool Cashflow/All Account is zero/Not revolving"]) -- `debug` ("End of pool collection with logs with length "++ show (length log))

  | otherwise
     = case ad of 
         PoolCollection d _ ->
           if any (> 0) remainCollectionNum then
             let 
               cutOffPoolFlowMap = Map.map (\pflow -> CF.splitCashFlowFrameByDate pflow d EqToLeft) poolFlowMap
               collectedFlow =  Map.map fst cutOffPoolFlowMap
               -- outstandingFlow = Map.map (CF.insertBegTsRow d . snd) cutOffPoolFlowMap
               outstandingFlow = Map.map snd cutOffPoolFlowMap
               -- deposit cashflow to SPV from external pool cf               
               accs = depositPoolFlow (collects t) d collectedFlow accMap -- `debug` ("d"++ show d++">>>"++ show collectedFlow++"\n")
               
               dAfterDeposit = (appendCollectedCF d t collectedFlow) {accounts=accs}   -- `debug` ("Collected flow"++ show collectedFlow)
               
               -- newScheduleFlowMap = Map.map (over CF.cashflowTxn (cutBy Exc Future d)) (fromMaybe Map.empty (getScheduledCashflow t Nothing))
               dealAfterUpdateScheduleFlow = over dealScheduledCashflow 
                                                  (Map.map (\mflow -> over CF.cashflowTxn (cutBy Exc Future d) <$> mflow))
                                                  dAfterDeposit

               runContext = RunContext outstandingFlow rAssump rates
               
               (dRunWithTrigger0, rc1,ads2, newLogs0) = runTriggers (dealAfterUpdateScheduleFlow,runContext,ads) d EndCollection  
               eopActionsLog = [ RunningWaterfall d W.EndOfPoolCollection | Map.member W.EndOfPoolCollection waterfallM ] -- `debug` ("new logs from trigger 1"++ show newLogs0)
               
               waterfallToExe = Map.findWithDefault [] W.EndOfPoolCollection (waterfall t)  -- `debug` ("new logs from trigger 1"++ show newLogs0)
               
               (dAfterAction,rc2,newLogs) = foldl (performActionWrap d) (dRunWithTrigger0 ,rc1 ,log ) waterfallToExe -- `debug` ("End collection action"++ show waterfallToExe)
               
               (dRunWithTrigger1,rc3,ads3,newLogs1) = runTriggers (dAfterAction,rc2,ads2) d EndCollectionWF -- `debug` ("new logs from waterfall 2"++ show newLogs)
             in 
               run dRunWithTrigger1 (runPoolFlow rc3) (Just ads3) rates calls rAssump (newLogs0++newLogs++ eopActionsLog ++newLogs1) --  `debug` ("Run  logs pool collection "++ show (length (log++newLogs0++newLogs++newLogs1))) -- `debug` ("last log"++ show (last ads))     -- `debug` ("End :after new pool flow"++ show (runPoolFlow rc))
           else
             run t Map.empty (Just ads) rates calls rAssump log  
   
         RunWaterfall d _ ->
            let
              runContext = RunContext poolFlowMap rAssump rates

              -- ads1 should be replace in the future
              -- newLogs0 -> record the deal status change info ,incremental 
              (dRunWithTrigger0, rc1, ads1, newLogs0) = runTriggers (t, runContext, ads) d BeginDistributionWF
              -- warning if not waterfall distribution found
              waterfallKey = if Map.member (W.DistributionDay dStatus) waterfallM then 
                               W.DistributionDay dStatus
                             else 
                               W.DefaultDistribution
                          
              waterfallToExe = Map.findWithDefault [] waterfallKey waterfallM
              logsBeforeDist = newLogs0++[ WarningMsg ("No waterfall distribution found on date"++show d++"with waterfall key "++show waterfallKey) 
                                | Map.notMember waterfallKey waterfallM ]
           in 
              case calls of
                 Just callOpts ->
                   if testCalls dRunWithTrigger0 d callOpts then 
                     let 
                        (dealAfterCleanUp, rc_, newLogWaterfall_ ) = foldl (performActionWrap d) (dRunWithTrigger0, rc1,log) cleanUpActions 
                        newStLogs = if null cleanUpActions then 
                                      [DealStatusChangeTo d dStatus Called]
                                    else 
                                      [DealStatusChangeTo d dStatus Called,RunningWaterfall d W.CleanUp]
                        endingLogs = Rpt.patchFinancialReports dealAfterCleanUp d log
                     in  
                        -- TODO missing newLogWaterfall_
                        (prepareDeal dealAfterCleanUp, endingLogs ++ logsBeforeDist ++newStLogs++[EndRun (Just d) "Clean Up"]) -- `debug` ("Called ! "++ show d)
                   else
                     let 
                       (dAfterWaterfall, rc2, newLogsWaterfall) = foldl (performActionWrap d) (dRunWithTrigger0,rc1,log) waterfallToExe 
                       (dRunWithTrigger1, rc3, ads2, newLogs2) = runTriggers (dAfterWaterfall,rc2,ads1) d EndDistributionWF  
                       
                     in 
                       run dRunWithTrigger1 (runPoolFlow rc3) (Just ads2) rates calls rAssump (newLogsWaterfall++newLogs2++logsBeforeDist++[RunningWaterfall d waterfallKey]) 
                 Nothing ->
                   let 
                     (dAfterWaterfall, rc2, newLogsWaterfall) = foldl (performActionWrap d) (dRunWithTrigger0,rc1,log) waterfallToExe 
                     (dRunWithTrigger1, rc3,ads2, newLogs2) = runTriggers (dAfterWaterfall,rc2,ads1) d EndDistributionWF  
                   in 
                     run dRunWithTrigger1 (runPoolFlow rc3) (Just ads2) rates calls rAssump (newLogsWaterfall++newLogs2++logsBeforeDist++[RunningWaterfall d waterfallKey]) 
                     
         EarnAccInt d accName ->
           let 
             newAcc = Map.adjust 
                        (\a -> case a of
                                (A.Account _ _ (Just A.BankAccount {}) _ _ ) -> A.depositInt a d  -- `debug` ("int acc"++show accName)
                                (A.Account _ _ (Just (A.InvestmentAccount idx _ lastAccureDate _)) _ _ ) -> 
                                  case AP.getRateAssumption (fromMaybe [] rates) idx of
                                    Nothing -> a -- `debug` ("error..."++show accName)
                                    Just (RateCurve _ _ts) -> A.depositIntByCurve a _ts d 
                                    Just (RateFlat _ r )   -> A.depositIntByCurve a (mkRateTs [(lastAccureDate,r),(d,r)]) d
                                    _ -> error ("Failed to match index "++show idx++" In rate assumpt" ++ name t) ) -- `debug` ("int acc"++show accName)
                        accName  
                        accMap
           in 
             run (t {accounts = newAcc}) poolFlowMap (Just ads) rates calls rAssump log

         AccrueFee d feeName ->  -- (t , log)
           let 
             newFeeMap = Map.adjust (calcDueFee t d) feeName feeMap -- `debug` ("Accure Fee on Actions")
           in
             run (t{fees=newFeeMap}) poolFlowMap (Just ads) rates calls rAssump log
   
         ResetLiqProvider d liqName -> 
           case liqProvider t of 
             Nothing -> run t poolFlowMap (Just ads) rates calls rAssump log
             (Just mLiqProvider) 
               -> let -- update credit 
                    newLiqMap = Map.adjust (updateLiqProvider t d) liqName mLiqProvider
                  in
                    run (t{liqProvider = Just newLiqMap}) poolFlowMap (Just ads) rates calls rAssump log

         ResetLiqProviderRate d liqName -> 
           case liqProvider t of 
             Nothing -> run t poolFlowMap (Just ads) rates calls rAssump log
             (Just mLiqProvider) 
               -> let -- update rate 
                    newLiqMap = Map.adjust (updateLiqProviderRate t d (fromMaybe [] rates)) liqName mLiqProvider
                  in
                    run (t{liqProvider = Just newLiqMap}) poolFlowMap (Just ads) rates calls rAssump log
        
         DealClosed d ->
           let
             newSt = case dStatus of
                        (PreClosing st) -> st
                        _ -> error $ "DealClosed action is not in PreClosing status but got"++ show dStatus
             w = Map.findWithDefault [] W.OnClosingDay (waterfall t)  -- `debug` ("DDD0")
             rc = RunContext poolFlowMap rAssump rates  
             (newDeal, newRc, newLog) = foldl (performActionWrap d) (t, rc, log) w  -- `debug` ("ClosingDay Action:"++show w)
             logForClosed = [RunningWaterfall d W.OnClosingDay| not (null w)] -- `debug` ("DDD1")]
           in 
             run newDeal{status=newSt} (runPoolFlow newRc) (Just ads) rates calls rAssump (newLog++[DealStatusChangeTo d (PreClosing newSt) newSt]++logForClosed) -- `debug` ("new st at closing"++ show newSt)

         ChangeDealStatusTo d s -> run (t{status=s}) poolFlowMap (Just ads) rates calls rAssump log

         ResetIRSwapRate d sn -> 
           let
             _rates = fromMaybe [] rates
             newRateSwap_rate = Map.adjust (updateRateSwapRate _rates d) sn <$> rateSwap t  
             newRateSwap_bal = Map.adjust (updateRateSwapBal t d) sn <$> newRateSwap_rate 
             newRateSwap_acc = Map.adjust (HE.accrueIRS d) sn <$> newRateSwap_bal
           in 
             run (t{rateSwap = newRateSwap_acc}) poolFlowMap (Just ads) rates calls rAssump log

         AccrueCapRate d cn -> 
            let
             _rates = fromMaybe [] rates
             newRateCap = Map.adjust (accrueRC t d _rates) cn <$> rateCap t
           in 
             run (t{rateCap = newRateCap}) poolFlowMap (Just ads) rates calls rAssump log

         InspectDS d ds -> 
           let 
             newlog = inspectVars t d ds 
           in 
             run t poolFlowMap (Just ads) rates calls rAssump $ log++[newlog] -- `debug` ("Add log"++show newlog)
         
         ResetBondRate d bn -> 
             let 
               rateList = fromMaybe [] rates
               newBndMap = Map.adjust (setBondNewRate t d rateList) bn bndMap -- `debug` ("Reset bond"++show bn)
             in 
               run t{bonds = newBndMap} poolFlowMap (Just ads) rates calls rAssump log
         
         BuildReport sd ed ->
             let 
               bsReport = Rpt.buildBalanceSheet t ed 
               cashReport = Rpt.buildCashReport t sd ed 
               newlog = FinancialReport sd ed bsReport cashReport
             in 
               run t poolFlowMap (Just ads) rates calls rAssump $ log++[newlog] 

                 
         FireTrigger d cyc n -> 
             let 
               triggerFired = case mTrgMap of 
                                  Nothing -> error "trigger is empty for override" 
                                  Just tm -> Map.adjust (Map.adjust (set trgStatusLens True) n) cyc tm
               triggerEffects = case mTrgMap of 
                                  Nothing -> Nothing
                                  Just tm -> case Map.lookup cyc tm of
                                               Nothing -> Nothing
                                               Just cycM -> case Map.lookup n cycM of
                                                              Nothing -> Nothing
                                                              Just trg -> Just $ trgEffects trg
               
               runContext = RunContext poolFlowMap rAssump rates

               (newT, rc@(RunContext newPool _ _),adsFromTrigger) = case triggerEffects of 
                                                      Nothing -> (t,runContext,ads)  `debug` "Nothing found on effects"
                                                      Just efs -> runEffects (t,runContext, ads) d efs
               (oldStatus,newStatus) = (status t,status newT)
               stChangeLogs = [DealStatusChangeTo d oldStatus newStatus |  oldStatus /= newStatus] 

               newLog = WarningMsg $ "Trigger Overrided to True "++ show(d,cyc,n)
             in 
               run newT{triggers = Just triggerFired} newPool (Just ads) rates calls rAssump $ log++[newLog]++stChangeLogs
         
         MakeWhole d spd walTbl -> 
             let 
               schedulePoolFlowMap = Map.map (fromMaybe (CF.CashFlowFrame (0,epocDate,Nothing) []))  $ view dealScheduledCashflow t
               factor = divideBB (queryDeal t (FutureCurrentPoolBegBalance Nothing)) (queryDeal t (FutureCurrentSchedulePoolBegBalance Nothing))
               reduceCfs = Map.map (over CF.cashflowTxn (\xs -> (CF.scaleTsRow factor) <$> xs)) schedulePoolFlowMap -- need to apply with factor and trucate with date
               (runDealWithSchedule,_) = run t reduceCfs (Just ads) rates calls rAssump $ log
               bondWal = Map.map (L.calcWalBond d) (bonds runDealWithSchedule) -- `debug` ("Bond schedule flow"++ show (bonds runDealWithSchedule))
               bondSprd = Map.map 
                            (\x -> (spd + (fromMaybe 0 (lookupTable walTbl Up (fromRational x >)))))
                          bondWal 
               bondPricingCurve = Map.map 
                                    (\x -> IRateCurve [ TsPoint d x,TsPoint (getDate (last ads)) x] )
                                  bondSprd 
               bondPricingResult = Map.intersectionWithKey (\k v1 v2 -> L.priceBond d v2 v1) (bonds runDealWithSchedule) bondPricingCurve 
               depositBondFlow = Map.intersectionWith
                                  (\bnd (PriceResult pv _ _ _ _ _ _) -> 
                                    let 
                                      ostBal = L.getCurBalance bnd
                                      prinToPay = min pv ostBal
                                      intToPay = max 0 (pv - prinToPay)
                                      bnd1 = L.payPrin d prinToPay bnd
                                    in 
                                      L.payYield d intToPay bnd1)
                                  (bonds t)
                                  bondPricingResult
             in 
               run t {bonds = depositBondFlow, status = Ended } poolFlowMap (Just []) rates calls rAssump $ log++[EndRun (Just d) "MakeWhole call"]
         
         IssueBond d Nothing bGroupName accName bnd mBal mRate -> run t poolFlowMap (Just ((IssueBond d (Just (Always True)) bGroupName accName bnd mBal mRate):ads)) rates calls rAssump log
         
         IssueBond d (Just p) bGroupName accName bnd mBal mRate ->
             case testPre d t p of
               False -> run t poolFlowMap (Just ads) rates calls rAssump (log ++ [WarningMsg ("Failed to issue to bond group"++ bGroupName++ ":" ++show p)])
               True ->
                        let 
                          newBndName = L.bndName bnd
                          newBalance = case mBal of
                                        Just _q -> queryDeal t (patchDateToStats d _q)  
                                        Nothing -> L.originBalance (L.bndOriginInfo bnd)
                          newRate = case mRate of 
                                        Just _q -> toRational $ queryDealRate t (patchDateToStats d _q)
                                        Nothing -> L.originRate (L.bndOriginInfo bnd)
                          newBonds = case Map.lookup bGroupName bndMap of
                                        Nothing -> bndMap
                                        Just L.Bond {} -> bndMap
                                        Just (L.BondGroup bndGrpMap) -> let
                                                                          bndOInfo = (L.bndOriginInfo bnd) {L.originDate = d, L.originRate = newRate, L.originBalance = newBalance }
                                                                          bndToInsert = bnd {L.bndOriginInfo = bndOInfo,
                                                                                             L.bndDueIntDate = Just d,
                                                                                             L.bndLastIntPay = Just d, 
                                                                                             L.bndLastPrinPay = Just d,
                                                                                             L.bndRate = fromRational newRate,
                                                                                             L.bndBalance = newBalance}
                                                                        in 
                                                                          Map.insert bGroupName 
                                                                                     (L.BondGroup (Map.insert newBndName bndToInsert bndGrpMap))
                                                                                     bndMap

                          issuanceProceeds = newBalance
                          newAcc = Map.adjust (A.deposit issuanceProceeds d (IssuanceProceeds newBndName))
                                              accName
                                              accMap
                        in 
                          run t{bonds = newBonds, accounts = newAcc} poolFlowMap (Just ads) rates calls rAssump log
         RefiBondRate d accName bName iInfo ->
            let
              -- settle accrued interest 
              nBnd = calcDueInt t d Nothing Nothing $ bndMap Map.! bName
              dueIntToPay = L.totalDueInt nBnd

              ((shortfall,drawAmt),newAcc) = A.tryDraw dueIntToPay d (PayInt [bName]) (accMap Map.! accName)

              newBnd = set L.bndIntLens iInfo $ L.payInt d drawAmt nBnd

              newAccMap = Map.insert accName newAcc accMap
              -- reset interest info
              newRate = L.getBeginRate iInfo
              newBndMap = Map.insert bName (newBnd {L.bndRate = newRate, L.bndDueIntDate = Just d 
                                                    ,L.bndLastIntPay = Just d}) bndMap
              -- TODO rebuild bond rate reset actions
              lstDate = getDate (last ads)
              resetDates = L.buildRateResetDates newBnd d lstDate 
              bResetActions = [ ResetBondRate d bName | d <- resetDates ]
              isResetActionEvent (ResetBondRate _ bName) = False 
              isResetActionEvent _ = True
              filteredAds = filter isResetActionEvent ads
              newAds = sortBy sortActionOnDate $ filteredAds ++ bResetActions
            in 
              run t{bonds = newBndMap, accounts = newAccMap} poolFlowMap (Just newAds) rates calls rAssump log
            
         RefiBond d accName bnd -> undefined

         _ -> error $ "Failed to match action on Date"++ show ad
         where
           cleanUpActions = Map.findWithDefault [] W.CleanUp (waterfall t) -- `debug` ("Running AD"++show(ad))
           remainCollectionNum = Map.elems $ Map.map CF.sizeCashFlowFrame poolFlowMap
           futureCashToCollect = Map.elems $ Map.map (\pcf -> sum (CF.tsTotalCash <$> view CF.cashflowTxn pcf)) poolFlowMap


run t empty Nothing Nothing Nothing Nothing log
  = run t pcf (Just ads) Nothing Nothing Nothing log  -- `debug` ("Init Done >>Last Action#"++show (length ads)++"F/L"++show (head ads)++show (last ads))
  where
    (t, ads, pcf, unStressPcf) = getInits t Nothing Nothing 

run t empty _ _ _ _ log = (prepareDeal t,log) -- `debug` ("End with pool CF is []")



-- reserved for future used
data ExpectReturn = DealStatus
                  | DealPoolFlow
                  | DealPoolFlowPricing   -- ^ default option, return pricing and bond/pool/account/fee etc cashflow
                  | DealTxns
                  | ExecutionSummary
                  deriving (Show,Generic)

priceBonds :: TestDeal a -> AP.BondPricingInput -> Map.Map String L.PriceResult
priceBonds t (AP.DiscountCurve d dc) = Map.map (L.priceBond d dc) (viewBondsInMap t)
priceBonds t@TestDeal {bonds = bndMap} (AP.RunZSpread curve bond_prices) 
  = Map.mapWithKey 
      (\bn (pd,price)-> L.ZSpread $
                           L.calcZspread 
                             (price,pd) 
                             0
                             (1.0
                              ,(1.0,0.5)
                              ,toRational (rateToday pd - toRational (L.bndRate (bndMap Map.!bn))))
                             (bndMap Map.! bn)
                             curve)
      bond_prices
    where 
      rateToday = getValByDate curve Inc     

runDeal :: Ast.Asset a => TestDeal a -> ExpectReturn -> Maybe AP.ApplyAssumptionType-> AP.NonPerfAssumption
        -> (TestDeal a, Maybe (Map.Map PoolId CF.CashFlowFrame), Maybe [ResultComponent], Maybe (Map.Map String L.PriceResult))
runDeal t _ perfAssumps nonPerfAssumps@AP.NonPerfAssumption{AP.callWhen  = opts
                                                           ,AP.pricing   = mPricing
                                                           ,AP.revolving = mRevolving
                                                           ,AP.interest  = mInterest} 
  | not runFlag = (t, Nothing, Just valLogs, Nothing)
  | otherwise = (finalDeal, Just poolFlowUsedNoEmpty, Just (getRunResult finalDeal ++ V.validateRun finalDeal ++logs), bndPricing)  `debug` ("Run Deal end with")
    where
      (runFlag, valLogs) = V.validateReq t nonPerfAssumps 
      -- getinits() will get (new deal snapshot, actions, pool cashflows, unstressed pool cashflow)
      (newT, ads, pcf, unStressPcf) = getInits t perfAssumps (Just nonPerfAssumps)  
      -- extract Revolving Assumption
      mRevolvingCtx = case mRevolving of
                        Nothing -> Nothing
                        Just (AP.AvailableAssets rp rperf) -> Just (Map.fromList [("Consol", (rp, rperf))])
                        Just (AP.AvailableAssetsBy rMap) -> Just rMap
                        Just _ -> error ("Failed to match revolving assumption"++show mRevolving)
      -- run() is a recusive function loop over all actions till deal end conditions are met
      (finalDeal, logs) = run (removePoolCf newT) 
                              pcf
                              (Just ads) 
                              mInterest
                              opts
                              mRevolvingCtx
                              []  
      poolFlowUsed = Map.map (fromMaybe (CF.CashFlowFrame (0,toDate "19000101",Nothing) [])) (getAllCollectedFrame finalDeal Nothing)  
      poolFlowUsedNoEmpty = Map.map (over CF.cashflowTxn CF.dropTailEmptyTxns) poolFlowUsed  
      -- bond pricing if any                            
      bndPricing = case mPricing of
                     Nothing -> Nothing     
                     Just _bpi -> Just (priceBonds finalDeal _bpi)  

-- | get bond principal and interest shortfalls from a deal
getRunResult :: Ast.Asset a => TestDeal a -> [ResultComponent]
getRunResult t = os_bn_i ++ os_bn_b -- `debug` ("Done with get result")
  where 
    bs = viewDealAllBonds t  
    os_bn_b = [ BondOutstanding (L.bndName _b) (L.bndBalance _b) (getBondBegBal t (L.bndName _b)) | _b <- bs ] -- `debug` ("B"++ show bs)
    os_bn_i = [ BondOutstandingInt (L.bndName _b) (L.bndDueInt _b) (getBondBegBal t (L.bndName _b)) | _b <- bs ] -- `debug` ("C"++ show bs)

prepareDeal :: Ast.Asset a => TestDeal a -> TestDeal a
prepareDeal t@TestDeal {bonds = bndMap} 
  = let 
      pIdCf = view dealCashflow t
      -- dropTailEmptyTxns   
      newPtMap = Map.map (\mCf -> (over CF.cashflowTxn CF.dropTailEmptyTxns) <$> mCf )
                          pIdCf
      t1 = set dealCashflow newPtMap t
    in 
      t1 {bonds = Map.map (L.patchBondFactor . L.consolStmt) bndMap}  -- `debug` ("Prepare Done")


appendCollectedCF :: Ast.Asset a => Date -> TestDeal a -> Map.Map PoolId CF.CashFlowFrame -> TestDeal a
-- ^ append cashflow frame (consolidate by a date) into deals collected pool
appendCollectedCF d t@TestDeal { pool = pt } poolInflowMap
  = let 
      newPt = case pt of
                SoloPool p -> 
                  let
                    txnCollected::[CF.TsRow] = view CF.cashflowTxn (poolInflowMap Map.! PoolConsol)
                    balInCollected = case length txnCollected of
                                       0 -> 0 
                                       _ ->  CF.mflowBalance $ last txnCollected
                    currentStats = case view P.poolFutureTxn p of
                                      [] -> P.poolBegStats p
                                      txns -> fromMaybe (0,0,0,0,0,0) $ view CF.txnCumulativeStats (last txns)
                    txnToAppend = CF.patchCumulative currentStats [] txnCollected -- `debug` ("Start iwht current stats="++ show currentStats)
                  in 
                    SoloPool $ over P.poolIssuanceStat (Map.insert RuntimeCurrentPoolBalance balInCollected) $ over P.poolFutureTxn (++ txnToAppend) p
                MultiPool poolM -> 
                  MultiPool $
                    Map.foldrWithKey
                      (\k (CF.CashFlowFrame _ txnCollected) acc ->
                        let 
                          currentStats = case view P.poolFutureTxn (acc Map.! k) of
                                          [] -> P.poolBegStats (acc Map.! k)
                                          txns -> fromMaybe (0,0,0,0,0,0) $ view CF.txnCumulativeStats (last txns)
                          balInCollected = case length txnCollected of 
                                             0 -> 0 
                                             _ ->  CF.mflowBalance $ last txnCollected
                          txnToAppend = CF.patchCumulative currentStats [] txnCollected
                          accUpdated =  Map.adjust (over P.poolFutureTxn (++ txnToAppend)) k acc
                        in 
                          Map.adjust 
                            (over P.poolIssuanceStat (Map.insert RuntimeCurrentPoolBalance balInCollected))
                            k accUpdated)
                      poolM
                      poolInflowMap
                ResecDeal uds -> 
                  ResecDeal $ 
                    Map.foldrWithKey
                      (\k (CF.CashFlowFrame _ newTxns) acc->
                        Map.adjust (over uDealFutureTxn (++ newTxns)) k acc)
                      uds poolInflowMap
    in 
      t {pool = newPt}  -- `debug` ("after insert bal"++ show newPt)

-- ^ emtpy deal's pool cashflow
removePoolCf :: Ast.Asset a => TestDeal a -> TestDeal a
removePoolCf t@TestDeal{pool=pt} =
  let 
    newPt = case pt of 
              SoloPool p -> SoloPool $ set P.poolFutureCf Nothing p
              MultiPool pM -> MultiPool $ Map.map (set P.poolFutureCf Nothing) pM 
              ResecDeal uds -> ResecDeal uds
              _ -> error "not implement"
  in
    t {pool=newPt}

-- ^ TODO: need to set cashflow to different pool other than SoloPool
setFutureCF :: Ast.Asset a => TestDeal a -> CF.CashFlowFrame -> TestDeal a
setFutureCF t@TestDeal{pool = (SoloPool p )} cf 
  = let 
      newPool =  p {P.futureCf = Just cf}
      newPoolType = SoloPool newPool 
    in 
      t {pool = newPoolType }

populateDealDates :: DateDesp -> DealStatus -> (Date,Date,Date,[ActionOnDate],[ActionOnDate],Date)
populateDealDates (WarehousingDates begDate rampingPoolDp rampingBondDp statedDate)
                  (Warehousing _) 
  = (begDate,begDate, getDate (head ba),pa,ba,statedDate)
    where
      pa = [ PoolCollection _d "" | _d <- genSerialDatesTill2 IE begDate rampingPoolDp statedDate ]
      ba = [ RunWaterfall _d "" | _d <- genSerialDatesTill2 IE begDate rampingBondDp statedDate ]

populateDealDates (CustomDates cutoff pa closing ba) _ 
  = (cutoff  
    ,closing
    ,getDate (head ba)
    ,pa
    ,ba
    ,getDate (max (last pa) (last ba)))

populateDealDates (PatternInterval _m) _
  = (cutoff,closing,nextPay,pa,ba,max ed1 ed2) 
    where 
      (cutoff,dp1,ed1) = _m Map.! CutoffDate
      (nextPay,dp2,ed2) = _m Map.! FirstPayDate 
      (closing,_,_) = _m Map.! ClosingDate
      pa = [ PoolCollection _d "" | _d <- genSerialDatesTill cutoff dp1 ed1 ]
      ba = [ RunWaterfall _d "" | _d <- genSerialDatesTill nextPay dp2 ed2 ]

populateDealDates (PreClosingDates cutoff closing mRevolving end (firstCollect,poolDp) (firstPay,bondDp)) _
  = (cutoff,closing,firstPay,pa,ba,end) 
    where 
      pa = [ PoolCollection _d "" | _d <- genSerialDatesTill2 IE firstCollect poolDp end ]
      ba = [ RunWaterfall _d "" | _d <- genSerialDatesTill2 IE firstPay bondDp end ]

populateDealDates (CurrentDates (lastCollect,lastPay) mRevolving end (nextCollect,poolDp) (nextPay,bondDp)) _
  = (lastCollect, lastPay,head futurePayDates, pa, ba, end) 
    where 
      futurePayDates = genSerialDatesTill2 IE nextPay bondDp end 
      ba = [ RunWaterfall _d "" | _d <- futurePayDates]
      futureCollectDates = genSerialDatesTill2 IE nextCollect poolDp end 
      pa = [ PoolCollection _d "" | _d <- futureCollectDates]


-- | run a pool of assets ,use asOfDate of Pool to cutoff cashflow yields from assets with assumptions supplied
runPool :: Ast.Asset a => P.Pool a -> Maybe AP.ApplyAssumptionType -> Maybe [RateAssumption] -> [(CF.CashFlowFrame, Map.Map CutoffFields Balance )]
-- schedule cashflow just ignores the interest rate assumption
runPool (P.Pool [] (Just cf) _ asof _ _ ) Nothing _ = [(cf, Map.empty)]
-- schedule cashflow with stress assumption
runPool (P.Pool [] (Just (CF.CashFlowFrame _ txn)) _ asof _ (Just dp)) (Just (AP.PoolLevel assumps)) mRates = [ Ast.projCashflow (ACM.ScheduleMortgageFlow asof txn dp) asof assumps mRates ] -- `debug` ("PROJ in schedule flow")

-- project contractual cashflow if nothing found in pool perf assumption
-- use interest rate assumption
runPool (P.Pool as _ _ asof _ _) Nothing mRates = map (\x -> (Ast.calcCashflow x asof mRates,Map.empty)) as 

-- asset cashflow with credit stress
---- By pool level
runPool (P.Pool as Nothing Nothing asof _ _) (Just (AP.PoolLevel assumps)) mRates = map (\x -> Ast.projCashflow x asof assumps mRates) as  
---- By index
runPool (P.Pool as Nothing Nothing asof _ _) (Just (AP.ByIndex idxAssumps)) mRates =
  let
    numAssets = length as
    _assumps = map (AP.lookupAssumptionByIdx idxAssumps) [0..(pred numAssets)] -- `debug` ("Num assets"++ show numAssets)
  in
    zipWith (\x a -> Ast.projCashflow x asof a mRates) as _assumps 
---- By Obligor
runPool (P.Pool as Nothing Nothing asof _ _) (Just (AP.ByObligor obligorRules)) mRates =
  let
            -- result cf,rules,assets
    matchAssets []   _ [] = [(CF.CashFlowFrame (0,epocDate,Nothing) [], Map.empty)] 
    matchAssets cfs [] [] = cfs 
    matchAssets cfs [] astList = cfs ++ ((\x -> (Ast.calcCashflow x asof mRates,Map.empty)) <$> astList)
    matchAssets cfs (rule:rules) astList = 
      case rule of 
        AP.ObligorById ids assetPerf 
          -> let 
              idSet = S.fromList ids
              (matchedAsts,unMatchedAsts) = partition 
                                              (\x -> case Ast.getObligorId x of 
                                                        Just oid -> S.member oid idSet
                                                        Nothing -> False) 
                                              astList
              matchedCfs = (\x -> Ast.projCashflow x asof assetPerf mRates) <$> matchedAsts 
              in 
                matchAssets (cfs ++ matchedCfs) rules unMatchedAsts
        AP.ObligorByTag tags tagRule assetPerf ->
          let 
            obrTags = S.fromList tags

            matchRuleFn AP.TagEq s1 s2 = s1 == s2 
            matchRuleFn AP.TagSubset s1 s2 = s1 `S.isSubsetOf` s2
            matchRuleFn AP.TagSuperset s1 s2 = s2 `S.isSubsetOf` s1
            matchRuleFn AP.TagAny s1 s2 = not $ S.null $ S.intersection s1 s2
            matchRuleFn (AP.TagNot tRule) s1 s2 = not $ matchRuleFn tRule s1 s2
            
            (matchedAsts,unMatchedAsts) = partition (\x -> matchRuleFn tagRule (Ast.getObligorTags x) obrTags) astList
            matchedCfs = (\x -> Ast.projCashflow x asof assetPerf mRates) <$> matchedAsts 
          in 
            matchAssets (cfs ++ matchedCfs) rules unMatchedAsts
        
        AP.ObligorByField fieldRules assetPerf -> 
          let 

            matchRuleFn (AP.FieldIn fv fvals) Nothing = False
            matchRuleFn (AP.FieldIn fv fvals) (Just fm) = case Map.lookup fv fm of
                                                    Just (Left v) -> v `elem` fvals
                                                    Nothing -> False
            matchRuleFn (AP.FieldCmp fv cmp dv) (Just fm) = case Map.lookup fv fm of
                                                        Just (Right v) -> case cmp of 
                                                                    G -> v > dv
                                                                    L -> v < dv
                                                                    GE -> v >= dv
                                                                    LE -> v <= dv
                                                        Nothing -> False
            matchRuleFn (AP.FieldInRange fv rt dv1 dv2) (Just fm) = 
              case Map.lookup fv fm of
                Just (Right v) -> case rt of 
                          II -> v <= dv2 && v >= dv1
                          IE -> v <= dv2 && v > dv1
                          EI -> v < dv2 && v >= dv1
                          EE -> v < dv2 && v > dv1
                          _ -> False
                Nothing -> False
            matchRuleFn (AP.FieldNot fRule) fm = not $ matchRuleFn fRule fm

            matchRulesFn fs fm = all (\f -> matchRuleFn f fm) fs

            (matchedAsts,unMatchedAsts) = partition (\x -> matchRulesFn fieldRules (Ast.getObligorFields x)) astList            
            matchedCfs = (\x -> Ast.projCashflow x asof assetPerf mRates) <$> matchedAsts 
          in 
            matchAssets (cfs ++ matchedCfs) rules unMatchedAsts
        AP.ObligorByDefault assetPerf ->
          matchAssets 
            (cfs ++ ((\x -> Ast.projCashflow x asof assetPerf mRates) <$> astList))
            []
            []

        
  in
    matchAssets [] obligorRules as



-- safe net to catch other cases
runPool _a _b _c = error $ "Failed to match" ++ show _a ++ show _b ++ show _c


-- ^ patch issuance balance for PreClosing Deal
patchIssuanceBalance :: Ast.Asset a => DealStatus -> Map.Map PoolId Balance -> PoolType a -> PoolType a
patchIssuanceBalance (Warehousing _) balM pt = patchIssuanceBalance (PreClosing Amortizing) balM pt
patchIssuanceBalance (PreClosing _ ) balM pt =
  case pt of 
    SoloPool p -> SoloPool $ over P.poolIssuanceStat (Map.insert IssuanceBalance (Map.findWithDefault 0.0 PoolConsol balM)) p -- `debug` ("Insert with issuance balance"++ show (Map.findWithDefault 0.0 PoolConsol balM))
    MultiPool pM -> MultiPool $ Map.mapWithKey (\k v -> over P.poolIssuanceStat (Map.insert IssuanceBalance (Map.findWithDefault 0.0 k balM)) v) pM
    ResecDeal pM -> ResecDeal pM  --TODO patch balance for resec deal
    
patchIssuanceBalance _ bal p = p -- `debug` ("NO patching ?")

patchScheduleFlow :: Ast.Asset a => Map.Map PoolId CF.CashFlowFrame -> PoolType a -> PoolType a
patchScheduleFlow flowM pt = 
  case pt of
    SoloPool p -> case Map.lookup PoolConsol flowM of
                    Nothing -> error $ "Failed to find schedule flow of pool id of Pool Console in "++ show (Map.keys flowM)
                    Just scheduleCf -> SoloPool $ set P.poolFutureScheduleCf (Just scheduleCf) p
    MultiPool pM -> MultiPool $ Map.intersectionWith (set P.poolFutureScheduleCf) (Just <$> flowM) pM
    ResecDeal pM -> ResecDeal pM

runPoolType :: Ast.Asset a => PoolType a -> Maybe AP.ApplyAssumptionType -> Maybe AP.NonPerfAssumption -> Map.Map PoolId (CF.CashFlowFrame, Map.Map CutoffFields Balance)
runPoolType (SoloPool p) mAssumps mNonPerfAssump = Map.fromList [(PoolConsol,P.aggPool (P.issuanceStat p) $ runPool p mAssumps (AP.interest =<< mNonPerfAssump))]
runPoolType (MultiPool pm) (Just (AP.ByName assumpMap)) mNonPerfAssump
  =  Map.mapWithKey 
                    (\k p -> P.aggPool (P.issuanceStat p) $ 
                               runPool p (AP.PoolLevel <$> Map.lookup k assumpMap) (AP.interest =<< mNonPerfAssump))
                    pm

runPoolType (MultiPool pm) (Just (AP.ByPoolId assumpMap)) mNonPerfAssump
  =  Map.mapWithKey 
                    (\k p -> P.aggPool (P.issuanceStat p) $ 
                               runPool p (Map.lookup k assumpMap) (AP.interest =<< mNonPerfAssump))
                    pm

runPoolType (MultiPool pm) mAssumps mNonPerfAssump
  = Map.map (\p -> P.aggPool (P.issuanceStat p) $ runPool p mAssumps (AP.interest =<< mNonPerfAssump)) pm

runPoolType (ResecDeal dm) mAssumps mNonPerfAssump
  = Map.foldrWithKey (\(DealBondFlow dn bn sd pct) (dname, cflow, stat) m ->
                          Map.insert (DealBondFlow dname bn sd pct) (cflow, stat) m)
                          Map.empty $
    Map.mapWithKey (\(DealBondFlow dn bn sd pct) (uDeal, mAssump) -> 
                        let
                          (poolAssump,dealAssump) = case mAssump of 
                                                      Nothing -> (Nothing, AP.NonPerfAssumption Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing)
                                                      Just (_poolAssump, _dealAssump) -> (Just _poolAssump, _dealAssump)
                          (dealRunned, _, _, _) = runDeal uDeal DealPoolFlowPricing poolAssump dealAssump
                          bondFlow = cutBy Inc Future sd $ concat $ Map.elems $ Map.map Stmt.getTxns $ getBondStmtByName dealRunned (Just [bn]) -- `debug` ("Bondflow from underlying runned"++ show (getBondStmtByName dealRunned (Just [bn])))
                          bondFlowRated = (\(BondTxn d b i p r c di dioi f t) -> CF.BondFlow d b p i)  <$> Stmt.scaleByFactor pct bondFlow -- `debug` ("Bondflow from underlying"++ show bondFlow)
                        in
                          (name uDeal, CF.CashFlowFrame (0,sd,Nothing) bondFlowRated, Map.empty)) $
    Map.mapWithKey (\_ (UnderlyingDeal uDeal _ _ _) -> 
                      let 
                        dName = name uDeal -- `debug` ("Getting name of underlying deal:"++ (name uDeal))
                        mAssump = case mAssumps of 
                                    Just (AP.ByDealName assumpMap) -> Map.lookup dName assumpMap
                                    _ -> Nothing
                      in 
                        (uDeal, mAssump))
                    dm

getInits :: Ast.Asset a => TestDeal a -> Maybe AP.ApplyAssumptionType -> Maybe AP.NonPerfAssumption -> (TestDeal a,[ActionOnDate], Map.Map PoolId CF.CashFlowFrame, Map.Map PoolId CF.CashFlowFrame)
getInits t@TestDeal{fees=feeMap,pool=thePool,status=status,bonds=bndMap} mAssumps mNonPerfAssump
  = (newT, allActionDates, pCollectionCfAfterCutoff, pUnstressedAfterCutoff)  -- `debug` ("init done actions->"++ show pCollectionCfAfterCutoff)
  where
    (startDate,closingDate,firstPayDate,pActionDates,bActionDates,endDate) = populateDealDates (dates t) status

    intEarnDates = A.buildEarnIntAction (Map.elems (accounts t)) endDate [] 
    iAccIntDates = [ EarnAccInt _d accName | (accName,accIntDates) <- intEarnDates
                                           , _d <- accIntDates ] 
    --fee accrue dates 
    _feeAccrueDates = F.buildFeeAccrueAction (Map.elems feeMap) endDate [] 
    feeAccrueDates = [ AccrueFee _d _feeName | (_feeName,feeAccureDates) <- _feeAccrueDates
                                             , _d <- feeAccureDates ]
    --liquidation facility
    liqResetDates = case liqProvider t of 
                      Nothing -> []
                      Just mLiqProvider -> 
                          let 
                            _liqResetDates = CE.buildLiqResetAction (Map.elems mLiqProvider) endDate []
                            _liqRateResetDates = CE.buildLiqRateResetAction (Map.elems mLiqProvider) endDate []
                          in 
                            [ ResetLiqProvider _d _liqName |(_liqName,__liqResetDates) <- _liqResetDates
                                                           , _d <- __liqResetDates ]
                            ++ 
                            [ ResetLiqProviderRate _d _liqName |(_liqName,__liqResetDates) <- _liqRateResetDates
                                                               , _d <- __liqResetDates ]                            
    --inspect dates 
    inspectDates = case mNonPerfAssump of
                     Just AP.NonPerfAssumption{AP.inspectOn= Just inspect_vars }
                       -> concat [[ InspectDS _d ds | _d <- genSerialDatesTill2 II startDate dp endDate]  | (dp,ds) <- inspect_vars ]
                     _ -> []
    
    financialRptDates = case mNonPerfAssump of 
                          Just AP.NonPerfAssumption{AP.buildFinancialReport= Just dp } 
                            -> let 
                                 _ds = genSerialDatesTill2 II startDate dp endDate 
                               in 
                                 [ BuildReport _sd _ed  | (_sd,_ed) <- zip _ds (tail _ds) ] 
                          _ -> []

    irSwapRateDates = case rateSwap t of
                        Nothing -> []
                        Just rsm -> Map.elems $ Map.mapWithKey 
                                                 (\k x -> let 
                                                           resetDs = genSerialDatesTill2 EE (HE.rsStartDate x) (HE.rsSettleDates x) endDate
                                                          in 
                                                           flip ResetIRSwapRate k <$> resetDs)
                                                 rsm
    rateCapSettleDates = case rateCap t of 
                           Nothing -> []
                           Just rcM -> Map.elems $ Map.mapWithKey 
                                                     (\k x -> let 
                                                                resetDs = genSerialDatesTill2 EE (HE.rcStartDate x) (HE.rcSettleDates x) endDate
                                                              in 
                                                                flip AccrueCapRate k <$> resetDs)
                                                     rcM
    -- bond rate resets 
    bndRateResets = let 
                      bndWithDate = Map.toList $ Map.map 
                                                (\b -> L.buildRateResetDates b closingDate endDate) 
                                                bndMap
                    in 
                      [ ResetBondRate bdate bn | (bn,bdates) <- bndWithDate, bdate <- bdates ] 
              
    -- mannual triggers 
    mannualTrigger = case mNonPerfAssump of 
                       Just AP.NonPerfAssumption{AP.fireTrigger = Just evts} -> [ FireTrigger d cycle n | (d,cycle,n) <- evts]
                       _ -> []

    -- make whole assumption
    makeWholeDate = case mNonPerfAssump of
                      Just AP.NonPerfAssumption{AP.makeWholeWhen = Just (_d,_s,_t)} -> [MakeWhole _d _s _t]
                      _ -> [] 

    -- issue bonds in the future 
    bondIssuePlan = case mNonPerfAssump of 
                      Just AP.NonPerfAssumption{AP.issueBondSchedule = Just bndPlan} 
                        -> [ IssueBond _d mPre bGroupName accName b mBal mRate | TsPoint _d (AP.IssueBondEvent mPre bGroupName accName b mBal mRate) <- bndPlan]
                      _ -> []

    -- refinance bonds in the future 
    bondRefiPlan = case mNonPerfAssump of 
                      Just AP.NonPerfAssumption{AP.refinance = Just bndPlan} 
                        -> [ RefiBondRate _d accName bName iInfo | TsPoint _d (AP.RefiRate accName bName iInfo) <- bndPlan]
                          ++ [ RefiBond _d accName bnd | TsPoint _d (AP.RefiBond accName bnd) <- bndPlan] 
                           
                      _ -> []

    allActionDates = let 
                       __actionDates = let 
                                        a = concat [bActionDates,pActionDates,iAccIntDates,makeWholeDate
                                                   ,feeAccrueDates,liqResetDates,mannualTrigger,concat rateCapSettleDates
                                                   ,concat irSwapRateDates,inspectDates, bndRateResets,financialRptDates
                                                   ,bondIssuePlan,bondRefiPlan] 
                                      in
                                        case dates t of 
                                          PreClosingDates {} -> sortBy sortActionOnDate $ DealClosed closingDate:a 
                                          _ -> sortBy sortActionOnDate a
                       _actionDates = __actionDates++[HitStatedMaturity endDate]
                     in 
                       case mNonPerfAssump of
                         Just AP.NonPerfAssumption{AP.stopRunBy = Just d} -> cutBy Exc Past d __actionDates ++ [StopRunFlag d]
                         _ -> _actionDates  
     
    pCfM = runPoolType thePool mAssumps mNonPerfAssump
    poolCfTsM = Map.map (\(CF.CashFlowFrame _ txns, pstats) -> cutBy Inc Future startDate txns) pCfM -- `debug` ("Pool cfm"++ show pCfM)
    poolCfTsMwithBegRow = Map.map (\case  
                                      (x:xs) -> buildBegTsRow startDate x:x:xs
                                      [] -> [])
                                  poolCfTsM 
    poolAggCfM = Map.map (\x -> CF.aggTsByDates x (getDates pActionDates)) poolCfTsMwithBegRow  
    pCollectionCfAfterCutoff = Map.map (CF.CashFlowFrame (0,startDate,Nothing)) poolAggCfM -- `debug` ("Pool agg cfm"++ show (Map.map (sliceBy II (toDate "20241201") (toDate "20241231") ) poolAggCfM))
    
    pScheduleCfM = case thePool of
                     (SoloPool p) 
                       -> Map.fromList [(PoolConsol,P.aggPool (P.issuanceStat p) $ runPool p Nothing (AP.interest =<< mNonPerfAssump))]
                     (MultiPool pm) 
                       -> Map.map (\p -> P.aggPool (P.issuanceStat p) $ runPool p Nothing (AP.interest =<< mNonPerfAssump)) pm
                     (ResecDeal dm)
                       -> Map.foldrWithKey (\(DealBondFlow dn bn sd pct) (dname, cflow, stat) m ->
                                     Map.insert (DealBondFlow dname bn sd pct) (cflow, stat) m)
                                     Map.empty $
                          Map.mapWithKey (\(DealBondFlow dn bn sd pct) (UnderlyingDeal uDeal _ _ _) -> 
                                                                let 
                                                                  (dealRunned, _, _, _) = runDeal uDeal DealPoolFlowPricing Nothing (fromMaybe (NonPerfAssumption {}) mNonPerfAssump)
                                                                  bondFlow = cutBy Inc Future sd $ concat $ Map.elems $ Map.map Stmt.getTxns $ getBondStmtByName dealRunned (Just [bn])
                                                                  bondFlowRated = (\(BondTxn d b i p r c di dioi f t) -> CF.BondFlow d b p i)  <$> Stmt.scaleByFactor pct bondFlow
                                                                  begBal = case bondFlowRated of 
                                                                              [] -> 0
                                                                              _ -> CF.mflowBegBalance $ head bondFlowRated
                                                                in
                                                                  (name uDeal, CF.CashFlowFrame (begBal,sd,Nothing) bondFlowRated, Map.empty))
                                          dm
    pTxnOfSpv = Map.map (\(CF.CashFlowFrame _ txns, pstats) -> cutBy Inc Future startDate txns) pScheduleCfM  
    pTxnWithBegRow = Map.map (\(x:xs) -> buildBegTsRow startDate x:x:xs) pTxnOfSpv  
    -- pAggCfM = Map.map (\x -> CF.aggTsByDates x (getDates pActionDates)) pTxnWithBegRow
    pAggCfM = pTxnWithBegRow
    pUnstressedAfterCutoff = Map.map (CF.CashFlowFrame (0,startDate,Nothing)) pAggCfM
    -- if preclosing deal , issuance balance is using beg balance of projected cashflow
    -- if it is ongoing deal, issuance balance is user input ( deal is not aware of issuance balance as point of time)
    -- issuanceBalance = case status t of
    --                     (PreClosing _) -> CF.mflowBalance begRow
    --                     _ -> fromMaybe 0 $ (Map.lookup IssuanceBalance) >>= (P.issuanceStat thePool)
    -- rateCurves = buildRateCurves [] dealAssumps  
    -- revolvingCurves = getRevolvingCurve dealAssumps -- `debug` ("Getting revolving Curves")
    -- callOptions = buildCallOptions Nothing dealAssumps 
    -- Expense Override
    newFeeMap = case mNonPerfAssump of
                  Nothing -> feeMap
                  Just AP.NonPerfAssumption{AP.projectedExpense = Nothing } -> feeMap
                  -- Just AP.NonPerfAssumption{AP.projectedExpense = Just (fn,projectedFlow) } 
                  --  -> Map.adjust (\x -> x {F.feeType = F.FeeFlow projectedFlow}) fn feeMap
                  Just AP.NonPerfAssumption{AP.projectedExpense = Just pairs } 
                    ->   foldr  (\(feeName,feeFlow) accM -> Map.adjust (\v -> v {F.feeType = F.FeeFlow feeFlow}) feeName accM)  feeMap pairs
    -- newPoolStat = Map.unionWith (+) (fromMaybe Map.empty (P.issuanceStat thePool)) historyStats
    -- newT = t {fees = newFeeMap, pool = thePool {P.issuanceStat = Just newPoolStat } } `debug` ("init with new pool stats"++ show newPoolStat)
    poolWithSchedule = patchScheduleFlow pUnstressedAfterCutoff thePool -- `debug` ("D")
    newT = t {fees = newFeeMap
             , pool = patchIssuanceBalance status (Map.map (CF.mflowBegBalance . head) poolAggCfM) poolWithSchedule
             } -- patching with performing balance

-- ^ UI translation : to read pool cash
readProceeds :: PoolSource -> CF.TsRow -> Balance
readProceeds CollectedInterest  = CF.mflowInterest
readProceeds CollectedPrincipal = CF.mflowPrincipal
readProceeds CollectedRecoveries = CF.mflowRecovery
readProceeds CollectedPrepayment = CF.mflowPrepayment
readProceeds CollectedRental     = CF.mflowRental
readProceeds CollectedPrepaymentPenalty =  CF.mflowPrepaymentPenalty
readProceeds CollectedCash =  CF.tsTotalCash
readProceeds CollectedFeePaid = CF.mflowFeePaid
readProceeds a = error $ "failed to read pool cashflow rule"++show a


extractTxnsFromFlowFrameMap :: Maybe [PoolId] -> Map.Map PoolId CF.CashFlowFrame -> [CF.TsRow]
extractTxnsFromFlowFrameMap mPids pflowMap = 
  case mPids of 
    Nothing -> extractTxns pflowMap
    Just pids -> extractTxns $ Map.filterWithKey (\k _ -> k `elem` pids) pflowMap
  where 
    extractTxns m = concat $ CF.getTsCashFlowFrame <$> Map.elems m


depositInflow :: Date -> W.CollectionRule -> Map.Map PoolId CF.CashFlowFrame -> Map.Map AccountName A.Account -> Map.Map AccountName A.Account
depositInflow d (W.Collect mPids s an) pFlowMap amap 
  = Map.adjust (A.deposit amt d (PoolInflow mPids s)) an amap -- `debug` ("Date"++show d++"Deposit"++show amt++"Rule"++show s ++">>AN"++ show an)
    where 
      txns =  extractTxnsFromFlowFrameMap mPids pFlowMap
      amt = sum $ readProceeds s <$> txns


depositInflow d (W.CollectByPct mPids s splitRules) pFlowMap amap    --TODO need to check 100%
  = foldr
      (\(accName,accAmt) accM -> 
        Map.adjust (A.deposit accAmt d (PoolInflow mPids s)) accName accM)
      amap
      amtsToAccs
    where 
      amtsToAccs = [ (an, mulBR amt splitRate) | (splitRate, an) <- splitRules]
      txns =  extractTxnsFromFlowFrameMap mPids pFlowMap 
      amt = sum $ readProceeds s <$> txns

depositInflow _ a _ _ = error $ "Failed to match collection rule"++ show a

-- ^ deposit cash to account by pool map CF
depositPoolFlow :: [W.CollectionRule] -> Date -> Map.Map PoolId CF.CashFlowFrame -> Map.Map String A.Account -> Map.Map String A.Account
depositPoolFlow rules d pFlowMap amap
  -- = foldr (\pflowM acc -> depositPoolInflow rules d pflowM acc) amap $ pFlowMap `debug` ("Deposit p fd"++ show (Map.elems pFlowMap))
  = foldr (\rule acc -> depositInflow d rule pFlowMap acc) amap rules

$(deriveJSON defaultOptions ''ExpectReturn)


