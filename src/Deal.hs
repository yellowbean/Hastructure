{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}

module Deal (run,runPool,getInits,runDeal,ExpectReturn(..)
            ,performAction
            ,populateDealDates,accrueRC
            ,calcTargetAmount,updateLiqProvider
            ,projAssetUnion,priceAssetUnion
            ,removePoolCf,runPoolType,PoolType
            ,ActionOnDate(..),DateDesp(..)
            ,changeDealStatus
            ) where

import Control.Parallel.Strategies
import qualified Accounts as A
import qualified Ledger as LD
import qualified Asset as Ast
import qualified Pool as P
import qualified Expense as F
import qualified Liability as L
import qualified CreditEnhancement as CE
import qualified Analytics
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

import qualified Data.Map as Map hiding (mapEither)
import qualified Data.Time as T
import qualified Data.Set as S
import qualified Control.Lens as LS
import Data.List
import qualified Data.DList as DL
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
import Control.Monad
import Control.Monad.Writer
import Control.Monad.Loops (allM,anyM)
import Control.Applicative (liftA2)

import Debug.Trace
import Cashflow (buildBegTsRow)
import Assumptions (NonPerfAssumption(NonPerfAssumption),lookupRate0)
import Asset ()
import Pool (issuanceStat)
import qualified Types as P
import Control.Lens hiding (element)
import Control.Lens.TH
import Data.Either.Utils
import InterestRate (calcInt)
import Liability (getDayCountFromInfo,getTxnRate)
import Hedge (RateCap(..),RateSwapBase(..),RateSwap(rsRefBalance))
import qualified Hedge as HE

debug = flip trace

-- ^ update bond interest rate from rate assumption
setBondNewRate :: Ast.Asset a => TestDeal a -> Date -> [RateAssumption] -> L.Bond -> Either String L.Bond
setBondNewRate t d ras b@(L.Bond _ _ L.OriginalInfo{ L.originDate = od} ii _ bal currentRate _ dueInt _ Nothing _ _ _)
  = setBondNewRate t d ras b {L.bndDueIntDate = Just od}

-- ^ Floater rate
setBondNewRate t d ras b@(L.Bond _ _ _ ii@(L.Floater br idx _spd rset dc mf mc) _ bal currentRate _ dueInt _ (Just dueIntDate) _ _ _)
  = Right $ (L.accrueInt d b){ L.bndRate = applyFloatRate ii d ras }

-- ^ Fix rate, do nothing
setBondNewRate t d ras b@(L.Bond _ _ _ L.Fix {} _ bal currentRate _ dueInt _ (Just dueIntDate) _ _ _)
  = Right b

-- ^ Ref rate
setBondNewRate t d ras b@(L.Bond _ _ _ (L.RefRate sr ds factor _) _ bal currentRate _ dueInt _ (Just dueIntDate) _ _ _) 
  = do
      let b' = L.accrueInt d b
      rate <- queryCompound t d (patchDateToStats d ds)
      return b' {L.bndRate = fromRational (rate * toRational factor) }

-- ^ cap & floor & IoI
setBondNewRate t d ras b@(L.Bond _ _ _ ii _ bal currentRate _ dueInt _ (Just dueIntDate) _ _ _) 
  = Right $ (L.accrueInt d b) { L.bndRate = applyFloatRate ii d ras}

-- ^ bond group
setBondNewRate t d ras bg@(L.BondGroup bMap pt)
  = do 
      m <- mapM (setBondNewRate t d ras) bMap
      return $ L.BondGroup m pt

-- ^ apply all rates for multi-int bond
setBondNewRate t d ras b@(L.MultiIntBond bn _ _ iis _ bal currentRates _ dueInts dueIoIs _ _ _ _)
  = let 
      newRates = applyFloatRate <$> iis <*> pure d <*> pure ras
      b' = L.accrueInt d b -- `debug` ("accrue due to new rate "++ bn)
    in
      Right $ b' { L.bndRates = newRates } 

setBondNewRate t d ras b = Left $ "set bond new rate: "++ show d ++"Failed to set bond rate: "++show b++"from rate assumption" ++ show ras


setBondStepUpRate :: Ast.Asset a => TestDeal a -> Date -> [RateAssumption] -> L.Bond -> Either String L.Bond
setBondStepUpRate t d ras b@(L.Bond _ _ _ ii (Just sp) _ _ _ _ _ _ _ _ _)
  = Right $ 
      let 
        newII = L.stepUpInterestInfo sp ii
        newRate = applyFloatRate ii d ras
      in 
        (L.accrueInt d b) { L.bndInterestInfo = newII, L.bndRate = newRate }

setBondStepUpRate t d ras b@(L.MultiIntBond bn _ _ iis (Just sps) _ _ _ _ _ _ _ _ _)
  = Right $ 
      let 
        newIIs = zipWith L.stepUpInterestInfo sps iis
        newRates = (\x -> applyFloatRate x d ras) <$> newIIs
      in 
        (L.accrueInt d b) { L.bndInterestInfos = newIIs, L.bndRates = newRates }  -- `debug` (show d ++ ">> accure due to step up rate "++ bn)

setBondStepUpRate t d ras bg@(L.BondGroup bMap pt)
  = do 
      m <- mapM (setBondStepUpRate t d ras) bMap
      return $ L.BondGroup m pt



updateSrtRate :: Ast.Asset a => TestDeal a -> Date -> [RateAssumption] -> HE.SRT -> Either String HE.SRT
updateSrtRate t d ras srt@HE.SRT{HE.srtPremiumType = rt} 
    = do 
        r <- applyFloatRate2 rt d ras 
        return srt { HE.srtPremiumRate = r }


accrueSrt :: Ast.Asset a => TestDeal a -> Date -> HE.SRT -> Either String HE.SRT
accrueSrt t d srt@HE.SRT{ HE.srtDuePremium = duePrem, HE.srtRefBalance = bal, HE.srtPremiumRate = rate
                        , HE.srtDuePremiumDate = mDueDate,  HE.srtType = st
                        , HE.srtStart = sd } 
  = do 
      newBal <- case st of
                  HE.SrtByEndDay ds dp -> queryCompound t d (patchDateToStats d ds)
                  _ -> Left "not support new bal type for Srt"
      let newPremium = duePrem +  calcInt (fromRational newBal) (fromMaybe sd mDueDate) d rate DC_ACT_365F
      let accrueInt = calcInt (HE.srtRefBalance srt + duePrem) (fromMaybe d (HE.srtDuePremiumDate srt)) d (HE.srtPremiumRate srt) DC_ACT_365F
      return srt { HE.srtRefBalance = fromRational newBal, HE.srtDuePremium = newPremium, HE.srtDuePremiumDate = Just d}


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
applyFloatRate (L.WithIoI ii _) d ras = applyFloatRate ii d ras

applyFloatRate2 :: IR.RateType -> Date -> [RateAssumption] -> Either String IRate
applyFloatRate2 (IR.Fix _ r) _ _ = Right r
applyFloatRate2 (IR.Floater _ idx spd _r _ mFloor mCap mRounding) d ras
  = let 
      flooring (Just f) v = max f v 
      flooring Nothing v = v 
      capping (Just f) v = min f v 
      capping Nothing  v = v 
    in 
      do 
        rateAtDate <- AP.lookupRate0 ras idx d 
        return $ flooring mFloor $ capping mCap $ rateAtDate + spd

updateRateSwapRate :: Ast.Asset a => TestDeal a -> Maybe [RateAssumption] -> Date -> HE.RateSwap -> Either String HE.RateSwap
updateRateSwapRate t Nothing _ _ = Left "Failed to update rate swap: No rate input assumption"
updateRateSwapRate t (Just rAssumps) d rs@HE.RateSwap{ HE.rsType = rt } 
  = let 
      getRate x = AP.lookupRate rAssumps x d
    in
      do  
        (pRate,rRate) <- case rt of 
                              HE.FloatingToFloating flter1 flter2 ->
                                do 
                                  r1 <- getRate flter1
                                  r2 <- getRate flter2
                                  return (r1, r2)
                              HE.FloatingToFixed flter r -> 
                                do 
                                  _r <- getRate flter
                                  return (_r, r)
                              HE.FixedToFloating r flter -> 
                                do 
                                  _r <- getRate flter
                                  return (r, _r)
                              HE.FormulaToFloating ds flter -> 
                                do 
                                  _r <- queryCompound t d (patchDateToStats d ds)
                                  r <- getRate flter
                                  return (fromRational _r, r)
                              HE.FloatingToFormula flter ds -> 
                                do 
                                  r <- getRate flter
                                  _r <- queryCompound t d (patchDateToStats d ds)
                                  return (r, fromRational _r)
        return rs {HE.rsPayingRate = pRate, HE.rsReceivingRate = rRate }

updateRateSwapBal :: Ast.Asset a => TestDeal a -> Date -> HE.RateSwap -> Either String HE.RateSwap
updateRateSwapBal t d rs@HE.RateSwap{ HE.rsNotional = base }
  =  case base of 
        HE.Fixed _ -> Right rs  
        HE.Schedule ts -> Right $ rs { HE.rsRefBalance = fromRational (getValByDate ts Inc d) }
        HE.Base ds -> 
            do 
              v <- queryCompound t d (patchDateToStats d ds) 
              return rs { HE.rsRefBalance = fromRational v} -- `debug` ("query Result"++ show (patchDateToStats d ds) )

-- ^ accure rate cap 
accrueRC :: Ast.Asset a => TestDeal a -> Date -> [RateAssumption] -> RateCap -> Either String RateCap
accrueRC t d rs rc@RateCap{rcNetCash = amt, rcStrikeRate = strike,rcIndex = index
                        ,rcStartDate = sd, rcEndDate = ed, rcNotional = notional
                        ,rcLastStlDate = mlsd
                        ,rcStmt = mstmt} 
  | d > ed || d < sd = Right rc 
  | otherwise = do
                  r <- lookupRate0 rs index d
                  balance <- case notional of
                               Fixed bal -> Right . toRational $ bal
                               Base ds -> queryCompound t d (patchDateToStats d ds)
                               Schedule ts -> Right $ getValByDate ts Inc d

                  let accRate = max 0 $ r - fromRational (getValByDate strike Inc d) -- `debug` ("Rate from curve"++show (getValByDate strike Inc d))
                  let addAmt = case mlsd of 
                                 Nothing -> calcInt (fromRational balance) sd d accRate DC_ACT_365F
                                 Just lstD -> calcInt (fromRational balance) lstD d accRate DC_ACT_365F

                  let newAmt = amt + addAmt  -- `debug` ("Accrue AMT"++ show addAmt)
                  let newStmt = appendStmt (IrsTxn d newAmt addAmt 0 0 0 SwapAccrue) mstmt 
                  return $ rc { rcLastStlDate = Just d ,rcNetCash = newAmt, rcStmt = newStmt }

-- ^ test if a clean up call should be fired
testCall :: Ast.Asset a => TestDeal a -> Date -> C.CallOption -> Either String Bool 
testCall t d opt = 
    case opt of 
       C.PoolBalance x -> (< x) <$> fromRational <$> queryCompound t d (FutureCurrentPoolBalance Nothing)
       C.BondBalance x -> (< x) <$> fromRational <$> queryCompound t d CurrentBondBalance
       C.PoolFactor x ->  (< x) <$> queryCompound t d (FutureCurrentPoolFactor d Nothing)  -- `debug` ("D "++show d++ "Pool Factor query ->" ++ show (queryDealRate t (FutureCurrentPoolFactor d)))
       C.BondFactor x ->  (< x) <$> queryCompound t d BondFactor
       C.OnDate x -> Right $ x == d 
       C.AfterDate x -> Right $ d > x
       C.And xs -> allM (testCall t d) xs
       C.Or xs -> anyM (testCall t d) xs
       -- C.And xs -> (all id) <$> sequenceA $ [testCall t d x | x <- xs]
       -- C.Or xs -> (any id) <$> sequenceA $ [testCall t d x | x <- xs]
       C.Pre pre -> testPre d t pre
       _ -> Left ("failed to find call options"++ show opt)


queryTrigger :: Ast.Asset a => TestDeal a -> DealCycle -> [Trigger]
queryTrigger t@TestDeal{ triggers = trgs } wt 
  = case trgs of 
      Nothing -> []
      Just _trgs -> maybe [] Map.elems $ Map.lookup wt _trgs

-- ^ execute effects of trigger: making changes to deal
-- TODO seems position of arugments can be changed : f :: a -> b -> m a  => f:: b -> a -> m a
runEffects :: Ast.Asset a => (TestDeal a, RunContext a, [ActionOnDate], DL.DList ResultComponent) -> Date -> TriggerEffect 
           -> Either String (TestDeal a, RunContext a, [ActionOnDate], DL.DList ResultComponent)
runEffects (t@TestDeal{accounts = accMap, fees = feeMap ,status=st, bonds = bondMap, pool=pt
                      ,collects = collRules}, rc, actions, logs) d te
  = case te of 
      DealStatusTo _ds -> Right (t {status = _ds}, rc, actions, logs)
      DoAccrueFee fns -> do
                           newFeeList <- sequenceA $ calcDueFee t d  <$> (feeMap Map.!) <$> fns
                           let newFeeMap = Map.fromList (zip fns newFeeList) <> feeMap
                           return (t {fees = newFeeMap}, rc, actions, logs)
      ChangeReserveBalance accName rAmt ->
          Right (t {accounts = Map.adjust (set A.accTypeLens (Just rAmt)) accName accMap }
                    , rc, actions, logs)
      
      TriggerEffects efs -> foldM (`runEffects` d) (t, rc, actions, logs) efs
      
      RunActions wActions -> do
                              (newT, newRc, newLogs) <- foldM (performActionWrap d) (t, rc, DL.empty) wActions
                              return (newT, newRc, actions, DL.append logs newLogs)

      DoNothing -> Right (t, rc, actions, DL.empty)
      _ -> Left $ "Date:"++ show d++" Failed to match trigger effects: "++show te

-- ^ test triggers in the deal and add a log if deal status changed
runTriggers :: Ast.Asset a => (TestDeal a, RunContext a, [ActionOnDate]) -> Date -> DealCycle -> Either String (TestDeal a, RunContext a, [ActionOnDate], DL.DList ResultComponent)
runTriggers (t@TestDeal{status=oldStatus, triggers = Nothing},rc, actions) d dcycle = Right (t, rc, actions, DL.empty)
runTriggers (t@TestDeal{status=oldStatus, triggers = Just trgM},rc, actions) d dcycle = 
  do
    let trgsMap = Map.findWithDefault Map.empty dcycle trgM
    let trgsToTest = Map.filter   
                           (\trg -> (not (trgStatus trg) || trgStatus trg && trgCurable trg))
                           trgsMap
    triggeredTrgs <- mapM (testTrigger t d) trgsToTest
    let triggeredEffects = [ trgEffects _trg | _trg <- Map.elems triggeredTrgs, (trgStatus _trg) ] 
    (newDeal, newRc, newActions, logsFromTrigger) <- foldM (`runEffects` d) (t,rc,actions, DL.empty) triggeredEffects
    let newStatus = status newDeal 
    let newLogs = DL.fromList [DealStatusChangeTo d oldStatus newStatus "By trigger"|  newStatus /= oldStatus] -- `debug` (">>"++show d++"trigger : new st"++ show newStatus++"old st"++show oldStatus)
    let newTriggers = Map.union triggeredTrgs trgsMap
    return (newDeal {triggers = Just (Map.insert dcycle newTriggers trgM)}
           , newRc
           , newActions
           , DL.append newLogs logsFromTrigger) -- `debug` ("New logs from trigger"++ show d ++">>>"++show newLogs)


changeDealStatus:: Ast.Asset a => (Date,String)-> DealStatus -> TestDeal a -> (Maybe ResultComponent, TestDeal a)
-- ^ no status change for deal already ended 
changeDealStatus _ _ t@TestDeal{status=Ended} = (Nothing, t) 
changeDealStatus (d,why) newSt t@TestDeal{status=oldSt} = (Just (DealStatusChangeTo d oldSt newSt why), t {status=newSt})



run :: Ast.Asset a => TestDeal a -> Map.Map PoolId CF.CashFlowFrame -> Maybe [ActionOnDate] -> Maybe [RateAssumption] -> Maybe ([Pre],[Pre])
        -> Maybe (Map.Map String (RevolvingPool,AP.ApplyAssumptionType))-> DL.DList ResultComponent -> Either String (TestDeal a,DL.DList ResultComponent)
run t@TestDeal{status=Ended} pCfM ads _ _ _ log  = Right (prepareDeal t,(DL.snoc log (EndRun Nothing "By Status:Ended")))
run t pCfM (Just []) _ _ _ log  = Right (prepareDeal t,(DL.snoc log (EndRun Nothing "No Actions")))
run t pCfM (Just [HitStatedMaturity d]) _ _ _ log  = Right (prepareDeal t, (DL.snoc log (EndRun (Just d) "Stop: Stated Maturity")))
run t pCfM (Just (StopRunFlag d:_)) _ _ _ log  = Right (prepareDeal t, (DL.snoc log (EndRun (Just d) "Stop Run Flag")))
run t@TestDeal{accounts=accMap,fees=feeMap,triggers=mTrgMap,bonds=bndMap,status=dStatus
              ,waterfall=waterfallM,name=dealName,pool=pt,stats=_stat} 
    poolFlowMap (Just (ad:ads)) rates calls rAssump log
  | all (== 0) futureCashToCollect && (queryCompound t (getDate ad) AllAccBalance == Right 0) && (dStatus /= Revolving) && (dStatus /= Warehousing Nothing) --TODO need to use prsim here to cover all warehouse status
     = do 
        let runContext = RunContext poolFlowMap rAssump rates
        (finalDeal,_,newLogs) <- foldM (performActionWrap (getDate ad)) (t,runContext,log) cleanUpActions 
        return (prepareDeal finalDeal, (DL.snoc newLogs (EndRun (Just (getDate ad)) "No Pool Cashflow/All Account is zero/Not revolving"))) -- `debug` ("End of pool collection with logs with length "++ show (length log))

  | otherwise
    = case ad of 
        PoolCollection d _ ->
          if any (> 0) remainCollectionNum then
            let 
              cutOffPoolFlowMap = Map.map (\pflow -> CF.splitCashFlowFrameByDate pflow d EqToLeft) poolFlowMap 
              collectedFlow =  Map.map fst cutOffPoolFlowMap  -- `debug` ("PoolCollection : "++ show d ++  " splited"++ show cutOffPoolFlowMap++"\n input pflow"++ show poolFlowMap)
              -- outstandingFlow = Map.map (CF.insertBegTsRow d . snd) cutOffPoolFlowMap
              outstandingFlow = Map.map snd cutOffPoolFlowMap
              -- deposit cashflow to SPV from external pool cf               
            in 
              do 
                accs <- depositPoolFlow (collects t) d collectedFlow accMap -- `debug` ("PoolCollection: deposit >>"++ show d++">>>"++ show collectedFlow++"\n")
                let dAfterDeposit = (appendCollectedCF d t collectedFlow) {accounts=accs}   -- `debug` ("Collected flow"++ show collectedFlow)
                -- newScheduleFlowMap = Map.map (over CF.cashflowTxn (cutBy Exc Future d)) (fromMaybe Map.empty (getScheduledCashflow t Nothing))
                let dealAfterUpdateScheduleFlow = over dealScheduledCashflow 
                                                    (Map.map (\mflow -> over CF.cashflowTxn (cutBy Exc Future d) <$> mflow))
                                                    dAfterDeposit 
                let runContext = RunContext outstandingFlow rAssump rates  -- `debug` ("PoolCollection: before rc >>"++ show d++">>>"++ show (pool dAfterDeposit))
                (dRunWithTrigger0, rc1, ads2, newLogs0) <- runTriggers (dealAfterUpdateScheduleFlow,runContext,ads) d EndCollection  -- `debug` ("PoolCollection: after update schedule flow >>"++ show d++">>"++show (pool dealAfterUpdateScheduleFlow))
                let eopActionsLog = DL.fromList [ RunningWaterfall d W.EndOfPoolCollection | Map.member W.EndOfPoolCollection waterfallM ] -- `debug` ("new logs from trigger 1"++ show newLogs0)
                let waterfallToExe = Map.findWithDefault [] W.EndOfPoolCollection (waterfall t)  -- `debug` ("new logs from trigger 1"++ show newLogs0)
                (dAfterAction,rc2,newLogs) <- foldM (performActionWrap d) (dRunWithTrigger0 ,rc1 ,log ) waterfallToExe  -- `debug` ("Pt 03"++ show d++">> context flow"++show (pool dRunWithTrigger0))-- `debug` ("End collection action"++ show waterfallToExe)
                (dRunWithTrigger1,rc3,ads3,newLogs1) <- runTriggers (dAfterAction,rc2,ads2) d EndCollectionWF -- `debug` ("PoolCollection: Pt 04"++ show d++">> context flow"++show (runPoolFlow rc2))-- `debug` ("End collection action"++ show waterfallToExe)
                run (increasePoolCollectedPeriod dRunWithTrigger1 )
                    (runPoolFlow rc3) 
                    (Just ads3) 
                    rates 
                    calls 
                    rAssump 
                    (DL.concat [newLogs0,newLogs,eopActionsLog,newLogs1]) -- `debug` ("PoolCollection: Pt 05>> "++ show d++">> context flow>> "++show (runPoolFlow rc3))
          else
            run t poolFlowMap (Just ads) rates calls rAssump log -- `debug` ("PoolCollection: hit zero pool length"++ show d++"pool"++ (show poolFlowMap)++"collected cf"++ show pt) 

        RunWaterfall d "" -> 
          let
            runContext = RunContext poolFlowMap rAssump rates
            waterfallKey = if Map.member (W.DistributionDay dStatus) waterfallM then 
                              W.DistributionDay dStatus
                            else 
                              W.DefaultDistribution
                        
            waterfallToExe = Map.findWithDefault [] waterfallKey waterfallM
            callTest = fst $ fromMaybe ([]::[Pre],[]::[Pre]) calls
          in 
            do 
              (dRunWithTrigger0, rc1, ads1, newLogs0) <- runTriggers (t, runContext, ads) d BeginDistributionWF -- `debug` ("In RunWaterfall Date"++show d++"before run trigger>> collected"++ show (pool t))
              let logsBeforeDist = DL.concat [newLogs0 , DL.fromList [ WarningMsg (" No waterfall distribution found on date "++show d++" with waterfall key "++show waterfallKey) 
                                 | Map.notMember waterfallKey waterfallM ] ]
              flag <- anyM (testPre d dRunWithTrigger0) callTest -- `debug` ( "In RunWaterfall status after before waterfall trigger >>"++ show (status dRunWithTrigger0) )
              if flag then
                do
                  let newStLogs = if null cleanUpActions then 
                                    [DealStatusChangeTo d dStatus Called "Call by triggers before waterfall distribution"]
                                  else 
                                    [DealStatusChangeTo d dStatus Called "Call by triggers before waterfall distribution", RunningWaterfall d W.CleanUp]
                  (dealAfterCleanUp, rc_, newLogWaterfall_ ) <- foldM (performActionWrap d) (dRunWithTrigger0, rc1,log) cleanUpActions 
                  endingLogs <- Rpt.patchFinancialReports dealAfterCleanUp d newLogWaterfall_
                  return (prepareDeal dealAfterCleanUp, DL.concat [logsBeforeDist,DL.fromList (newStLogs++[EndRun (Just d) "Clean Up"]),endingLogs]) -- `debug` ("Called ! "++ show d)
              else
                do
                  (dAfterWaterfall, rc2, newLogsWaterfall) <- foldM (performActionWrap d) (dRunWithTrigger0,rc1,log) waterfallToExe -- `debug` ("In RunWaterfall Date"++show d++">>> status "++show (status dRunWithTrigger0)++"before run waterfall collected >>"++ show (pool dRunWithTrigger0))
                  (dRunWithTrigger1, rc3, ads2, newLogs2) <- runTriggers (dAfterWaterfall,rc2,ads1) d EndDistributionWF  -- `debug` ("In RunWaterfall Date"++show d++"after run waterfall >>"++ show (runPoolFlow rc2)++" collected >>"++ show (pool dAfterWaterfall))
                  run (increaseBondPaidPeriod dRunWithTrigger1)
                      (runPoolFlow rc3) 
                      (Just ads2) 
                      rates 
                      calls 
                      rAssump 
                      (DL.concat [newLogsWaterfall, newLogs2 ,logsBeforeDist,DL.fromList [RunningWaterfall d waterfallKey]]) -- `debug` ("In RunWaterfall Date"++show d++"after run waterfall 3>>"++ show (pool dRunWithTrigger1)++" status>>"++ show (status dRunWithTrigger1))

        -- Custom waterfall execution action from custom dates
        RunWaterfall d wName -> 
          let
            runContext = RunContext poolFlowMap rAssump rates
            waterfallKey = W.CustomWaterfall wName
          in 
            do
              waterfallToExe <- maybeToEither
                                  ("No waterfall distribution found on date "++show d++" with waterfall key "++show waterfallKey) $
                                  Map.lookup waterfallKey waterfallM
              let logsBeforeDist =[ WarningMsg (" No waterfall distribution found on date "++show d++" with waterfall key "++show waterfallKey) 
                                        | Map.notMember waterfallKey waterfallM ]  
              (dAfterWaterfall, rc2, newLogsWaterfall) <- foldM (performActionWrap d) (t,runContext,log) waterfallToExe -- `debug` (show d ++ " running action"++ show waterfallToExe)
              run dAfterWaterfall (runPoolFlow rc2) (Just ads) rates calls rAssump 
                  (DL.concat [newLogsWaterfall,DL.fromList (logsBeforeDist ++ [RunningWaterfall d waterfallKey])]) -- `debug` ("size of logs"++ show (length newLogsWaterfall)++ ">>"++ show d++ show (length logsBeforeDist))

        EarnAccInt d accName ->
          let 
            newAcc = Map.adjust (A.depositInt d) accName accMap
          in 
            run (t {accounts = newAcc}) poolFlowMap (Just ads) rates calls rAssump log

        AccrueFee d feeName -> 
          do 
            fToAcc <- maybeToEither 
                        ("Failed to find fee "++feeName)
                        (Map.lookup feeName feeMap)
            newF <- calcDueFee t d fToAcc
            let newFeeMap = (Map.fromList [(feeName,newF)]) <> feeMap
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
            w = Map.findWithDefault [] W.OnClosingDay (waterfall t) 
            rc = RunContext poolFlowMap rAssump rates  
            logForClosed =  [RunningWaterfall d W.OnClosingDay| not (null w)]
          in 
            do
              newSt <- case dStatus of
                         (PreClosing st) -> Right st
                         _ -> Left $ "DealClosed action is not in PreClosing status but got"++ show dStatus
              (newDeal, newRc, newLog) <- foldM (performActionWrap d) (t, rc, log) w  -- `debug` ("ClosingDay Action:"++show w)
              run newDeal{status=newSt} (runPoolFlow newRc) (Just ads) rates calls rAssump 
                  (DL.concat [newLog, DL.fromList ([DealStatusChangeTo d (PreClosing newSt) newSt "By Deal Close"]++logForClosed)]) -- `debug` ("new st at closing"++ show newSt)

        ChangeDealStatusTo d s -> run (t{status=s}) poolFlowMap (Just ads) rates calls rAssump log

        CalcIRSwap d sn -> 
          case rateSwap t of 
            Nothing -> Left $ " No rate swaps modeled when looking for "++ sn
            Just rSwap ->
              do
                newRateSwap_rate <- adjustM (updateRateSwapRate t rates d) sn rSwap
                newRateSwap_bal <- adjustM (updateRateSwapBal t d) sn newRateSwap_rate 
                let newRateSwap_acc = Map.adjust (HE.accrueIRS d) sn $ newRateSwap_bal
                run (t{rateSwap = Just newRateSwap_acc}) poolFlowMap (Just ads) rates calls rAssump log

        SettleIRSwap d sn -> 
          case rateSwap t of 
            Nothing -> Left $ " No rate swaps modeled when looking for "++ sn
            Just rSwap ->
              do
                acc <- case HE.rsSettleDates (rSwap Map.! sn) of 
                          Nothing -> Left $ "No settle date found for "++ sn
                          Just (_, _accName) -> Right $ accMap Map.! _accName
                let accBal = A.accBalance acc
                let rs = rSwap Map.! sn
                let settleAmt = HE.rsNetCash rs
                let accName = A.accName acc
                case (settleAmt <0, accBal < abs settleAmt) of 
                  (True, True) ->
                    let
                      newAcc = Map.adjust (A.draw accBal d (SwapOutSettle sn)) accName accMap
                      newRsMap = Just $ Map.adjust (HE.payoutIRS d accBal) sn rSwap 
                    in 
                      run (t{accounts = newAcc, rateSwap = newRsMap}) poolFlowMap (Just ads) rates calls rAssump
                      $ DL.snoc log (WarningMsg $ "Settle Rate Swap Error: "++ show d ++" Insufficient balance to settle "++ sn)
                    -- Left $ "Settle Rate Swap Error: "++ show d ++" Insufficient balance to settle "++ sn
                  (True, False) -> 
                    let
                      newAcc = Map.adjust (A.draw (abs settleAmt) d (SwapOutSettle sn)) accName  accMap
                      newRsMap = Just $ Map.adjust (HE.payoutIRS d settleAmt) sn rSwap 
                    in 
                      run (t{accounts = newAcc, rateSwap = newRsMap}) poolFlowMap (Just ads) rates calls rAssump log
                  (False, _) -> 
                    let 
                      newAcc = Map.adjust (A.deposit settleAmt d (SwapInSettle sn)) accName accMap
                      newRsMap = Just $ Map.adjust (HE.receiveIRS d) sn rSwap 
                    in
                      run (t{accounts = newAcc, rateSwap = newRsMap}) poolFlowMap (Just ads) rates calls rAssump log

        AccrueCapRate d cn -> 
          case rateCap t of 
            Nothing -> Left $ " No rate cap found for "++ cn
            Just rCap ->
              let
                _rates = fromMaybe [] rates
              in 
                do 
                  newRateCap <- adjustM (accrueRC t d _rates) cn rCap
                  run (t{rateCap = Just newRateCap}) poolFlowMap (Just ads) rates calls rAssump log

        InspectDS d dss -> 
          do
            newlog <- inspectListVars t d dss 
            run t poolFlowMap (Just ads) rates calls rAssump $ DL.append log (DL.fromList newlog) -- `debug` ("Add log"++show newlog)
        
        ResetBondRate d bn  -> 
          let 
            rateList = fromMaybe [] rates
            bnd = bndMap Map.! bn
          in 
            do 
              newBnd <- setBondNewRate t d rateList bnd 
              run t{bonds = Map.fromList [(bn,newBnd)] <> bndMap} poolFlowMap (Just ads) rates calls rAssump log
        
        StepUpBondRate d bn -> 
          let 
            bnd = bndMap Map.! bn -- `debug` ("StepUpBondRate--------------"++ show bn)
          in 
            do 
              -- newBnd <- setBondStepUpRate t d bnd `debug` ("StepUpBondRate"++ show d++ show bn)
              newBndMap <- adjustM (setBondStepUpRate t d (fromMaybe [] rates)) bn bndMap
              run t{bonds = newBndMap } poolFlowMap (Just ads) rates calls rAssump log
        
        ResetAccRate d accName -> 
          do
            newAccMap <- adjustM 
                          (\a@(A.Account _ _ (Just (A.InvestmentAccount idx spd dp dp1 lastDay _)) _ _)
                            -> do
                                 newRate <- AP.lookupRate (fromMaybe [] rates) (idx,spd) d 
                                 let accWithNewInt = A.depositInt d a
                                 return accWithNewInt { A.accInterest = Just (A.InvestmentAccount idx spd dp dp1 lastDay newRate)})
                          accName accMap
            run t{accounts = newAccMap} poolFlowMap (Just ads) rates calls rAssump log

        BuildReport sd ed ->
          let 
            cashReport = Rpt.buildCashReport t sd ed 
          in 
            do 
              bsReport <- Rpt.buildBalanceSheet t ed
              let newlog = FinancialReport sd ed bsReport cashReport
              run t poolFlowMap (Just ads) rates calls rAssump $ DL.snoc log newlog -- `debug` ("new log"++ show ed++ show newlog)

        FireTrigger d cyc n -> 
          let 
            triggerFired = case mTrgMap of 
                               Nothing -> error "trigger is empty for override" 
                               Just tm -> Map.adjust (Map.adjust (set trgStatusLens True) n) cyc tm
            triggerEffects = do
                                tm <- mTrgMap
                                cycM <- Map.lookup cyc tm
                                trg <- Map.lookup n cycM
                                return $ trgEffects trg
            
            runContext = RunContext poolFlowMap rAssump rates
          in 
            do 
              (newT, rc@(RunContext newPool _ _), adsFromTrigger, newLogsFromTrigger) 
                <- case triggerEffects of 
                    Nothing -> Right (t, runContext, ads, DL.empty) -- `debug` "Nothing found on effects"
                    Just efs -> runEffects (t, runContext, ads, DL.empty) d efs
              let (oldStatus,newStatus) = (status t,status newT)
              let stChangeLogs = DL.fromList [DealStatusChangeTo d oldStatus newStatus "by Manual fireTrigger" |  oldStatus /= newStatus] 
              run newT {triggers = Just triggerFired} newPool (Just ads) rates calls rAssump $ DL.concat [log,stChangeLogs,newLogsFromTrigger]
        
        MakeWhole d spd walTbl -> 
            let 
              schedulePoolFlowMap = Map.map (fromMaybe (CF.CashFlowFrame (0,epocDate,Nothing) []))  $ view dealScheduledCashflow t
            in 
              do 
                factor <- liftA2
                            (/)
                            (queryCompound t d (FutureCurrentPoolBegBalance Nothing)) 
                            (queryCompound t d (FutureCurrentSchedulePoolBegBalance Nothing))
                let reduceCfs = Map.map (over CF.cashflowTxn (\xs -> (CF.scaleTsRow factor) <$> xs)) schedulePoolFlowMap -- need to apply with factor and trucate with date
                (runDealWithSchedule,_) <- run t reduceCfs (Just ads) rates calls rAssump $ log
                let bondWal = Map.map (L.calcWalBond d) (bonds runDealWithSchedule) -- `debug` ("Bond schedule flow"++ show (bonds runDealWithSchedule))
                let bondSprd = Map.map 
                                 (\x -> (spd + (fromMaybe 0 (lookupTable walTbl Up (fromRational x >)))))
                                 bondWal 
                let bondPricingCurve = Map.map 
                                         (\x -> IRateCurve [ TsPoint d x,TsPoint (getDate (last ads)) x] )
                                         bondSprd 
                let bondPricingResult = Map.intersectionWithKey (\k v1 v2 -> L.priceBond d v2 v1) (bonds runDealWithSchedule) bondPricingCurve 
                let depositBondFlow = Map.intersectionWith
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
                run t {bonds = depositBondFlow, status = Ended } poolFlowMap (Just []) rates calls rAssump $ DL.snoc log (EndRun (Just d) "MakeWhole call")
        
        FundBond d Nothing bName accName fundAmt ->
          let 
            newAcc = Map.adjust (A.deposit fundAmt d (FundWith bName fundAmt)) accName accMap
          in 
            do
              let bndFunded = L.fundWith d fundAmt $ bndMap Map.! bName
              run t{accounts = newAcc, bonds = Map.insert bName bndFunded bndMap}
                  poolFlowMap (Just ads) rates calls rAssump log

        FundBond d (Just p) bName accName fundAmt ->
          let 
            newAcc = Map.adjust (A.deposit fundAmt d (FundWith bName fundAmt)) accName accMap
          in 
            do
              flag <- testPre d t p
              case flag of
                False -> run t poolFlowMap (Just ads) rates calls rAssump (DL.snoc log (WarningMsg ("Failed to fund bond"++ bName++ ":" ++show p)))
                True -> 
                  do
                    let bndFunded = L.fundWith d fundAmt $ bndMap Map.! bName
                    run t{accounts = newAcc, bonds = Map.insert bName bndFunded bndMap}
                        poolFlowMap (Just ads) rates calls rAssump log
          

        IssueBond d Nothing bGroupName accName bnd mBal mRate -> 
           run t poolFlowMap (Just ((IssueBond d (Just (Always True)) bGroupName accName bnd mBal mRate):ads)) rates calls rAssump log
        
        IssueBond d (Just p) bGroupName accName bnd mBal mRate ->
            do 
              flag <- testPre d t p
              case flag of
                False -> run t poolFlowMap (Just ads) rates calls rAssump (DL.snoc log (WarningMsg ("Failed to issue to bond group"++ bGroupName++ ":" ++show p)))
                True -> let 
                          newBndName = L.bndName bnd
                        in
                           do
                             newBalance <- case mBal of
                                             Just _q -> queryCompound t d (patchDateToStats d _q)  
                                             Nothing -> Right . toRational $ L.originBalance (L.bndOriginInfo bnd)
                             newRate <- case mRate of 
                                         Just _q -> queryCompound t d (patchDateToStats d _q)
                                         Nothing -> Right $ L.originRate (L.bndOriginInfo bnd)
                             let newBonds = case Map.lookup bGroupName bndMap of
                                              Nothing -> bndMap
                                              Just L.Bond {} -> bndMap
                                              Just (L.BondGroup bndGrpMap pt) -> let
                                                                                bndOInfo = (L.bndOriginInfo bnd) {L.originDate = d, L.originRate = newRate, L.originBalance = fromRational newBalance }
                                                                                bndToInsert = bnd {L.bndOriginInfo = bndOInfo,
                                                                                                   L.bndDueIntDate = Just d,
                                                                                                   L.bndLastIntPay = Just d, 
                                                                                                   L.bndLastPrinPay = Just d,
                                                                                                   L.bndRate = fromRational newRate,
                                                                                                   L.bndBalance = fromRational newBalance}
                                                                              in 
                                                                                Map.insert bGroupName 
                                                                                           (L.BondGroup (Map.insert newBndName bndToInsert bndGrpMap) pt)
                                                                                           bndMap

                             let issuanceProceeds = fromRational newBalance
                             let newAcc = Map.adjust (A.deposit issuanceProceeds d (IssuanceProceeds newBndName))
                                                     accName
                                                     accMap
                             run t{bonds = newBonds, accounts = newAcc} poolFlowMap (Just ads) rates calls rAssump log
        RefiBondRate d accName bName iInfo ->
           let
             -- settle accrued interest 
             -- TODO rebuild bond rate reset actions
             lstDate = getDate (last ads)
             isResetActionEvent (ResetBondRate _ bName ) = False 
             isResetActionEvent _ = True
             filteredAds = filter isResetActionEvent ads
             newRate = L.getBeginRate iInfo
          in 
             do 
               nBnd <- calcDueInt t d $ bndMap Map.! bName
               let dueIntToPay = L.getTotalDueInt nBnd
               let ((shortfall,drawAmt),newAcc) = A.tryDraw dueIntToPay d (PayInt [bName]) (accMap Map.! accName)
               let newBnd = set L.bndIntLens iInfo $ L.payInt d drawAmt nBnd
               let resetDates = L.buildRateResetDates newBnd d lstDate 
               -- let bResetActions = [ ResetBondRate d bName 0 | d <- resetDates ]
               -- TODO tobe fix
               let bResetActions = []
               let newAccMap = Map.insert accName newAcc accMap
               let newBndMap = Map.insert bName (newBnd {L.bndRate = newRate, L.bndDueIntDate = Just d 
                                                        ,L.bndLastIntPay = Just d}) bndMap
               let newAds = sortBy sortActionOnDate $ filteredAds ++ bResetActions
               run t{bonds = newBndMap, accounts = newAccMap} poolFlowMap (Just newAds) rates calls rAssump log
           
        RefiBond d accName bnd -> Left "Undefined action: RefiBond"

        TestCall d ->
          let 
            timeBasedTests::[Pre] = snd (fromMaybe ([],[]) calls)
          in
            do 
              flags::[Bool] <- sequenceA $ [ (testPre d t pre) | pre <- timeBasedTests ]
              case any id flags of
                True -> 
                  let 
                     runContext = RunContext poolFlowMap rAssump rates
                     newStLogs = if null cleanUpActions then 
                                   DL.fromList [DealStatusChangeTo d dStatus Called "by Date-Based Call"]
                                 else 
                                   DL.fromList [DealStatusChangeTo d dStatus Called "by Date-Based Call", RunningWaterfall d W.CleanUp]
                  in  
                     do 
                       (dealAfterCleanUp, rc_, newLogWaterfall_ ) <- foldM (performActionWrap d) (t, runContext, log) cleanUpActions
                       endingLogs <- Rpt.patchFinancialReports dealAfterCleanUp d newLogWaterfall_
                       return (prepareDeal dealAfterCleanUp, DL.snoc (endingLogs `DL.append` newStLogs) (EndRun (Just d) "Clean Up")) -- `debug` ("Called ! "++ show d)
                _ -> run t poolFlowMap (Just ads) rates calls rAssump log

        _ -> Left $ "Failed to match action on Date"++ show ad

       where
         cleanUpActions = Map.findWithDefault [] W.CleanUp (waterfall t) -- `debug` ("Running AD"++show(ad))
         remainCollectionNum = Map.elems $ Map.map CF.sizeCashFlowFrame poolFlowMap
         futureCashToCollect = Map.elems $ Map.map (\pcf -> sum (CF.tsTotalCash <$> view CF.cashflowTxn pcf)) poolFlowMap


run t empty Nothing Nothing Nothing Nothing log
  = do
      (t, ads, pcf, unStressPcf) <- getInits t Nothing Nothing 
      run t pcf (Just ads) Nothing Nothing Nothing log  -- `debug` ("Init Done >>Last Action#"++show (length ads)++"F/L"++show (head ads)++show (last ads))

run t empty _ _ _ _ log = Right (prepareDeal t, log) -- `debug` ("End with pool CF is []")



-- reserved for future used
data ExpectReturn = DealPoolFlow
                  | DealPoolFlowPricing   -- ^ default option, return pricing and bond/pool/account/fee etc cashflow
                  | DealTxns
                  | ExecutionSummary
                  deriving (Show,Generic)


-- priceBondIrr :: AP.IrrType -> [Txn] -> Either String (Rate, [(Date,Balance)])
priceBondIrr :: AP.IrrType -> [Txn] -> Either String (Rate, [Txn])
-- No projected transaction, use history cashflow only
priceBondIrr AP.BuyBond {} [] = Left "No transaction to buy the bond" 
priceBondIrr (AP.HoldingBond historyCash _ _) [] 
  = let 
      (ds,vs) = unzip historyCash
      txns' = [ BondTxn d 0 0 0 0 v 0 0 Nothing Types.Empty | (d,v) <- historyCash ]
    in 
      do 
        irr <- Analytics.calcIRR ds vs
        return (irr, txns')
-- Projected transaction and hold to maturity
priceBondIrr (AP.HoldingBond historyCash holding Nothing) txns
  = let 
      begBal = (getTxnBegBalance . head) txns
      holdingPct = divideBB holding begBal
      bProjectedTxn = scaleTxn holdingPct <$> txns -- `debug` ("holding pct"++ show holding ++"/" ++ show begBal ++" : " ++ show holdingPct)
      (ds,vs) = unzip historyCash
      (ds2,vs2) = (getDate <$> bProjectedTxn, getTxnAmt <$> bProjectedTxn) -- `debug` ("projected txn position"++ show bProjectedTxn)
      
      txns' = [ BondTxn d 0 0 0 0 v 0 0 Nothing Types.Empty | (d,v) <- historyCash ]
    in 
      do 
        irr <- Analytics.calcIRR (ds++ds2) (vs++vs2) -- `debug` ("projected holding"++ show (ds2,vs2))
        return (irr, txns' ++ bProjectedTxn)

-- TODO: need to use DC from bond
-- Projected transaction and sell at a Date
priceBondIrr (AP.HoldingBond historyCash holding (Just (sellDate, sellPricingMethod))) txns
  = let 
      -- history cash
      (ds,vs) = unzip historyCash
      txns' = [ BondTxn d 0 0 0 0 v 0 0 Nothing Types.Empty | (d,v) <- historyCash ]
      
      begBal = (getTxnBegBalance . head) txns
      holdingPct = toRational $ holding / begBal
      -- assume cashflow of sell date belongs to seller(owner)
      (bProjectedTxn',futureFlow') = splitByDate txns sellDate EqToLeft
      (bProjectedTxn,futureFlow) = ((scaleTxn holdingPct) <$> bProjectedTxn',(scaleTxn holdingPct) <$> futureFlow')
      -- projected cash
      (ds2,vs2) = (getDate <$> bProjectedTxn, getTxnAmt <$> bProjectedTxn)
      -- accrued interest
      accruedInt = L.backoutAccruedInt sellDate epocDate (bProjectedTxn++futureFlow)
      (ds3,vs3) = (sellDate, accruedInt)  -- `debug` ("accrued interest"++ show (accruedInt,sellDate))
      -- sell price 
      sellPrice = case sellPricingMethod of 
                    BondBalanceFactor f -> case bProjectedTxn of 
                                            [] -> mulBR begBal (f * holdingPct) 
                                            _txns -> mulBR (getTxnBalance (last _txns)) f
      (ds4,vs4) = (sellDate,  sellPrice)  -- `debug` ("sale price, date"++ show (sellPrice,sellDate))
    in 
      do 
        irr <- Analytics.calcIRR (ds++ds2++[ds3]++[ds4]) (vs++vs2++[vs3]++[vs4]) -- `debug` ("vs:"++ show vs++ "vs2:"++ show vs2++ "vs3:"++ show vs3++ "vs4:"++ show vs4 ++">>> ds "++ show ds++ "ds2"++ show ds2++ "ds3"++ show ds3++ "ds4"++ show ds4)
        return (irr, txns'++ bProjectedTxn++ [(BondTxn sellDate 0 vs3 sellPrice 0 (sellPrice+vs3) 0 0 Nothing Types.Empty)]) 

-- Buy and hold to maturity
priceBondIrr (AP.BuyBond dateToBuy bPricingMethod (AP.ByCash cash) Nothing) txns
  | null futureFlow' = Left "No transaction to buy bond"
  | otherwise
    = let 
      -- balance of bond on buy date
      nextTxn = head futureFlow'
      balAsBuyDate = getTxnBegBalance nextTxn
      buyPrice = case bPricingMethod of 
                    BondBalanceFactor f -> mulBR balAsBuyDate f 
      buyPaidOut = min buyPrice cash
      buyPct = divideBB buyPaidOut buyPrice
      boughtTxns = scaleTxn buyPct <$> futureFlow'
      -- buy price (including accrued interest)

      accuredInt = let
                    --TODO what about interest over interest
                    accruedInt' = calcInt balAsBuyDate dateToBuy (getDate nextTxn) (getTxnRate nextTxn) DC_ACT_365F
                    x = nextTxn
                    totalInt' = (fromMaybe 0) <$> [(preview (_BondTxn . _3 ) x), (preview (_BondTxn . _7 ) x), (preview (_BondTxn . _8 ) x)]
                   in
                    sum(totalInt') - accruedInt'

      (ds1, vs1) = (dateToBuy, negate (buyPaidOut + accuredInt))
      (ds2, vs2) = (getDate <$> futureFlow', getTxnAmt <$> boughtTxns)
    in 
      do 
        irr <- Analytics.calcIRR ([ds1]++ds2) ([vs1]++vs2)
        return (irr, (BondTxn dateToBuy 0 (negate accuredInt) (negate buyPaidOut) 0 vs1 0 0 Nothing Types.Empty):boughtTxns)
  where 
    -- assume cashflow of buy date belongs to seller(owner)
    (bProjectedTxn',futureFlow') = splitByDate txns dateToBuy EqToLeft


priceBonds :: Ast.Asset a => TestDeal a -> AP.BondPricingInput -> Either String (Map.Map String PriceResult)
-- Price bond via discount future cashflow
priceBonds t (AP.DiscountCurve d dc) = Right $ Map.map (L.priceBond d dc) (viewBondsInMap t)
-- Run Z-Spread
priceBonds t@TestDeal {bonds = bndMap} (AP.RunZSpread curve bondPrices) 
  = sequenceA $ 
      Map.mapWithKey 
        (\bn (pd,price)-> ZSpread <$> (L.calcZspread (price,pd) (bndMap Map.! bn) curve))
        bondPrices
-- Calc Irr of bonds 
priceBonds t@TestDeal {bonds = bndMap} (AP.IrrInput bMapInput) 
  = let
      -- Date 
      d = getNextBondPayDate t
      -- get projected bond txn
      projectedTxns xs = snd $ splitByDate xs d EqToRight 
      -- (Maybe Bond,IrrType)
      bndMap' = Map.mapWithKey (\k v -> (getBondByName t True k, v)) bMapInput
      -- (Rate, [(date, cash)])
      bndMap'' = Map.mapWithKey (\bName (Just b, v) -> 
                                  do 
                                    let _irrTxns = projectedTxns (getAllTxns b)
                                    (_irr, flows) <- priceBondIrr v _irrTxns
                                    return (IrrResult (fromRational _irr) flows))
                                bndMap'
    in 
      sequenceA bndMap''


-- ^ split call option assumption , 
-- lefts are for waterfall payment days
-- rights are for date-based calls
splitCallOpts :: AP.CallOpt -> ([Pre],[Pre])
splitCallOpts (AP.CallPredicate ps) = (ps,[])
splitCallOpts (AP.LegacyOpts copts) = 
    let 
      cFn (C.PoolBalance bal) = If L (CurrentPoolBalance Nothing) bal
      cFn (C.BondBalance bal) = If L CurrentBondBalance bal
      cFn (C.PoolFactor r) = IfRate L (PoolFactor Nothing) (fromRational r)
      cFn (C.BondFactor r) = IfRate L BondFactor (fromRational r)
      cFn (C.OnDate d) = IfDate E d
      cFn (C.AfterDate d) = IfDate G d
      cFn (C.And _opts) = Types.All [ cFn o | o <- _opts  ]
      cFn (C.Or _opts) = Types.Any [ cFn o | o <- _opts  ]
      cFn (C.Pre p) = p
    in 
      ([ cFn copt | copt <- copts ],[])
-- legacyCallOptConvert (AP.CallOptions opts) = concat [ legacyCallOptConvert o | o <- opts ]
splitCallOpts (AP.CallOnDates dp ps) = ([],ps)
splitCallOpts x = error $ "Failed to find call option types but got"++ show x


-- <Legacy Test>, <Test on dates>
readCallOptions :: [AP.CallOpt] -> ([Pre],[Pre])
readCallOptions [] = ([],[])
readCallOptions opts = 
  let 
    result = splitCallOpts <$> opts
  in 
    (concat (fst <$> result), concat (snd <$> result))


runDeal :: Ast.Asset a => TestDeal a -> ExpectReturn -> Maybe AP.ApplyAssumptionType-> AP.NonPerfAssumption
        -> Either String (TestDeal a, Maybe (Map.Map PoolId CF.CashFlowFrame), Maybe [ResultComponent], Map.Map String PriceResult)
runDeal t _ perfAssumps nonPerfAssumps@AP.NonPerfAssumption{AP.callWhen = opts ,AP.pricing = mPricing ,AP.revolving = mRevolving ,AP.interest = mInterest} 
  | not runFlag = Left $ intercalate ";" $ show <$> valLogs 
  | otherwise 
    = do 
        (newT, ads, pcf, unStressPcf) <- getInits t perfAssumps (Just nonPerfAssumps)  
        (finalDeal, logs) <- run (removePoolCf newT) 
                                  pcf
                                  (Just ads) 
                                  mInterest
                                  (readCallOptions <$> opts)
                                  mRevolvingCtx
                                  DL.empty
        let poolFlowUsed = Map.map (fromMaybe (CF.CashFlowFrame (0,toDate "19000101",Nothing) [])) (getAllCollectedFrame finalDeal Nothing)  
        let poolFlowUsedNoEmpty = Map.map (over CF.cashflowTxn CF.dropTailEmptyTxns) poolFlowUsed  
        bndPricing <- case mPricing of 
                        (Just p) -> priceBonds finalDeal p 
                        Nothing -> Right Map.empty
        return (finalDeal, Just poolFlowUsedNoEmpty, Just (getRunResult finalDeal ++ V.validateRun finalDeal ++ DL.toList logs), bndPricing) -- `debug` ("Run Deal end with")
    where
      (runFlag, valLogs) = V.validateReq t nonPerfAssumps 
      -- getinits() will get (new deal snapshot, actions, pool cashflows, unstressed pool cashflow)
      -- extract Revolving Assumption
      mRevolvingCtx = case mRevolving of
                        Nothing -> Nothing
                        Just (AP.AvailableAssets rp rperf) -> Just (Map.fromList [("Consol", (rp, rperf))])
                        Just (AP.AvailableAssetsBy rMap) -> Just rMap
                        Just _ -> error ("Failed to match revolving assumption"++show mRevolving)
      -- run() is a recusive function loop over all actions till deal end conditions are met
      
-- | get bond principal and interest shortfalls from a deal
getRunResult :: Ast.Asset a => TestDeal a -> [ResultComponent]
getRunResult t = os_bn_i ++ os_bn_b -- `debug` ("Done with get result")
  where 
    bs = viewDealAllBonds t  
    os_bn_b = [ BondOutstanding (L.bndName _b) (L.getCurBalance _b) (getBondBegBal t (L.bndName _b)) | _b <- bs ] -- `debug` ("B"++ show bs)
    os_bn_i = [ BondOutstandingInt (L.bndName _b) (L.getTotalDueInt _b) (getBondBegBal t (L.bndName _b)) | _b <- bs ] -- `debug` ("C"++ show bs)

prepareDeal :: Ast.Asset a => TestDeal a -> TestDeal a
prepareDeal t@TestDeal {bonds = bndMap, liqProvider = mLiqProvider} 
  = let 
      pIdCf = view dealCashflow t
      newPtMap = Map.map (\mCf -> (over CF.cashflowTxn CF.dropTailEmptyTxns) <$> mCf )
                          pIdCf
      t1 = set dealCashflow newPtMap t
    in 
      t1 {bonds = Map.map (L.patchBondFactor . L.consolStmt) bndMap }


appendCollectedCF :: Ast.Asset a => Date -> TestDeal a -> Map.Map PoolId CF.CashFlowFrame -> TestDeal a
-- ^ append cashflow frame (consolidate by a date) into deals collected pool
appendCollectedCF d t@TestDeal { pool = pt } poolInflowMap
  = let 
      newPt = case pt of
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
                                             _ ->  view CF.tsRowBalance $ last txnCollected
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
              MultiPool pM -> MultiPool $ Map.map (set P.poolFutureCf Nothing) pM 
              ResecDeal uds -> ResecDeal uds
              _ -> error "not implement"
  in
    t {pool = newPt}





-- | run a pool of assets ,use asOfDate of Pool to cutoff cashflow yields from assets with assumptions supplied
runPool :: Ast.Asset a => P.Pool a -> Maybe AP.ApplyAssumptionType -> Maybe [RateAssumption] 
        -> Either String [(CF.CashFlowFrame, Map.Map CutoffFields Balance)]
-- schedule cashflow just ignores the interest rate assumption
runPool (P.Pool [] (Just cf) _ asof _ _ ) Nothing _ = Right [(cf, Map.empty)]
-- schedule cashflow with stress assumption
runPool (P.Pool [] (Just (CF.CashFlowFrame _ txn)) _ asof _ (Just dp)) (Just (AP.PoolLevel assumps)) mRates 
  = sequenceA [ Ast.projCashflow (ACM.ScheduleMortgageFlow asof txn dp) asof assumps mRates ] -- `debug` ("PROJ in schedule flow")

-- project contractual cashflow if nothing found in pool perf assumption
-- use interest rate assumption
runPool (P.Pool as _ _ asof _ _) Nothing mRates 
  = do 
      cf <- sequenceA $ parMap rdeepseq  
                              (\x -> Ast.calcCashflow x asof mRates) 
                              as 
      return [ (x, Map.empty) | x <- cf ]
-- asset cashflow with credit stress
---- By pool level
runPool (P.Pool as Nothing Nothing asof _ _) (Just (AP.PoolLevel assumps)) mRates 
  = sequenceA $ parMap rdeepseq (\x -> Ast.projCashflow x asof assumps mRates) as  
---- By index
runPool (P.Pool as Nothing Nothing asof _ _) (Just (AP.ByIndex idxAssumps)) mRates =
  let
    numAssets = length as
  in
    do 
      _assumps <- sequenceA $ map (AP.lookupAssumptionByIdx idxAssumps) [0..(pred numAssets)] -- `debug` ("Num assets"++ show numAssets)
      sequenceA $ parMap rdeepseq (\(x, a) -> Ast.projCashflow x asof a mRates) (zip as _assumps)

---- By Obligor
runPool (P.Pool as Nothing Nothing asof _ _) (Just (AP.ByObligor obligorRules)) mRates =
  let
    -- result cf,rules,assets
    -- matchAssets:: Ast.Asset c => [Either String (CF.CashFlowFrame, Map.Map CutoffFields Balance)] -> [AP.ObligorStrategy] 
    --               -> [c] -> Either String [(CF.CashFlowFrame, Map.Map CutoffFields Balance)] 
    matchAssets []   _ [] = Right [(CF.CashFlowFrame (0,epocDate,Nothing) [], Map.empty)] 
    matchAssets cfs [] [] = sequenceA cfs
    -- matchAssets cfs [] astList = sequenceA $ cfs ++ ((\x -> (\y -> (y, Map.empty)) <$> (Ast.calcCashflow x asof mRates)) <$> astList)
    matchAssets cfs [] astList = let
                                    poolCfs = parMap rdeepseq (\x -> Ast.calcCashflow x asof mRates) astList
                                    poolCfs' = (\x -> (, Map.empty) <$> x) <$> poolCfs
                                 in 
                                    sequenceA $ cfs ++ poolCfs'
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
               matchedCfs = parMap rdeepseq (\x -> Ast.projCashflow x asof assetPerf mRates) matchedAsts 
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
            matchedCfs = parMap rdeepseq (\x -> Ast.projCashflow x asof assetPerf mRates) matchedAsts 
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

            matchRulesFn fs fm = all (`matchRuleFn` fm) fs

            (matchedAsts,unMatchedAsts) = partition (matchRulesFn fieldRules . Ast.getObligorFields) astList            
            matchedCfs = parMap rdeepseq (\x -> Ast.projCashflow x asof assetPerf mRates) matchedAsts 
          in 
            matchAssets (cfs ++ matchedCfs) rules unMatchedAsts
        AP.ObligorByDefault assetPerf ->
          matchAssets 
            (cfs ++ (parMap rdeepseq (\x -> Ast.projCashflow x asof assetPerf mRates) astList))
            []
            []

        
  in
    matchAssets [] obligorRules as



-- safe net to catch other cases
runPool _a _b _c = Left $ "Failed to match" ++ show _a ++ show _b ++ show _c


-- ^ patch issuance balance for PreClosing Deal
patchIssuanceBalance :: Ast.Asset a => DealStatus -> Map.Map PoolId Balance -> PoolType a -> PoolType a
patchIssuanceBalance (Warehousing _) balM pt = patchIssuanceBalance (PreClosing Amortizing) balM pt
patchIssuanceBalance (PreClosing _ ) balM pt =
  case pt of 
    MultiPool pM -> MultiPool $ Map.mapWithKey (\k v -> over P.poolIssuanceStat (Map.insert IssuanceBalance (Map.findWithDefault 0.0 k balM)) v) pM
    ResecDeal pM -> ResecDeal pM  --TODO patch balance for resec deal
    
patchIssuanceBalance _ bal p = p -- `debug` ("NO patching ?")

patchScheduleFlow :: Ast.Asset a => Map.Map PoolId CF.CashFlowFrame -> PoolType a -> PoolType a
patchScheduleFlow flowM pt = 
  case pt of
    MultiPool pM -> MultiPool $ Map.intersectionWith (set P.poolFutureScheduleCf) (Just <$> flowM) pM
    ResecDeal pM -> ResecDeal pM

patchRuntimeBal :: Ast.Asset a => Map.Map PoolId Balance -> PoolType a -> PoolType a
patchRuntimeBal balMap (MultiPool pM) 
  = MultiPool $
      Map.mapWithKey
        (\k p -> over P.poolIssuanceStat 
                      (Map.insert RuntimeCurrentPoolBalance (Map.findWithDefault 0.0 k balMap)) 
                      p)
        pM

patchRuntimeBal balMap pt = pt

runPoolType :: Ast.Asset a => PoolType a -> Maybe AP.ApplyAssumptionType 
            -> Maybe AP.NonPerfAssumption -> Either String (Map.Map PoolId (CF.CashFlowFrame, Map.Map CutoffFields Balance))

runPoolType (MultiPool pm) (Just (AP.ByName assumpMap)) mNonPerfAssump
  = sequenceA $ Map.mapWithKey 
                         (\k p -> (P.aggPool (P.issuanceStat p)) <$> 
                                  (runPool p (AP.PoolLevel <$> Map.lookup k assumpMap) (AP.interest =<< mNonPerfAssump)))
                         pm

runPoolType (MultiPool pm) (Just (AP.ByPoolId assumpMap)) mNonPerfAssump
  = sequenceA $ Map.mapWithKey 
                    (\k p -> (P.aggPool (P.issuanceStat p)) <$> 
                             (runPool p (Map.lookup k assumpMap) (AP.interest =<< mNonPerfAssump)))
                    pm

runPoolType (MultiPool pm) mAssumps mNonPerfAssump
  = sequenceA $ 
      Map.map (\p -> (P.aggPool (P.issuanceStat p)) <$> (runPool p mAssumps (AP.interest =<< mNonPerfAssump)))
              pm

runPoolType (ResecDeal dm) mAssumps mNonPerfAssump
  -- = Map.foldrWithKey (\(DealBondFlow dn bn sd pct) (dname, cflow, stat) m ->
  --                          
  --                         Map.insert (DealBondFlow dname bn sd pct) (cflow, stat) m)
  --                         Map.empty $
  --   Map.mapWithKey (\(DealBondFlow dn bn sd pct) (uDeal, mAssump) -> 
  --                         let
  --                           (poolAssump,dealAssump) = case mAssump of 
  --                                                       Nothing -> (Nothing, AP.NonPerfAssumption Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing)
  --                                                       Just (_poolAssump, _dealAssump) -> (Just _poolAssump, _dealAssump)
  --                         in
  --                           do 
  --                             (dealRunned, _, _, _) <- runDeal uDeal DealPoolFlowPricing poolAssump dealAssump
  --                             let bondFlow = cutBy Inc Future sd $ concat $ Map.elems $ Map.map Stmt.getTxns $ getBondStmtByName dealRunned (Just [bn]) -- `debug` ("Bondflow from underlying runned"++ show (getBondStmtByName dealRunned (Just [bn])))
  --                             let bondFlowRated = (\(BondTxn d b i p r c di dioi f t) -> CF.BondFlow d b p i) <$> Stmt.scaleByFactor pct bondFlow -- `debug` ("Bondflow from underlying"++ show bondFlow)
  --                             return (name uDeal, CF.CashFlowFrame (0,sd,Nothing) bondFlowRated, Map.empty)) $
  --   Map.mapWithKey (\_ (UnderlyingDeal uDeal _ _ _) -> 
  --                       let 
  --                         dName = name uDeal -- `debug` ("Getting name of underlying deal:"++ (name uDeal))
  --                         mAssump = case mAssumps of 
  --                                     Just (AP.ByDealName assumpMap) -> Map.lookup dName assumpMap
  --                                     _ -> Nothing
  --                       in 
  --                         (uDeal, mAssump))
  --                     dm
  = 
    let 
      assumpMap =  Map.mapWithKey (\_ (UnderlyingDeal uDeal _ _ _) -> 
                              let 
                                 dName = name uDeal -- `debug` ("Getting name of underlying deal:"++ (name uDeal))
                                 mAssump = case mAssumps of 
                                             Just (AP.ByDealName assumpMap) -> Map.lookup dName assumpMap
                                             _ -> Nothing
                               in 
                                 (uDeal, mAssump))
                             dm
      ranMap =   Map.mapWithKey (\(DealBondFlow dn bn sd pct) (uDeal, mAssump) -> 
                                  let
                                    (poolAssump,dealAssump) = case mAssump of 
                                                                Nothing -> (Nothing, AP.NonPerfAssumption Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing)
                                                                Just (_poolAssump, _dealAssump) -> (Just _poolAssump, _dealAssump)
                                  in
                                    do 
                                      (dealRunned, _, _, _) <- runDeal uDeal DealPoolFlowPricing poolAssump dealAssump
                                      let bondFlow = cutBy Inc Future sd $ concat $ Map.elems $ Map.map (DL.toList . Stmt.getTxns) $ getBondStmtByName dealRunned (Just [bn]) -- `debug` ("Bondflow from underlying runned"++ show (getBondStmtByName dealRunned (Just [bn])))
                                      let bondFlowRated = (\(BondTxn d b i p r c di dioi f t) -> CF.BondFlow d b p i) <$> Stmt.scaleByFactor pct bondFlow -- `debug` ("Bondflow from underlying"++ show bondFlow)
                                      return (CF.CashFlowFrame (0,sd,Nothing) bondFlowRated, Map.empty))
                                 assumpMap
    in
      sequenceA ranMap
    

getInits :: Ast.Asset a => TestDeal a -> Maybe AP.ApplyAssumptionType -> Maybe AP.NonPerfAssumption 
         -> Either String (TestDeal a,[ActionOnDate], Map.Map PoolId CF.CashFlowFrame, Map.Map PoolId CF.CashFlowFrame)
getInits t@TestDeal{fees=feeMap,pool=thePool,status=status,bonds=bndMap,stats=_stats} mAssumps mNonPerfAssump =
  let 
    expandInspect sd ed (AP.InspectPt dp ds) = [ InspectDS _d [ds] | _d <- genSerialDatesTill2 II sd dp ed ]
    expandInspect sd ed (AP.InspectRpt dp dss) = [ InspectDS _d dss | _d <- genSerialDatesTill2 II sd dp ed ] 
  in 
    do 
      (startDate,closingDate,firstPayDate,pActionDates,bActionDates,endDate,custWdates) <- populateDealDates (dates t) status

      let intEarnDates = A.buildEarnIntAction (Map.elems (accounts t)) endDate [] 
      let intAccRateResetDates = (A.buildRateResetDates endDate) <$> (Map.elems (accounts t))
      let iAccIntDates = [ EarnAccInt _d accName | (accName,accIntDates) <- intEarnDates , _d <- accIntDates ] 
      let iAccRateResetDates = concat [ [ResetAccRate _d accName | _d <- _ds] | rst@(Just (accName, _ds)) <- intAccRateResetDates, isJust rst ]
    
      --fee accrue dates 
      let _feeAccrueDates = F.buildFeeAccrueAction (Map.elems feeMap) endDate [] 
      let feeAccrueDates = [ AccrueFee _d _feeName | (_feeName,feeAccureDates) <- _feeAccrueDates , _d <- feeAccureDates ]
    --liquidation facility
      let liqResetDates = case liqProvider t of 
                        Nothing -> []
                        Just mLiqProvider -> 
                            let 
                              _liqResetDates = CE.buildLiqResetAction (Map.elems mLiqProvider) endDate []
                              _liqRateResetDates = CE.buildLiqRateResetAction (Map.elems mLiqProvider) endDate []
                            in 
                              [ ResetLiqProvider _d _liqName |(_liqName,__liqResetDates) <- _liqResetDates , _d <- __liqResetDates ]
                              ++ 
                              [ ResetLiqProviderRate _d _liqName |(_liqName,__liqResetDates) <- _liqRateResetDates , _d <- __liqResetDates ]                            
    --inspect dates 
      let inspectDates = case mNonPerfAssump of
                          Just AP.NonPerfAssumption{AP.inspectOn = Just inspectList } -> concat $ (expandInspect startDate endDate) <$> inspectList
                          _ -> []
    
      let financialRptDates = case mNonPerfAssump of 
                            Just AP.NonPerfAssumption{AP.buildFinancialReport= Just dp } 
                              -> let 
                                   _ds = genSerialDatesTill2 II startDate dp endDate 
                                 in 
                                   [ BuildReport _sd _ed  | (_sd,_ed) <- zip _ds (tail _ds) ] -- `debug` ("ds"++ show _ds)
                            _ -> []  -- `debug` ("emtpy rpt dates")

      let irUpdateSwapDates = case rateSwap t of
                          Nothing -> []
                          Just rsm -> Map.elems $ Map.mapWithKey 
                                                   (\k x -> let 
                                                             resetDs = genSerialDatesTill2 EE (HE.rsStartDate x) (HE.rsUpdateDates x) endDate
                                                            in 
                                                             flip CalcIRSwap k <$> resetDs)
                                                   rsm
      let irSettleSwapDates = case rateSwap t of
                          Nothing -> []
                          Just rsm -> Map.elems $ Map.mapWithKey 
                                                    (\k x@HE.RateSwap{ HE.rsSettleDates = sDates} ->
                                                      case sDates of 
                                                        Nothing -> []
                                                        Just (sdp,_) ->
                                                          let 
                                                            resetDs = genSerialDatesTill2 EE (HE.rsStartDate x) sdp endDate
                                                          in 
                                                            flip SettleIRSwap k <$> resetDs)
                                                    rsm
      let rateCapSettleDates = case rateCap t of 
                             Nothing -> []
                             Just rcM -> Map.elems $ Map.mapWithKey 
                                                       (\k x -> let 
                                                                  resetDs = genSerialDatesTill2 EE (HE.rcStartDate x) (HE.rcSettleDates x) endDate
                                                                in 
                                                                  flip AccrueCapRate k <$> resetDs)
                                                       rcM
    -- bond rate resets 
      let bndRateResets = let 
                        bndWithDate = Map.toList $ Map.map 
                                                  (\b -> L.buildRateResetDates b closingDate endDate) 
                                                  bndMap
                      in 
                        [ ResetBondRate bdate bn | (bn, bdates) <- bndWithDate
                                                    , bdate <- bdates ] 

    -- bond step ups events
      let bndStepUpDates = let 
                        bndWithDate = Map.toList $ Map.map 
                                                  (\b -> L.buildStepUpDates b closingDate endDate) 
                                                  bndMap
                      in
                        [ StepUpBondRate bdate bn  | (bn, bdates) <- bndWithDate , bdate <- bdates ] 

    -- mannual triggers 
      let mannualTrigger = case mNonPerfAssump of 
                            Just AP.NonPerfAssumption{AP.fireTrigger = Just evts} -> [ FireTrigger d cycle n | (d,cycle,n) <- evts]
                            _ -> []

    -- make whole assumption
      let makeWholeDate = case mNonPerfAssump of
                            Just AP.NonPerfAssumption{AP.makeWholeWhen = Just (_d,_s,_t)} -> [MakeWhole _d _s _t]
                            _ -> [] 

    -- issue bonds in the future 
      let bondIssuePlan = case mNonPerfAssump of 
                            Just AP.NonPerfAssumption{AP.issueBondSchedule = Just bndPlan} 
                              -> [ IssueBond _d mPre bGroupName accName b mBal mRate | TsPoint _d (AP.IssueBondEvent mPre bGroupName accName b mBal mRate) <- bndPlan]
                                  ++ [FundBond _d mPre bName accName amount | TsPoint _d (AP.FundingBondEvent mPre bName accName amount) <- bndPlan]
                            _ -> []

    -- refinance bonds in the future 
      let bondRefiPlan = case mNonPerfAssump of 
                        Just AP.NonPerfAssumption{AP.refinance = Just bndPlan} 
                          -> [ RefiBondRate _d accName bName iInfo | TsPoint _d (AP.RefiRate accName bName iInfo) <- bndPlan]
                            ++ [ RefiBond _d accName bnd | TsPoint _d (AP.RefiBond accName bnd) <- bndPlan] 
                             
                        _ -> []

      let extractTestDates (AP.CallOnDates dp _) = [TestCall x | x <- genSerialDatesTill2 EE startDate dp endDate ]
      let extractTestDates _ = []
    -- extractTestDates (AP.CallOptions opts) = concat [ extractTestDates opt | opt <- opts ]
    -- call test dates 
      let callDates = case mNonPerfAssump of
                    Just AP.NonPerfAssumption{AP.callWhen = Just callOpts}
                      -> concat [ extractTestDates callOpt | callOpt <- callOpts ]
                    _ -> []

      let allActionDates = let 
                         __actionDates = let 
                                          a = concat [bActionDates,pActionDates,custWdates,iAccIntDates,makeWholeDate
                                                     ,feeAccrueDates,liqResetDates,mannualTrigger,concat rateCapSettleDates
                                                     ,concat irUpdateSwapDates, concat irSettleSwapDates ,inspectDates, bndRateResets,financialRptDates
                                                     ,bondIssuePlan,bondRefiPlan,callDates, iAccRateResetDates 
                                                     ,bndStepUpDates] 
                                        in
                                          case (dates t,status) of 
                                            (PreClosingDates {}, PreClosing _) -> sortBy sortActionOnDate $ DealClosed closingDate:a 
                                            _ -> sortBy sortActionOnDate a
                         _actionDates = __actionDates++[HitStatedMaturity endDate]
                       in 
                         case mNonPerfAssump of
                           Just AP.NonPerfAssumption{AP.stopRunBy = Just d} -> cutBy Exc Past d __actionDates ++ [StopRunFlag d]
                           _ -> _actionDates  
     
      let newFeeMap = case mNonPerfAssump of
                        Nothing -> feeMap
                        Just AP.NonPerfAssumption{AP.projectedExpense = Nothing } -> feeMap
                        -- Just AP.NonPerfAssumption{AP.projectedExpense = Just (fn,projectedFlow) } 
                        --  -> Map.adjust (\x -> x {F.feeType = F.FeeFlow projectedFlow}) fn feeMap
                        Just AP.NonPerfAssumption{AP.projectedExpense = Just pairs } 
                          ->   foldr  (\(feeName,feeFlow) accM -> Map.adjust (\v -> v {F.feeType = F.FeeFlow feeFlow}) feeName accM)  feeMap pairs
      pCfM <- runPoolType thePool mAssumps mNonPerfAssump
      pScheduleCfM <- runPoolType thePool Nothing mNonPerfAssump
      let poolCfTsM = Map.map (\(CF.CashFlowFrame _ txns, pstats) -> cutBy Inc Future startDate txns) pCfM -- `debug` ("Pool cfm"++ show pCfM)
      let poolCfTsMwithBegRow = Map.map (\case  
                                           (x:xs) -> buildBegTsRow startDate x:x:xs
                                           [] -> [])
                                        poolCfTsM 
      let poolAggCfM = Map.map (\x -> CF.aggTsByDates x (getDates pActionDates)) poolCfTsMwithBegRow  
      let pCollectionCfAfterCutoff = Map.map (\case 
                                               [] -> CF.CashFlowFrame (0,startDate,Nothing) []
                                               (txn:txns) -> CF.CashFlowFrame (CF.mflowBegBalance txn,startDate,Nothing) (txn:txns) ) 
                                             poolAggCfM -- `debug` ("Pool agg cfm"++ show (Map.map (sliceBy II (toDate "20241201") (toDate "20241231") ) poolAggCfM))
      let pTxnOfSpv = Map.map (\(CF.CashFlowFrame _ txns, pstats) -> cutBy Inc Future startDate txns) pScheduleCfM
      let pAggCfM = Map.map (\case
                          [] -> [] 
                          (x:xs) -> buildBegTsRow startDate x:x:xs) pTxnOfSpv  
      let pUnstressedAfterCutoff = Map.map (CF.CashFlowFrame (0,startDate,Nothing)) pAggCfM
      let poolWithSchedule = patchScheduleFlow pUnstressedAfterCutoff thePool -- `debug` ("D")
      let poolWithIssuanceBalance = patchIssuanceBalance status (Map.map 
                                                                  (\case 
                                                                      [] -> 0
                                                                      txns -> (CF.mflowBegBalance . head) txns)
                                                                  poolAggCfM)
                                                                poolWithSchedule
      let poolWithRunPoolBalance = patchRuntimeBal (Map.map (\(CF.CashFlowFrame (b,_,_) _) -> b) pCollectionCfAfterCutoff) poolWithIssuanceBalance

      let newStat = if (isPreClosing t) then 
                      _stats & (over _4) (`Map.union` (Map.fromList [(BondPaidPeriod,0),(PoolCollectedPeriod,0)]))
                    else 
                      _stats
      return (t {fees = newFeeMap , pool = poolWithRunPoolBalance , stats = newStat}
             , allActionDates
             , pCollectionCfAfterCutoff
             , pUnstressedAfterCutoff)

-- ^ UI translation : to read pool cash
-- TODO: need to make this a Maybe
readProceeds :: PoolSource -> CF.TsRow -> Either String Balance
readProceeds CollectedInterest x = Right $ CF.mflowInterest x
readProceeds CollectedPrincipal x = Right $ CF.mflowPrincipal x
readProceeds CollectedRecoveries x = Right $ CF.mflowRecovery x
readProceeds CollectedPrepayment x = Right $ CF.mflowPrepayment x
readProceeds CollectedRental  x    = Right $ CF.mflowRental x
readProceeds CollectedPrepaymentPenalty x = Right $ CF.mflowPrepaymentPenalty x
readProceeds CollectedCash x = Right $ CF.tsTotalCash x
readProceeds CollectedFeePaid x = Right $ CF.mflowFeePaid x
readProceeds a _ = Left $ " Failed to find pool cashflow field from pool cashflow rule "++show a


extractTxnsFromFlowFrameMap :: Maybe [PoolId] -> Map.Map PoolId CF.CashFlowFrame -> [CF.TsRow]
extractTxnsFromFlowFrameMap mPids pflowMap = 
  case mPids of 
    Nothing -> extractTxns pflowMap
    Just pids -> extractTxns $ Map.filterWithKey (\k _ -> k `elem` pids) pflowMap
  where 
    extractTxns m = concat $ (view CF.cashflowTxn) <$> Map.elems m
    -- extractTxns m = concatMap $ (view CF.cashflowTxn) $ Map.elems m

-- ^ deposit cash to account by collection rule
depositInflow :: Date -> W.CollectionRule -> Map.Map PoolId CF.CashFlowFrame -> Map.Map AccountName A.Account -> Either String (Map.Map AccountName A.Account)
depositInflow d (W.Collect mPids s an) pFlowMap amap 
  = do 
      amts <- sequenceA $ readProceeds s <$> txns
      let amt = sum amts
      return $ Map.adjust (A.deposit amt d (PoolInflow mPids s)) an amap
    where 
      txns =  extractTxnsFromFlowFrameMap mPids pFlowMap


depositInflow d (W.CollectByPct mPids s splitRules) pFlowMap amap    --TODO need to check 100%
  = do 
      amts <- sequenceA $ readProceeds s <$> txns
      let amt = sum amts
      let amtsToAccs = [ (an, mulBR amt splitRate) | (splitRate, an) <- splitRules]
      return $ 
              foldr
                (\(accName,accAmt) accM -> 
                  Map.adjust (A.deposit accAmt d (PoolInflow mPids s)) accName accM)
                amap
                amtsToAccs
    where 
      txns =  extractTxnsFromFlowFrameMap mPids pFlowMap 

depositInflow _ a _ _ = Left $ " Failed to match collection rule "++ show a

-- ^ deposit cash to account by pool map CF and rules
depositPoolFlow :: [W.CollectionRule] -> Date -> Map.Map PoolId CF.CashFlowFrame -> Map.Map String A.Account -> Either String (Map.Map String A.Account)
depositPoolFlow rules d pFlowMap amap
  -- = foldr (\rule acc -> depositInflow d rule pFlowMap acc) amap rules
  = foldM (\acc rule -> depositInflow d rule pFlowMap acc) amap rules

$(deriveJSON defaultOptions ''ExpectReturn)