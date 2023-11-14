{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}

module Deal (run,runPool,getInits,runDeal,ExpectReturn(..)
            ,performAction,queryDeal
            ,setFutureCF,populateDealDates
            ,calcTargetAmount,updateLiqProvider
            ,projAssetUnion,priceAssetUnion
            ,removePoolCf 
            ) where

import qualified Accounts as A
import qualified Ledger as LD
import qualified Asset as P
import qualified Expense as F
import qualified Liability as L
import qualified CreditEnhancement as CE
import qualified Hedge as HE
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
import Hedge (RateSwap(rsRefBalance))
import Cashflow (buildBegTsRow)
import Assumptions (NonPerfAssumption(NonPerfAssumption))
import Asset (Pool(issuanceStat))
import qualified Types as P
import Control.Lens hiding (element)
import Control.Lens.TH

debug = flip trace


setBondNewRate :: P.Asset a => TestDeal a -> Date -> [RateAssumption] -> L.Bond -> L.Bond
setBondNewRate t d ras b@(L.Bond _ _ _ _ (Just (L.PassDateSpread _ spd)) _ currentRate _ _ _ _ _ _)
  = b { L.bndRate = currentRate + spd }

setBondNewRate t d ras b@(L.Bond _ _ _ _ (Just (L.PassDateLadderSpread _ spd _)) _ currentRate _ _ _ _ _ _)
  = b { L.bndRate = currentRate + spd }

setBondNewRate t d ras b@(L.Bond _ _ _ (L.RefRate sr ds factor _) _ _ _ _ _ _ _ _ _) 
  = let 
      rate = queryDealRate t (patchDateToStats d ds)
    in 
      b {L.bndRate = fromRational (toRational rate * toRational factor) }

setBondNewRate t d ras b@(L.Bond _ _ _ ii _ _ _ _ _ _ _ _ _) 
  = b { L.bndRate = applyFloatRate ii d ras }

updateLiqProviderRate :: P.Asset a => TestDeal a -> Date -> [RateAssumption] -> CE.LiqFacility -> CE.LiqFacility
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
        Just (RateCurve _ curve) -> capping mCap $ flooring mFloor $ fromRational $ (getValByDate curve Inc d) + (toRational spd)

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
      _rate = idx_rate + spd

applyFloatRate (L.CapRate ii _rate) d ras = min _rate (applyFloatRate ii d ras)
applyFloatRate (L.FloorRate ii _rate) d ras = max _rate (applyFloatRate ii d ras)


updateRateSwapRate :: [RateAssumption] -> Date -> HE.RateSwap -> HE.RateSwap
updateRateSwapRate rAssumps d rs@HE.RateSwap{ HE.rsType = rt } 
  = rs {HE.rsPayingRate = pRate, HE.rsReceivingRate = rRate }
  where 
      (pRate,rRate) = case rt of 
                     HE.FloatingToFloating flter1 flter2 -> (getRate flter1,getRate flter2)
                     HE.FloatingToFixed flter r -> (getRate flter, r)
                     HE.FixedToFloating r flter -> (r , getRate flter)
      getRate x = AP.lookupRate rAssumps x d

updateRateSwapBal :: P.Asset a => TestDeal a -> Date -> HE.RateSwap -> HE.RateSwap
updateRateSwapBal t d rs@HE.RateSwap{ HE.rsNotional = base }
  =  case base of 
       HE.Fixed _ -> rs  
       HE.Base ds -> rs { HE.rsRefBalance = queryDeal t (patchDateToStats d ds) } -- `debug` ("query Result"++ show (patchDateToStats d ds) )
       HE.Schedule ts -> rs { HE.rsRefBalance = fromRational (getValByDate ts Inc d) }

testCall :: P.Asset a => TestDeal a -> Date -> C.CallOption -> Bool 
testCall t d opt = 
    case opt of 
       C.PoolBalance x -> queryDeal t FutureCurrentPoolBalance < x
       C.BondBalance x -> queryDeal t CurrentBondBalance < x
       C.PoolFactor x ->  queryDealRate t (FutureCurrentPoolFactor d) < fromRational x -- `debug` ("D "++show d++ "Pool Factor query ->" ++ show (queryDealRate t (FutureCurrentPoolFactor d)))
       C.BondFactor x ->  queryDealRate t BondFactor < fromRational x
       C.OnDate x -> x == d 
       C.AfterDate x -> d > x
       C.And xs -> all (testCall t d) xs
       C.Or xs -> any (testCall t d) xs
       C.Pre pre -> testPre d t pre
       _ -> error ("failed to find call options"++ show opt)

testCalls :: P.Asset a => TestDeal a -> Date -> [C.CallOption] -> Bool
testCalls t d [] = False  
testCalls t d opts = any (testCall t d) opts  

queryTrigger :: P.Asset a => TestDeal a -> DealCycle -> [Trigger]
queryTrigger t@TestDeal{ triggers = trgs } wt 
  = case trgs of 
      Nothing -> []
      Just _trgs -> maybe [] Map.elems $ Map.lookup wt _trgs

testTriggers :: P.Asset a => TestDeal a -> Date -> [Trigger] -> Bool
testTriggers t d [] = False
testTriggers t d triggers = any (testTrigger t d) triggers 

runEffects :: P.Asset a => TestDeal a -> Date -> TriggerEffect -> TestDeal a 
runEffects t@TestDeal{accounts = accMap, fees = feeMap } d te 
  = case te of 
      DealStatusTo _ds -> t {status=_ds} -- `debug` ("changing status to "++show _ds++"on date"++ show d)
      DoAccrueFee fns -> t {fees = foldr (Map.adjust (calcDueFee t d)) feeMap fns}  
      ChangeReserveBalance accName rAmt ->
          t {accounts = Map.adjust (A.updateReserveBalance rAmt) accName accMap }        
      _ -> error $ "Failed to match trigger effects: "++show te

-- ^ test trigger and add a log if deal status changed
runTriggers :: P.Asset a => TestDeal a -> Date -> DealCycle -> (TestDeal a,[ResultComponent])
runTriggers t@TestDeal{status=oldStatus, triggers = Nothing} d dcycle = (t, [])
runTriggers t@TestDeal{status=oldStatus, triggers = Just trgM} d dcycle = 
    (newDeal {triggers = Just (Map.insert dcycle newTriggers trgM)}, newLogs)
  where 
    -- _trgs = Map.findWithDefault [] dcycle trgM

    -- get triggeres to run at `dealCycle`
    trgsMap = Map.findWithDefault Map.empty dcycle trgM
    
    --testTrgsResult = [ (_trg, (not (trgStatus _trg) || trgStatus _trg && trgCurable _trg) && testTrigger t d _trg)
    --                  | _trg <- _trgs ] 
    
    -- triggered trigger
    triggeredTrgs = Map.filter   
                          (\trg -> 
                            (not (trgStatus trg) || trgStatus trg && trgCurable trg) && testTrigger t d trg)
                          trgsMap

    -- extract trigger effects to run                   
    triggeredEffects = [ trgEffects _trg | _trg <- Map.elems triggeredTrgs ] 

    -- run effects on deals
    -- aka (\_t _te -> runEffects _t d _te)
    newDeal = foldl (`runEffects` d) t triggeredEffects

    -- if deal status changed, then insert to log if changes
    newStatus = status newDeal 
    newLogs = [DealStatusChangeTo d oldStatus newStatus |  newStatus /= oldStatus] 

    -- new status of trigger, update status of trigger to True
    triggeredNames = Map.keys triggeredTrgs

    newTriggers = Map.union (Map.map (set trgStatusLens True) triggeredTrgs) trgsMap
  
 
run :: P.Asset a => TestDeal a -> CF.CashFlowFrame -> Maybe [ActionOnDate] -> Maybe [RateAssumption] -> Maybe [C.CallOption] -> Maybe (RevolvingPool , AP.ApplyAssumptionType)-> [ResultComponent] -> (TestDeal a,[ResultComponent])
run t@TestDeal{status=Ended} pcf ads _ _ _ log  = (prepareDeal t,log) `debug` ("Deal Ended")
run t pcf (Just []) _ _ _ log  = (prepareDeal t,log)  `debug` "End with Empty ActionOnDate"
run t@TestDeal{accounts=accMap,fees=feeMap,triggers=mTrgMap,bonds=bndMap,status=dStatus,waterfall=waterfallM} poolFlow (Just (ad:ads)) rates calls rAssump log
  | (CF.sizeCashFlowFrame poolFlow == 0) && (queryDeal t  AllAccBalance == 0) 
     = let 
         _dealAfterCleanUp = foldl (performAction (getDate ad)) t cleanUpActions `debug` ("CleanUp deal:"++ (name t))
       in 
         (prepareDeal _dealAfterCleanUp,log) `debug` "End with pool cf == 0 and all account bals are 0" -- ++ "> Remain Actions" ++ show (ad:ads))
  | otherwise
     = case ad of 
         PoolCollection d _ ->
           if CF.sizeCashFlowFrame poolFlow > 0 then
             let 
               (collectedFlow, outstandingFlow) = CF.splitCashFlowFrameByDate poolFlow d EqToLeft 
               accs = depositPoolInflow (collects t) d collectedFlow accMap  -- `debug` ("Collected"++ show d++"pool CF\n"++ show poolFlow)--  `debug` ("Running AD P"++show(d)) --`debug` ("Deposit-> Collection Date "++show(d)++"with"++show(collected_flow))
               dAfterDeposit = (appendCollectedCF d t collectedFlow) {accounts=accs}   -- `debug` ("CF size collected"++ show (CF.getTsCashFlowFrame))
               (dRunWithTrigger0,newLogs0) = runTriggers dAfterDeposit d EndCollection  
               waterfallToExe = Map.findWithDefault [] W.EndOfPoolCollection (waterfall t)  -- `debug` ("AD->"++show(ad)++"remain ads"++show(length ads))
               (dAfterAction,rc,newLogs) = foldl (performActionWrap d) (dRunWithTrigger0
                                                                        ,RunContext outstandingFlow rAssump rates
                                                                        ,log ) waterfallToExe
               (dRunWithTrigger1,newLogs1) = runTriggers dAfterAction d EndCollectionWF -- `debug` ("Running T end of Collection"++show (queryTrigger dAfterAction EndCollectionWF))
             in 
               run dRunWithTrigger1 (runPoolFlow rc) (Just ads) rates calls rAssump (log++newLogs0++newLogs1)  -- `debug` ("End :after new pool flow"++ show (runPoolFlow rc))
           else
             run t (CF.CashFlowFrame []) (Just ads) rates calls rAssump log    
   
         RunWaterfall d _ ->
           case calls of
             Just callOpts ->
               if testCalls dRunWithTrigger1 d callOpts then 
                 let 
                    dealAfterCleanUp = foldl (performAction d) dRunWithTrigger1 cleanUpActions 
                    newStLogs = [DealStatusChangeTo d dStatus Called ] 
                    endingLogs = Rpt.patchFinancialReports dealAfterCleanUp d newLogs
                 in  
                    (prepareDeal dealAfterCleanUp, endingLogs++newStLogs) -- `debug` ("Called ! "++ show d)
               else
                 run dRunWithTrigger1 (runPoolFlow newRc) (Just ads) rates calls rAssump newLogs -- `debug` ("status in run waterfall"++show (status dRunWithTrigger1))
             Nothing ->
               run dRunWithTrigger1 (runPoolFlow newRc) (Just ads) rates Nothing rAssump newLogs  -- `debug` ("Run waterfall "++ show d) -- `debug` ("Deal Status"++ show (status dRunWithTrigger1)) -- `debug` ("Call is Nothing")-- `debug` ("Running Waterfall at"++ show d)--  `debug` ("!!!Running waterfall"++show(ad)++"Next ad"++show(head ads)++"PoolFLOW>>"++show(poolFlow)++"AllACCBAL"++show(queryDeal t AllAccBalance))
           where
                (dRunWithTrigger0,newLogs0) = runTriggers t d BeginDistributionWF
                -- warning if not waterfall distribution found
                newLogs1 = if Map.notMember (W.DistributionDay dStatus) waterfallM then
                             [WarningMsg ("No waterfall distribution found on date"++show d++"with status"++show dStatus)]
                           else
                             []
                waterfallToExe = Map.findWithDefault 
                                   (Map.findWithDefault [] (W.DistributionDay dStatus) waterfallM)
                                   W.DefaultDistribution 
                                   waterfallM
                runContext = RunContext poolFlow rAssump rates
                (dAfterWaterfall,newRc,newLogsWaterfall) = foldl (performActionWrap d) (dRunWithTrigger0,runContext,newLogs0) waterfallToExe  -- `debug` ("Waterfall>>>"++show(waterfallToExe))
                (dRunWithTrigger1,newLogs2) = runTriggers dAfterWaterfall d EndDistributionWF  
                newLogs = log ++ newLogsWaterfall ++ newLogs1 ++ newLogs2

         EarnAccInt d accName ->
           let 
             newAcc = Map.adjust 
                        (\a -> case a of
                                (A.Account _ _ (Just (A.BankAccount {})) _ _ ) -> (A.depositInt a d)  -- `debug` ("int acc"++show accName)
                                (A.Account _ _ (Just (A.InvestmentAccount idx _ lastAccureDate _)) _ _ ) -> 
                                  case AP.getRateAssumption (fromMaybe [] rates) idx of
                                    Nothing -> a -- `debug` ("error..."++show accName)
                                    Just (RateCurve _ _ts) -> A.depositIntByCurve a _ts d 
                                    Just (RateFlat _ r )   -> A.depositIntByCurve a (mkRateTs [(lastAccureDate,r),(d,r)]) d
                                    _ -> error ("Failed to match index "++show idx++" In rate assumpt" ++ (name t)) ) -- `debug` ("int acc"++show accName)
                        accName  
                        accMap
           in 
             run (t {accounts = newAcc}) poolFlow (Just ads) rates calls rAssump log
         
         AccrueFee d feeName ->  -- (t , log)
           let 
             newFeeMap = Map.adjust (calcDueFee t d) feeName feeMap -- `debug` ("Accure Fee on Actions")
           in
             run (t{fees=newFeeMap}) poolFlow (Just ads) rates calls rAssump log
   
         ResetLiqProvider d liqName -> 
           case liqProvider t of 
             Nothing -> run t poolFlow (Just ads) rates calls rAssump log
             (Just mLiqProvider) 
               -> let -- update credit 
                    newLiqMap = Map.adjust (updateLiqProvider t d) liqName mLiqProvider
                  in
                    run (t{liqProvider = Just newLiqMap}) poolFlow (Just ads) rates calls rAssump log

         ResetLiqProviderRate d liqName -> 
           case liqProvider t of 
             Nothing -> run t poolFlow (Just ads) rates calls rAssump log
             (Just mLiqProvider) 
               -> let -- update rate 
                    newLiqMap = Map.adjust (updateLiqProviderRate t d (fromMaybe [] rates)) liqName mLiqProvider
                  in
                    run (t{liqProvider = Just newLiqMap}) poolFlow (Just ads) rates calls rAssump log
        
         DealClosed d ->
           let 
             (PreClosing newSt) = status t `debug` (">>>>>>"++ show (status t))
             w = Map.findWithDefault [] W.OnClosingDay (waterfall t)  -- `debug` ("DDD0")
             rc = RunContext poolFlow rAssump rates  -- `debug` ("DDD1")
             (newDeal, newRc, newLog) = foldl (performActionWrap d) (t, rc, log) w  -- `debug` ("ClosingDay Action:"++show w)
           in 
             run newDeal{status=newSt} (runPoolFlow newRc) (Just ads) rates calls rAssump (newLog++[DealStatusChangeTo d (PreClosing newSt) newSt]) -- `debug` ("New pool flow"++show (runPoolFlow newRc))

         ChangeDealStatusTo d s -> run (t{status=s}) poolFlow (Just ads) rates calls rAssump log

         ResetIRSwapRate d sn -> 
           let
             _rates = fromMaybe [] rates
             newRateSwap_rate = Map.adjust (updateRateSwapRate _rates d) sn  <$>  rateSwap t  
             newRateSwap_bal = Map.adjust (updateRateSwapBal t d) sn <$> newRateSwap_rate 
             newRateSwap_acc = Map.adjust (HE.accrueIRS d) sn <$> newRateSwap_bal
           in 
             run (t{rateSwap = newRateSwap_acc}) poolFlow (Just ads) rates calls rAssump log

         InspectDS d ds -> 
           let 
             newlog = inspectVars t d ds 
           in 
             run t poolFlow (Just ads) rates calls rAssump $ log++[newlog] -- `debug` ("Add log"++show newlog)
         
         ResetBondRate d bn -> 
             let 
               newBndMap = case rates of 
                             Nothing -> error ("No rate assumption for floating bond:"++bn++"Deal"++ name t)
                             (Just _rates) -> Map.adjustWithKey 
                                              (\k v-> setBondNewRate t d _rates v)
                                              bn
                                              bndMap -- `debug` ("Reset bond"++show bn)
             in 
               run t{bonds = newBndMap} poolFlow (Just ads) rates calls rAssump log
         BuildReport sd ed ->
             let 
               bsReport = Rpt.buildBalanceSheet t ed 
               cashReport = Rpt.buildCashReport t sd ed 
               newlog = FinancialReport sd ed bsReport cashReport
             in 
               run t poolFlow (Just ads) rates calls rAssump $ log++[newlog] 
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
               newT = case triggerEffects of 
                        Nothing -> t  `debug` "Nothing found on effects"
                        Just efs -> runEffects t d efs
               (oldStatus,newStatus) = (status t,status newT)
               stChangeLogs = [DealStatusChangeTo d oldStatus newStatus |  oldStatus /= newStatus] 

               newLog = WarningMsg $ "Trigger Overrided to True "++ show(d,cyc,n)
             in 
               run newT{triggers = Just triggerFired} poolFlow (Just ads) rates calls rAssump $ log++[newLog]++stChangeLogs
                
         _ -> error $ "Failed to match action on Date"++ show ad
         where
           cleanUpActions = Map.findWithDefault [] W.CleanUp (waterfall t) -- `debug` ("Running AD"++show(ad))


run t (CF.CashFlowFrame []) Nothing Nothing Nothing Nothing log
  = run t pcf (Just ads) Nothing Nothing Nothing log  -- `debug` ("Init Done >>Last Action#"++show (length ads)++"F/L"++show (head ads)++show (last ads))
  where
    (t, ads,pcf) = getInits t Nothing Nothing 

run t (CF.CashFlowFrame []) _ _ _ _ log = (prepareDeal t,log) -- `debug` ("End with pool CF is []")
 


-- reserved for future used
data ExpectReturn = DealStatus
                  | DealPoolFlow
                  | DealPoolFlowPricing   -- ^ default option, return pricing and bond/pool/account/fee etc cashflow
                  | DealTxns
                  | ExecutionSummary
                  deriving (Show,Generic)

priceBonds :: TestDeal a -> AP.BondPricingInput -> Map.Map String L.PriceResult
priceBonds t (AP.DiscountCurve d dc) = Map.map (L.priceBond d dc) (bonds t)
priceBonds t@TestDeal {bonds = bndMap} (AP.RunZSpread curve bond_prices) 
  = Map.mapWithKey 
      (\bn (pd,price)-> L.ZSpread $
                           L.calcZspread 
                             (price,pd) 
                             0
                             (1.0
                              ,(1.0,0.5)
                              ,toRational ((rateToday pd) - toRational (L.bndRate (bndMap Map.!bn))))
                             (bndMap Map.! bn)
                             curve)
      bond_prices
    where 
      rateToday = getValByDate curve Inc     

runDeal :: P.Asset a => TestDeal a -> ExpectReturn -> Maybe AP.ApplyAssumptionType-> AP.NonPerfAssumption
        -> (TestDeal a,Maybe CF.CashFlowFrame, Maybe [ResultComponent],Maybe (Map.Map String L.PriceResult))
runDeal t _ perfAssumps nonPerfAssumps@AP.NonPerfAssumption{AP.callWhen  = opts
                                                           ,AP.pricing   = mPricing
                                                           ,AP.revolving = mRevolving
                                                           ,AP.interest  = mInterest} 
  | not runFlag = (t, Nothing, Just valLogs, Nothing)
  | otherwise = (finalDeal, Just pcf, Just (getRunResult finalDeal ++ V.validateRun finalDeal ++logs), bndPricing) -- `debug` ("Run Deal end with")
    where
      (runFlag,valLogs) = V.validateReq t nonPerfAssumps
      -- getinits() will get (new deal snapshot, actions, pool cashflows)
      (newT, ads, pcf) = getInits t perfAssumps (Just nonPerfAssumps) -- `debug` ("runDeal init line") 
      -- extract Revolving Assumption
      mRevolvingCtx = case mRevolving of
                        Nothing -> Nothing
                        Just (AP.AvailableAssets rp rperf) -> Just (rp,rperf)
                        Just _ -> error ("Failed to match revolving assumption"++show mRevolving)
      -- run() is a recusive function loop over all actions till deal end conditions are met
      (finalDeal, logs) = run (removePoolCf newT) 
                              pcf 
                              (Just ads) 
                              mInterest
                              opts
                              mRevolvingCtx
                              [] -- `debug` ("start status"++show (status t) )-- `debug` ("run rAssump>>"++show revolvingAssump++"1st Action"++ show (head ads)++"PCF size"++show (CF.sizeCashFlowFrame pcf))
      -- bond pricing if any                            
      bndPricing = case mPricing of
                     Nothing -> Nothing   --  `debug` ("pricing bpi with Nothing")
                     Just _bpi -> Just (priceBonds finalDeal _bpi)  -- `debug` ("Pricing with")

-- | get bond principal and interest shortfalls from a deal
getRunResult :: TestDeal a -> [ResultComponent]
getRunResult t = os_bn_i ++ os_bn_b
  where 
    bs = Map.elems $ bonds t
    os_bn_b = [ BondOutstanding (L.bndName _b) (L.bndBalance _b) (getBondBegBal t (L.bndName _b)) | _b <- bs ]
    os_bn_i = [ BondOutstandingInt (L.bndName _b) (L.bndDueInt _b) (getBondBegBal t (L.bndName _b)) | _b <- bs ]

prepareDeal :: TestDeal a -> TestDeal a
prepareDeal t@TestDeal {bonds = bndMap} 
  = t {bonds = Map.map L.consolStmt bndMap}  -- `debug` ("Consolidation in Preparing")

appendCollectedCF :: P.Asset a => Date -> TestDeal a -> CF.CashFlowFrame -> TestDeal a
-- ^ append cashflow frame (consolidate by a date) into deals collected pool
appendCollectedCF d t (CF.CashFlowFrame []) = t
appendCollectedCF d t@TestDeal { pool = mpool } cf@(CF.CashFlowFrame _trs)
  = case P.futureCf mpool of 
      Nothing -> setFutureCF t cf
      Just _p -> setFutureCF t (CF.appendCashFlow _p mergedPoolStats)
    where
      mergedPoolStats = [CF.sumTsCF _trs d]

-- ^ emtpy deal's pool cashflow
removePoolCf :: P.Asset a => TestDeal a -> TestDeal a
removePoolCf = set (dealPool . P.poolFutureCf) Nothing

setFutureCF :: P.Asset a => TestDeal a -> CF.CashFlowFrame -> TestDeal a
setFutureCF t cf = set (dealPool . P.poolFutureCf) (Just cf) t 

populateDealDates :: DateDesp -> (Date,Date,Date,[ActionOnDate],[ActionOnDate],Date)
populateDealDates (CustomDates cutoff pa closing ba) 
  = (cutoff  
    ,closing
    ,getDate (head ba)
    ,pa
    ,ba
    ,(getDate (max (last pa) (last ba))))

populateDealDates (PatternInterval _m) 
  = (cutoff,closing,nextPay,pa,ba,max ed1 ed2) -- `debug` ("PA>>>"++ show pa)
    where 
      (cutoff,dp1,ed1) = _m Map.! CutoffDate
      (nextPay,dp2,ed2) = _m Map.! FirstPayDate 
      (closing,_,_) = _m Map.! ClosingDate
      pa = [ PoolCollection _d "" | _d <- genSerialDatesTill cutoff dp1 ed1 ]
      ba = [ RunWaterfall _d "" | _d <- genSerialDatesTill nextPay dp2 ed2 ]

populateDealDates (PreClosingDates cutoff closing mRevolving end (firstCollect,poolDp) (firstPay,bondDp))
  = (cutoff,closing,firstPay,pa,ba,end) -- `debug` ("POOL A"++show pa) 
    where 
      pa = [ PoolCollection _d "" | _d <- genSerialDatesTill2 IE firstCollect poolDp end ]
      ba = [ RunWaterfall _d "" | _d <- genSerialDatesTill2 IE firstPay bondDp end ]

populateDealDates (CurrentDates (lastCollect,lastPay) mRevolving end (nextCollect,poolDp) (nextPay,bondDp))
  = (lastCollect, lastPay,head futurePayDates, pa, ba, end) 
    where 
      futurePayDates = genSerialDatesTill2 IE nextPay bondDp end 
      ba = [ RunWaterfall _d "" | _d <- futurePayDates]
      futureCollectDates = genSerialDatesTill2 IE nextCollect poolDp end 
      pa = [ PoolCollection _d "" | _d <- futureCollectDates]

calcDealStageDate :: DateDesp -> [(Date,DealStatus)]
calcDealStageDate (PreClosingDates _ closing Nothing endDate _ _) = [(endDate,Ended)]
calcDealStageDate (PreClosingDates _ closing (Just revolvingEndDate) endDate _ _) = [(endDate,Ended)]
calcDealStageDate (CurrentDates _ Nothing endDate _ _) = [(endDate,Ended)]
calcDealStageDate (CurrentDates _ (Just revolvingEndDate) endDate _ _) = [(endDate,Ended)]
calcDealStageDate _ = []

-- | run a pool of assets ,use asOfDate of Pool to cutoff cashflow yields from assets with assumptions supplied
runPool :: P.Asset a => P.Pool a -> Maybe AP.ApplyAssumptionType -> Maybe [RateAssumption] -> [(CF.CashFlowFrame, Map.Map CutoffFields Balance )]
-- schedule cashflow just ignores the interest rate assumption
runPool (P.Pool [] (Just cf) asof _ _) Nothing _ = [(cf, Map.empty)]
runPool (P.Pool [] (Just (CF.CashFlowFrame txn)) asof _ (Just dp)) (Just (AP.PoolLevel assumps)) mRates = [ P.projCashflow (ACM.ScheduleMortgageFlow asof txn dp) asof assumps mRates ] -- `debug` ("PROJ in schedule flow")

-- contractual cashflow will use interest rate assumption
runPool (P.Pool as _ asof _ _) Nothing mRates = map (\x -> (P.calcCashflow x asof mRates,Map.empty)) as -- `debug` ("RUNPOOL-> calc cashflow")

-- asset cashflow with credit stress
runPool (P.Pool as Nothing asof _ _) (Just (AP.PoolLevel assumps)) mRates = map (\x -> P.projCashflow x asof assumps mRates) as  -- `debug` (">> Single Pool")
runPool (P.Pool as Nothing asof _ _) (Just (AP.ByIndex idxAssumps)) mRates =
  let
    numAssets = length as
    _assumps = map (AP.lookupAssumptionByIdx idxAssumps) [0..(pred numAssets)] -- `debug` ("Num assets"++ show numAssets)
  in
    zipWith (\x a -> P.projCashflow x asof a mRates) as _assumps

-- mixed asset
-- runPool (P.Pool mixedAsset Nothing asof _ _) (Just (AP.ByName assumpM)) mRates = 
--   let 
--     r = projectCashflow (head mixedAsset) asof assumpM mRates
--     
--   in 
--     r 


-- safe net to catch other cases
runPool _a _b _c = error $ "Failed to match" ++ show _a ++ show _b ++ show _c


-- ^ patch issuance balance for PreClosing Deal
patchIssuanceBalance :: P.Asset a => DealStatus -> Balance -> Pool a -> Pool a
patchIssuanceBalance (PreClosing _ ) bal p@P.Pool{issuanceStat = Nothing } = p { issuanceStat = Just (Map.fromList [(IssuanceBalance,bal)])}
patchIssuanceBalance (PreClosing _ ) bal p@P.Pool{issuanceStat = Just statM } = p { issuanceStat = Just (Map.insert IssuanceBalance bal statM)}
patchIssuanceBalance _ bal p = p

getInits :: P.Asset a => TestDeal a -> Maybe AP.ApplyAssumptionType -> Maybe AP.NonPerfAssumption -> (TestDeal a,[ActionOnDate], CF.CashFlowFrame)
getInits t@TestDeal{fees= feeMap,pool=thePool,status=status,bonds=bndMap} mAssumps mNonPerfAssump
  = (newT, allActionDates, pCollectionCfAfterCutoff)  -- `debug` ("init done actions->"++ show pCollectionCfAfterCutoff)
  where
    (startDate,closingDate,firstPayDate,pActionDates,bActionDates,endDate) = populateDealDates (dates t)
    dealStatusDates = calcDealStageDate (dates t) 
    dealStageDates = [ ChangeDealStatusTo d s | (d,s) <- dealStatusDates ]
    intEarnDates = A.buildEarnIntAction (Map.elems (accounts t)) endDate [] -- `debug` (show (startDate,firstPayDate,pActionDates,bActionDates,endDate))
    iAccIntDates = [ EarnAccInt _d accName | (accName,accIntDates) <- intEarnDates
                                           , _d <- accIntDates ] -- `debug` ("PoolactionDates"++show  pActionDates)
    --fee accrue dates 
    _feeAccrueDates = F.buildFeeAccrueAction (Map.elems (fees t)) endDate [] 
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
    -- bond rate resets 
    bndRateResets = let 
                      bndWithDate = Map.toList $ Map.map (\b -> L.buildRateResetDates b closingDate endDate) bndMap
                    in 
                      [ ResetBondRate bdate bn | (bn,bdates) <- bndWithDate, bdate <- bdates ]
    -- mannual triggers 
    mannualTrigger = case mNonPerfAssump of 
                       Just AP.NonPerfAssumption{AP.fireTrigger = Just evts} -> [ FireTrigger d cycle n | (d,cycle,n) <- evts]
                       _ -> []

    allActionDates = let 
                       _actionDates = let 
                                        a = concat [bActionDates,pActionDates,iAccIntDates
                                                   ,feeAccrueDates,liqResetDates,dealStageDates,mannualTrigger
                                                   ,concat irSwapRateDates,inspectDates, bndRateResets,financialRptDates] -- `debug` ("fee acc dates"++show feeAccrueDates)
                                      in
                                        case dates t of 
                                          PreClosingDates {} -> sortBy sortActionOnDate $ DealClosed closingDate:a  -- `debug` ("add a closing date"++show closingDate)
                                          _ -> sortBy sortActionOnDate a
                     in 
                       case mNonPerfAssump of
                         Just AP.NonPerfAssumption{AP.stopRunBy = Just d} -> cutBy Exc Past d _actionDates
                         _ -> _actionDates
                                    

    (poolCf,historyStats) = P.aggPool (P.issuanceStat thePool) $ runPool thePool mAssumps (AP.interest =<< mNonPerfAssump)  -- `debug` ("rates assump"++ show (AP.interest =<< mNonPerfAssump)mNonPerfAssump)
    -- (poolCf,historyStats) = P.aggPool $ runPool thePool mAssumps (AP.interest <*> mNonPerfAssump) -- `debug` ("agg pool flow")
    -- poolCfTs = cutBy Inc Future startDate $ CF.getTsCashFlowFrame poolCf -- `debug` ("Pool Cf in pool>>"++show poolCf++"\n start date"++ show startDate)
    poolCfTs = cutBy Inc Future startDate $ CF.getTsCashFlowFrame poolCf -- `debug` ("Pool Cf in pool>>"++show poolCf++"\n start date"++ show startDate)
    poolAggCf = CF.aggTsByDates poolCfTs (getDates pActionDates)
    begRow = buildBegTsRow startDate (head poolAggCf)
    pCollectionCfAfterCutoff = CF.CashFlowFrame $ begRow:poolAggCf
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
    newT = t {fees = newFeeMap
             , pool = patchIssuanceBalance status (CF.mflowBalance begRow) thePool
             } -- patching with performing balance

-- ^ UI translation : to read pool cash
readProceeds :: PoolSource -> CF.TsRow -> Balance
readProceeds CollectedInterest  row = CF.mflowInterest row
readProceeds CollectedPrincipal row = CF.mflowPrincipal row
readProceeds CollectedRecoveries row = CF.mflowRecovery row
readProceeds CollectedPrepayment row = CF.mflowPrepayment row
readProceeds CollectedRental     row = CF.mflowRental row
readProceeds CollectedPrepaymentPenalty row =  CF.mflowPrepaymentPenalty row
readProceeds CollectedCash row =  CF.tsTotalCash row
readProceeds a b = error $ "failed to read pool cashflow rule"++show a

depositInflow :: W.CollectionRule -> Date -> CF.TsRow -> Map.Map AccountName A.Account -> Map.Map AccountName A.Account
depositInflow (W.Collect s an) d row amap 
  = Map.adjust (A.deposit amt d (PoolInflow s)) an amap
    where 
      amt = readProceeds s row


depositInflow (W.CollectByPct s splitRules) d row amap    --TODO need to check 100%
  = foldr
      (\(accName,accAmt) accM -> 
        Map.adjust (A.deposit accAmt d (PoolInflow s)) accName accM)
      amap
      amtsToAccs
    where 
      amtsToAccs = [ (an, mulBR amt splitRate) | (splitRate, an) <- splitRules]
      amt = readProceeds s row

depositInflow a _ _ _ = error $ "Failed to match collection rule"++ show a


depositInflowByRules :: [W.CollectionRule] -> Date -> CF.TsRow -> Map.Map AccountName A.Account ->  Map.Map AccountName A.Account
depositInflowByRules rs d row amap 
  = foldr (\r -> depositInflow r d row) amap rs

depositPoolInflow :: [W.CollectionRule] -> Date -> CF.CashFlowFrame -> Map.Map String A.Account -> Map.Map String A.Account
depositPoolInflow rules d (CF.CashFlowFrame []) amap = amap 
depositPoolInflow rules d (CF.CashFlowFrame txn) amap = foldr (depositInflowByRules rules d) amap txn

$(deriveJSON defaultOptions ''ExpectReturn)
