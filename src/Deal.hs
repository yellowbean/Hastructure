{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}

module Deal (run2,runPool2,getInits,runDeal,ExpectReturn(..)
            ,applicableAdjust,performAction,queryDeal
            ,setFutureCF,populateDealDates
            ,calcTargetAmount,updateLiqProvider
            ,projAssetUnion,priceAssetUnion,accrueLiqProvider
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
import qualified Call as C
import qualified InterestRate as IR
import Deal.DealBase
import Deal.DealQuery
import Deal.DealAction
import Stmt
import Lib
import Util
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
debug = flip trace


setBondNewRate :: P.Asset a => TestDeal a -> Date -> [RateAssumption] -> L.Bond -> L.Bond
setBondNewRate t d ras b@(L.Bond _ _ _ (L.StepUpFix _ _ _ spd) _ currentRate _ _ _ _ _ _) 
  = b { L.bndRate = currentRate + spd }

setBondNewRate t d ras b@(L.Bond _ _ _ (L.StepUpByDate _ p f1 f2) _ currentRate _ _ _ _ _ _)
  | d < p = b {L.bndRate = applyFloatRate f1 d ras}
  | otherwise = b {L.bndRate = applyFloatRate f2 d ras}

setBondNewRate t d ras b@(L.Bond _ _ _ ii _ _ _ _ _ _ _ _) 
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

getRateAssumptionByIndex :: [RateAssumption] -> Index -> Maybe RateAssumption
getRateAssumptionByIndex ras idx
  = find
      (\case
        (RateCurve _idx _ts) -> (_idx==idx)
        (RateFlat _idx _rval) -> (_idx==idx))
      ras

evalFloaterRate :: Date -> [RateAssumption] -> IR.RateType -> IRate 
evalFloaterRate _ _ (IR.Fix r) = r 
evalFloaterRate d ras (IR.Floater idx spd _r _ mFloor mCap mRounding)
  = let 
      ra = getRateAssumptionByIndex ras idx 
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
      ra = getRateAssumptionByIndex ras idx
      _rate = idx_rate + spd

applyFloatRate (L.CapRate ii _rate) d ras 
  = min _rate (applyFloatRate ii d ras)

applyFloatRate (L.FloorRate ii _rate) d ras 
  = max _rate (applyFloatRate ii d ras)

applicableAdjust :: L.Bond -> Bool
applicableAdjust (L.Bond _ _ _ (L.Fix _ _ ) _ _ _ _ _ _ _ _ ) = False
applicableAdjust (L.Bond _ _ _ (L.InterestByYield _ ) _ _ _ _ _ _ _ _ ) = False
applicableAdjust (L.Bond _ _ _ ii _ _ _ _ _ _ _ _ ) = True


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
       HE.Base ds -> rs { HE.rsRefBalance = queryDeal t (patchDateToStats d ds) }

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

testCalls :: P.Asset a => TestDeal a -> Date -> [C.CallOption] -> Bool
testCalls t d [] = False  -- `debug` ("Empty call optns")
testCalls t d opts = any (testCall t d) opts  -- `debug` ("testing call options"++ show opts)

queryTrigger :: P.Asset a => TestDeal a -> DealCycle -> [Trigger]
queryTrigger t@TestDeal{ triggers = trgs } wt 
  = case trgs of 
      Nothing -> []
      Just _trgs -> Map.findWithDefault [] wt _trgs

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
      _ -> error $ "Failed to match"++show te


runTriggers :: P.Asset a => TestDeal a -> Date -> DealCycle -> (TestDeal a,[ResultComponent])
runTriggers t@TestDeal{status=oldStatus, triggers = Nothing} d dcycle = (t, [])
runTriggers t@TestDeal{status=oldStatus, triggers = Just trgM} d dcycle = 
    (newDeal{triggers = Just (Map.insert dcycle newTriggers trgM)}, newLogs)
  where 
    _trgs = Map.findWithDefault [] dcycle trgM
    testTrgsResult = [ (_trg, (not (trgStatus _trg) || trgStatus _trg && trgCurable _trg) && testTrigger t d _trg)
                      | _trg <- _trgs ] 
    triggeredEffects = [ trgEffects _trg | (_trg,_flag)  <- testTrgsResult, _flag ] -- `debug` ("DEBUG->TG"++show [ testTrigger t d (fst x) | x <- _trgs ])
    newDeal = foldl 
               (\_t _te -> runEffects _t d _te)
               t
               triggeredEffects 
    newStatus = status newDeal 
    newLogs = [DealStatusChangeTo d oldStatus newStatus |  newStatus /= oldStatus] 
    newTriggers = [ if _flag then 
                       _trg { trgStatus = True } 
                    else 
                       _trg     
                   | (_trg,_flag) <- testTrgsResult ]
  
type RevolvingAssumption = (RevolvingPool , [AP.AssumptionBuilder])

-- newtype RunContext a = [TestDeal a, CF.CashFlowFrame , [ActionOnDate] , [RateAssumption] , [C.CallOption] , Maybe RevolvingAssumption]
-- 
-- runner :: P.Asset a => RunContext a -> ([ResultComponent],RunContext a)
-- 
-- runState :: State [ResultComponent] RunContext
-- runState = State runState
-- 
-- run3 :: P.Asset a => TestDeal a -> CF.CashFlowFrame -> [ActionOnDate] -> [RateAssumption] -> [C.CallOption] -> Maybe RevolvingAssumption -> (TestDeal a,[ResultComponent])
-- run3 t@TestDeal{status=Ended} pcf ads rateAssumps calls mRAssumps = (prepareDeal t,[])
-- run3 t pcf [] rateAssumps calls mRAssumps = (prepareDeal t,[])
-- run3 t pcf ads rateAssumps calls mRAssumps = (t,[0])
-- 
-- runWithLog :: P.Asset a => (TestDeal a,[ResultComponent]) -> (TestDeal a -> (TestDeal a,[ResultComponent])) -> (TestDeal a,[ResultComponent])
-- runWithLog (t,logs) runner = 
--   let 
--     (t',newLogs) = runner t
--   in 
--     (t',logs ++ newLogs)
-- 
-- runWithLog2 :: P.Asset a => TestDeal a -> Writer [ResultComponent] (TestDeal a)
 
run2 :: P.Asset a => TestDeal a -> CF.CashFlowFrame -> Maybe [ActionOnDate] -> Maybe [RateAssumption] -> Maybe [C.CallOption] -> Maybe RevolvingAssumption -> [ResultComponent] -> (TestDeal a,[ResultComponent])
run2 t@TestDeal{status=Ended} pcf ads _ _ _ log  = (prepareDeal t,log) `debug` ("Deal Ended")
run2 t pcf (Just []) _ _ _ log  = (prepareDeal t,log)  `debug` "End with Empty ActionOnDate"
run2 t@TestDeal{accounts=accMap,fees=feeMap,triggers=mTrgMap,bonds=bndMap} poolFlow (Just (ad:ads)) rates calls rAssump log
  | (CF.sizeCashFlowFrame poolFlow == 0) && (queryDeal t  AllAccBalance == 0) 
     = let 
         _dealAfterCleanUp = foldl (performAction (getDate ad)) t cleanUpActions `debug` ("CleanUp deal")
       in 
         (prepareDeal _dealAfterCleanUp,log) `debug` "End with pool cf == 0 and all account bals are 0" -- ++ "> Remain Actions" ++ show (ad:ads))
        -- (_dealAfterCleanUp,log) `debug` "End with pool cf == 0 and all account bals are 0 with out console" -- ++ "> Remain Actions" ++ show (ad:ads))
  | otherwise
     = case ad of 
         PoolCollection d _ ->
           if CF.sizeCashFlowFrame poolFlow > 0 then
             let 
               (collected_flow,outstanding_flow) = CF.splitCashFlowFrameByDate poolFlow d EqToLeft 
               accs = depositPoolInflow (collects t) d collected_flow accMap  -- `debug` ("Splitting:"++show(d)++"|||"++show(collected_flow))--  `debug` ("Running AD P"++show(d)) --`debug` ("Deposit-> Collection Date "++show(d)++"with"++show(collected_flow))
               dAfterDeposit = (appendCollectedCF t collected_flow) {accounts=accs}   -- `debug` ("CF size collected"++ show (CF.getTsCashFlowFrame))
               (dRunWithTrigger0,newLogs0) = runTriggers dAfterDeposit d EndCollection  
               waterfallToExe = Map.findWithDefault [] W.EndOfPoolCollection (waterfall t)  -- `debug` ("AD->"++show(ad)++"remain ads"++show(length ads))
               (dAfterAction,rc,newLogs) = foldl (performActionWrap d) (dRunWithTrigger0
                                                                        ,RunContext outstanding_flow rAssump
                                                                        ,log ) waterfallToExe
               (dRunWithTrigger1,newLogs1) = runTriggers dAfterAction d EndCollectionWF -- `debug` ("Running T end of Collection"++show (queryTrigger dAfterAction EndCollectionWF))
             in 
               run2 dRunWithTrigger1 (runPoolFlow rc) (Just ads) rates calls rAssump (log++newLogs0++newLogs1)  -- `debug` ("Logs"++ show d++"is"++ show log++">>"++show newLogs0++show newLogs1)
           else
             run2 t (CF.CashFlowFrame []) (Just ads) rates calls rAssump log    
   
         RunWaterfall d _ ->
           case calls of
             Just callOpts ->
               if testCalls dRunWithTrigger1 d callOpts then 
                 let 
                    dealAfterCleanUp = foldl (performAction d) dRunWithTrigger1 cleanUpActions 
                    endingLogs = Rpt.patchFinancialReports dealAfterCleanUp d newLogs
                 in  
                    (prepareDeal dealAfterCleanUp, endingLogs) -- `debug` ("Called ! "++ show d)
               else
                 run2 dRunWithTrigger1 (runPoolFlow newRc) (Just ads) rates calls rAssump newLogs -- `debug` ("status in run waterfall"++show (status dRunWithTrigger1))
             Nothing ->
               run2 dRunWithTrigger1 (runPoolFlow newRc) (Just ads) rates Nothing rAssump newLogs  -- `debug` ("Run waterfall "++ show d) -- `debug` ("Deal Status"++ show (status dRunWithTrigger1)) -- `debug` ("Call is Nothing")-- `debug` ("Running Waterfall at"++ show d)--  `debug` ("!!!Running waterfall"++show(ad)++"Next ad"++show(head ads)++"PoolFLOW>>"++show(poolFlow)++"AllACCBAL"++show(queryDeal t AllAccBalance))
           where
                (dRunWithTrigger0,newLogs0) = runTriggers t d BeginDistributionWF
                waterfallToExe = Map.findWithDefault 
                                   (Map.findWithDefault [] (W.DistributionDay (status t)) (waterfall t))
                                   W.DefaultDistribution 
                                   (waterfall t)
                runContext = RunContext poolFlow rAssump 
                (dAfterWaterfall,newRc,newLogsWaterfall) = foldl (performActionWrap d) (dRunWithTrigger0,runContext,newLogs0) waterfallToExe  -- `debug` ("Waterfall>>>"++show(waterfallToExe))
                (dRunWithTrigger1,newLogs1) = runTriggers dAfterWaterfall d EndDistributionWF  
                newLogs = log ++ newLogsWaterfall ++ newLogs1

         EarnAccInt d accName ->
           let 
             newAcc = Map.adjust 
                        (\a -> case a of
                                (A.Account _ _ (Just (A.BankAccount _ _ _)) _ _ ) -> (A.depositInt a d)  -- `debug` ("int acc"++show accName)
                                (A.Account _ _ (Just (A.InvestmentAccount idx _ _ _)) _ _ ) -> 
                                  case getRateAssumptionByIndex (fromMaybe [] rates) idx of
                                    Nothing -> a -- `debug` ("error..."++show accName)
                                    Just (RateCurve _ _ts) -> A.depositIntByCurve a _ts d  ) -- `debug` ("int acc"++show accName)
                        accName  
                        accMap
           in 
             run2 (t {accounts = newAcc}) poolFlow (Just ads) rates calls rAssump log
         
         AccrueFee d feeName ->  -- (t , log)
           let 
             newFeeMap = Map.adjust (calcDueFee t d) feeName feeMap -- `debug` ("Accure Fee on Actions")
           in
             run2 (t{fees=newFeeMap}) poolFlow (Just ads) rates calls rAssump log
   
         ResetLiqProvider d liqName -> 
           case liqProvider t of 
             Nothing -> run2 t poolFlow (Just ads) rates calls rAssump log
             (Just mLiqProvider) 
               -> let -- update credit 
                    newLiqMap = Map.adjust (updateLiqProvider t d) liqName mLiqProvider
                  in
                    run2 (t{liqProvider = Just newLiqMap}) poolFlow (Just ads) rates calls rAssump log

         ResetLiqProviderRate d liqName -> 
           case liqProvider t of 
             Nothing -> run2 t poolFlow (Just ads) rates calls rAssump log
             (Just mLiqProvider) 
               -> let -- update rate 
                    newLiqMap = Map.adjust (updateLiqProviderRate t d (fromMaybe [] rates)) liqName mLiqProvider
                  in
                    run2 (t{liqProvider = Just newLiqMap}) poolFlow (Just ads) rates calls rAssump log
        
         DealClosed d ->
           let 
             w = Map.findWithDefault [] W.OnClosingDay (waterfall t)  -- `debug` ("DDD0")
             rc = RunContext poolFlow rAssump  -- `debug` ("DDD1")
             (newDeal,newRc, newLog) = foldl (performActionWrap d) (t, rc, log) w  -- `debug` ("ClosingDay Action:"++show w)
           in 
             run2 newDeal (runPoolFlow newRc) (Just ads) rates calls rAssump newLog -- `debug` ("New pool flow"++show (runPoolFlow newRc))

         ChangeDealStatusTo d s -> run2 (t{status=s}) poolFlow (Just ads) rates calls rAssump log

         ResetIRSwapRate d sn -> 
           let
             _rates = fromMaybe [] rates
             newRateSwap_rate = Map.adjust (updateRateSwapRate _rates d) sn  <$>  rateSwap t
             newRateSwap_bal = Map.adjust (updateRateSwapBal t d) sn <$> newRateSwap_rate
           in 
             run2 (t{rateSwap = newRateSwap_bal}) poolFlow (Just ads) rates calls rAssump log

         InspectDS d ds -> 
           let 
             newlog = 
                case ds of 
                  TriggersStatusAt dc idx -> InspectBool d ds $ queryDealBool t (patchDateToStats d ds)
                  _ -> InspectBal d ds $ queryDeal t (patchDateToStats d ds)
           in 
             run2 t poolFlow (Just ads) rates calls rAssump $ log++[newlog] -- `debug` ("Add log"++show newlog)
         
         ResetBondRate d bn -> 
             let 
               newBndMap = case rates of 
                             Nothing -> error ("No rate assumption for floating bond:"++bn)
                             (Just _rates) -> Map.adjustWithKey 
                                              (\k v-> setBondNewRate t d _rates v)
                                              bn
                                              bndMap -- `debug` ("Reset bond"++show bn)
             in 
               run2 t{bonds = newBndMap} poolFlow (Just ads) rates calls rAssump log
         BuildReport sd ed ->
             let 
               bsReport = Rpt.buildBalanceSheet t ed 
               cashReport = Rpt.buildCashReport t sd ed 
               newlog = FinancialReport sd ed bsReport cashReport
             in 
               run2 t poolFlow (Just ads) rates calls rAssump $ log++[newlog] 
               
         _ -> error $ "Failed to match action on Date"++ show ad
         where
           cleanUpActions = Map.findWithDefault [] W.CleanUp (waterfall t) -- `debug` ("Running AD"++show(ad))


run2 t (CF.CashFlowFrame []) Nothing Nothing Nothing Nothing log
  = run2 t pcf (Just ads) Nothing Nothing Nothing log  -- `debug` ("Init Done >>Last Action#"++show (length ads)++"F/L"++show (head ads)++show (last ads))
  where
    (t, ads,pcf,rcurves,clls,revolveAssump) = getInits t Nothing  

run2 t (CF.CashFlowFrame []) _ _ _ _ log = (prepareDeal t,log) -- `debug` ("End with pool CF is []")
 

data ExpectReturn = DealStatus
                  | DealPoolFlow
                  | DealPoolFlowPricing
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

runDeal :: P.Asset a => TestDeal a -> ExpectReturn -> Maybe AP.ApplyAssumptionType-> Maybe AP.BondPricingInput
        -> (TestDeal a,Maybe CF.CashFlowFrame, Maybe [ResultComponent],Maybe (Map.Map String L.PriceResult))
runDeal t _ assumps bpi =
    (finalDeal, Just pcf, Just (getRunResult finalDeal++logs), bndPricing) -- `debug` ("Run Deal end with")
  where
    (newT, ads,pcf,rcurves,calls,revolvingAssump) = getInits t assumps -- `debug` ("runDeal init line") 
    (finalDeal,logs) = run2 (removePoolCf newT) pcf (Just ads) (Just rcurves) calls revolvingAssump [] -- `debug` ("start status"++show (status t) )-- `debug` ("run2 rAssump>>"++show revolvingAssump++"1st Action"++ show (head ads)++"PCF size"++show (CF.sizeCashFlowFrame pcf))
    bndPricing = case bpi of
                   Nothing -> Nothing   --  `debug` ("pricing bpi with Nothing")
                   Just _bpi -> Just (priceBonds finalDeal _bpi)  -- `debug` ("Pricing with")

getRunResult :: TestDeal a -> [ResultComponent]
getRunResult t = os_bn_i ++ os_bn_b
  where 
    bs = Map.elems $ bonds t
    os_bn_b = [ BondOutstanding (L.bndName _b) (L.bndBalance _b) (getBondBegBal t (L.bndName _b)) | _b <- bs ]
    os_bn_i = [ BondOutstandingInt (L.bndName _b) (L.bndDueInt _b) (getBondBegBal t (L.bndName _b)) | _b <- bs ]

prepareDeal :: TestDeal a -> TestDeal a
prepareDeal t@TestDeal {bonds = bndMap} 
  = t {bonds = Map.map L.consolStmt bndMap}  -- `debug` ("Consolidation in Preparing")

buildRateCurves :: [RateAssumption]-> [AP.AssumptionBuilder] -> [RateAssumption] 
buildRateCurves rs (assump:assumps) = 
    case assump of 
      AP.InterestRateConstant i f -> buildRateCurves (RateFlat i f:rs) assumps
      AP.InterestRateCurve i ds -> buildRateCurves ((RateCurve i ds):rs) assumps
      _ -> buildRateCurves rs assumps    
    where  
        dsToTs ds = IRateCurve $ map (\(d,f) -> TsPoint d f ) ds
buildRateCurves rs [] = rs

getRevolvingCurve :: [AP.AssumptionBuilder] -> Maybe (RevolvingPool ,[AP.AssumptionBuilder])
getRevolvingCurve [] = Nothing
getRevolvingCurve (assump:assumps) = 
  case assump of 
    AP.AvailableAssets afs assumpsForRevolving -> Just (afs, assumpsForRevolving)
    -- AP.AvailableMortgage afs assumpsForRevolving -> Just (afs, assumpsForRevolving)
    -- AP.AvailableInstallment afs assumpsForRevolving -> Just (afs, assumpsForRevolving)
    -- AP.AvailableLoan afs assumpsForRevolving -> Just (afs, assumpsForRevolving)
    -- AP.AvailableLease afs assumpsForRevolving -> Just (afs, assumpsForRevolving)
    _ -> getRevolvingCurve assumps


buildCallOptions :: Maybe [C.CallOption] -> [AP.AssumptionBuilder] -> Maybe [C.CallOption]
buildCallOptions rs (assump:assumps) =
    case assump of  
      AP.CallWhen opts -> buildCallOptions (Just opts) assumps --`debug` ("assump in build"++show(assumps))
      _ -> buildCallOptions rs assumps

buildCallOptions Nothing [] =  Nothing
buildCallOptions rs [] =  rs


appendCollectedCF :: TestDeal a -> CF.CashFlowFrame -> TestDeal a
appendCollectedCF t (CF.CashFlowFrame []) = t
appendCollectedCF t@TestDeal { pool = mpool } cf@(CF.CashFlowFrame _trs)
  = case P.futureCf mpool of 
      Nothing -> t {pool = mpool {P.futureCf = Just cf}}
      Just _p -> t {pool = mpool {P.futureCf = Just (CF.appendCashFlow _p _trs)}}

removePoolCf :: TestDeal a -> TestDeal a
removePoolCf t@TestDeal {pool = _pool}
  = case P.futureCf _pool of 
      Nothing -> t 
      Just _cf -> t {pool = _pool {P.futureCf = Nothing}}

setFutureCF :: TestDeal a-> CF.CashFlowFrame -> TestDeal a
setFutureCF t@TestDeal{ pool = mpool} cf = 
    t {pool = newPool}
    where 
    newPool = mpool {P.futureCf = Just cf}

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


runPool2 :: P.Asset a => P.Pool a -> Maybe AP.ApplyAssumptionType -> [CF.CashFlowFrame]
runPool2 (P.Pool [] (Just cf) asof _) Nothing = [cf]
runPool2 (P.Pool [] (Just cf) asof _) (Just (AP.PoolLevel [])) = [cf]
runPool2 (P.Pool [] (Just (CF.CashFlowFrame txn)) asof _) (Just (AP.PoolLevel assumps)) = [ (P.projCashflow (ACM.ScheduleMortgageFlow asof txn) asof assumps) ] -- `debug` ("PROJ in schedule flow")
runPool2 (P.Pool as _ asof _) Nothing = map (\x -> P.calcCashflow x asof) as -- `debug` ("RUNPOOL-> calc cashflow")
runPool2 (P.Pool as Nothing asof _) (Just applyAssumpType)
  = case applyAssumpType of
       AP.PoolLevel [] -> map (\x -> P.calcCashflow x asof) as  -- `debug` ("In Run pool"++show as)
       AP.PoolLevel assumps -> map (\x -> P.projCashflow x asof assumps) as  -- `debug` (">> Single Pool")
       AP.ByIndex idxAssumps _ ->
         let
           numAssets = length as
           _assumps = map (AP.lookupAssumptionByIdx idxAssumps) [0..(pred numAssets)] -- `debug` ("Num assets"++ show numAssets)
         in
           zipWith (\x a -> P.projCashflow x asof a) as _assumps


getInits :: P.Asset a => TestDeal a -> Maybe AP.ApplyAssumptionType ->
    (TestDeal a,[ActionOnDate], CF.CashFlowFrame, [RateAssumption],Maybe [C.CallOption], Maybe (RevolvingPool ,[AP.AssumptionBuilder]))
getInits t mAssumps 
  = (newT, allActionDates, pCollectionCfAfterCutoff, rateCurves, callOptions, revolvingCurves)   `debug` ("init done actions->"++ show (head allActionDates))
  where
    dealAssumps = case mAssumps of
                    Just (AP.PoolLevel []) -> []
                    Just (AP.PoolLevel _aps) -> fst $ AP.splitAssumptions _aps ([],[])
                    Just (AP.ByIndex _ _aps) -> _aps
                    Nothing -> []
    
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
    inspectDates = let 
                     m_inspect_vars = find (\case
                                             (AP.InspectOn _ ) -> True
                                             _ -> False)
                                           dealAssumps 
                   in
                     case m_inspect_vars of 
                       Just (AP.InspectOn inspect_vars) -> concat [[ InspectDS _d ds | _d <- genSerialDatesTill2 II startDate dp endDate]  | (dp,ds) <- inspect_vars ]
                       Nothing -> []  -- `debug` ("M inspect"++show dealAssumps)
    financialRptDates = case find (\case (AP.BuildFinancialReport _) -> True
                                         _ -> False )  dealAssumps of 
                            Nothing -> [] -- `debug` ("No report date found -> "++ show dealAssumps)
                            Just (AP.BuildFinancialReport dp) 
                              -> let 
                                   _ds = genSerialDatesTill2 II startDate dp endDate 
                                   _ds2 = tail _ds
                                 in [ BuildReport _sd _ed  | (_sd,_ed) <- zip _ds _ds2 ] 

    irSwapRateDates = case rateSwap t of
                        Nothing -> []
                        Just rsm -> Map.elems $ Map.mapWithKey 
                                                 (\k x -> let 
                                                           resetDs = (genSerialDatesTill2 IE startDate (HE.rsSettleDates x) endDate)
                                                          in 
                                                           ((flip ResetIRSwapRate) k) <$> resetDs)
                                                 rsm
    -- bond rate resets 
    bndRateResets = let 
                      rateAdjBnds = Map.filter applicableAdjust $ bonds t
                      bndWithDate = Map.toList $ Map.map (\b -> L.buildRateResetDates (L.bndInterestInfo b) startDate endDate) rateAdjBnds
                    in 
                      [ ResetBondRate bdate bn | (bn,bdates) <- bndWithDate , bdate     <- bdates ]

    stopDate = find 
                 (\case
                   (AP.StopRunBy d) -> True
                   _ -> False)
                 dealAssumps 
    
    _actionDates = let 
                     a = concat [bActionDates,pActionDates,iAccIntDates
                                ,feeAccrueDates,liqResetDates,dealStageDates
                                ,concat irSwapRateDates,inspectDates, bndRateResets,financialRptDates] -- `debug` ("fee acc dates"++show feeAccrueDates)
                   in
                     case dates t of 
                       (PreClosingDates _ _ _ _ _ _) -> sortBy sortActionOnDate $ (DealClosed closingDate):a  -- `debug` ("add a closing date"++show closingDate)
                       _ -> sortBy sortActionOnDate a
                      
    allActionDates = case stopDate of
                       Just (AP.StopRunBy d) -> filter (\x -> getDate x < d) _actionDates
                       Nothing ->  _actionDates   -- `debug` ("Action days") -- `debug` (">>action dates done"++show(_actionDates))

    poolCf = P.aggPool $ runPool2 (pool t) mAssumps -- `debug` ("agg pool flow")
    poolCfTs = filter (\txn -> CF.getDate txn >= startDate) $ CF.getTsCashFlowFrame poolCf -- `debug` ("Pool Cf in pool>>"++show poolCf++"\n start date"++ show startDate)
    pCollectionCfAfterCutoff = CF.CashFlowFrame $ CF.aggTsByDates poolCfTs (getDates pActionDates)  -- `debug`  (("poolCf "++ show poolCfTs) )
    rateCurves = buildRateCurves [] dealAssumps  
    revolvingCurves = getRevolvingCurve dealAssumps -- `debug` ("Getting revolving Curves")
                      
    callOptions = buildCallOptions Nothing dealAssumps 

    -- Expense Override
    newFeeMap = case find 
                       (\case
                         (AP.ProjectedExpense _ _) -> True    
                         _ -> False)
                       dealAssumps of 
                         Nothing -> fees t 
                         Just (AP.ProjectedExpense fn projectedFlow) -> 
                           Map.adjust
                             (\x -> x {F.feeType = F.FeeFlow projectedFlow})
                             fn
                             (fees t) 
    newT = t {fees = newFeeMap} 




depositInflow :: W.CollectionRule -> Date -> CF.TsRow -> Map.Map AccountName A.Account -> Map.Map AccountName A.Account
depositInflow (W.Collect s an) d row amap 
  = Map.adjust (A.deposit amt d (PoolInflow s)) an amap
    where 
      amt = case s of 
              W.CollectedInterest   -> CF.mflowInterest row
              W.CollectedPrincipal  -> CF.mflowPrincipal row
              W.CollectedRecoveries -> CF.mflowRecovery row
              W.CollectedPrepayment -> CF.mflowPrepayment row
              W.CollectedRental     -> CF.mflowRental row
              W.CollectedPrepaymentPenalty -> CF.mflowPrepaymentPenalty row

depositInflow (W.CollectByPct s splitRules) d row amap    --TODO need to check 100%
  = foldr
      (\(accName,accAmt) accM -> 
        Map.adjust (A.deposit accAmt d (PoolInflow s)) accName accM)
      amap
      amtsToAccs
    where 
      amtsToAccs = [ (an, mulBR amt splitRate) | (splitRate, an) <- splitRules]
      amt = case s of 
              CollectedInterest   -> CF.mflowInterest row
              CollectedPrincipal  -> CF.mflowPrincipal row
              CollectedRecoveries -> CF.mflowRecovery row
              CollectedPrepayment -> CF.mflowPrepayment row
              CollectedRental     -> CF.mflowRental row
              CollectedPrepaymentPenalty -> CF.mflowPrepaymentPenalty row

depositInflowByRules :: [W.CollectionRule] -> Date -> CF.TsRow -> Map.Map AccountName A.Account ->  Map.Map AccountName A.Account
depositInflowByRules rs d row amap 
  = foldr 
      (\r accMap -> depositInflow r d row accMap)
      amap
      rs

depositPoolInflow :: [W.CollectionRule] -> Date -> CF.CashFlowFrame -> Map.Map String A.Account -> Map.Map String A.Account
depositPoolInflow rules d (CF.CashFlowFrame []) amap = amap -- `debug` ("Deposit inflow Nothing")
depositPoolInflow rules d (CF.CashFlowFrame txn) amap =
  foldr 
      (depositInflowByRules rules d)
      amap
      txn

$(deriveJSON defaultOptions ''ExpectReturn)