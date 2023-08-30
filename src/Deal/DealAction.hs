{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Deal.DealAction (performActionWrap,performAction,calcDueFee
                       ,testTrigger,RunContext(..),updateLiqProvider
                       ,calcDueInt,projAssetUnion,priceAssetUnion) 
  where

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
import qualified AssetClass.AssetBase as ACM
import AssetClass.Mortgage
import AssetClass.Lease
import AssetClass.Loan
import AssetClass.Installment
import qualified Call as C
import qualified InterestRate as IR
import qualified Analytics as AN

import Deal.DealBase
import Deal.DealQuery
import Deal.DealDate

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
import Cashflow (CashFlowFrame(CashFlowFrame))
debug = flip trace

calcDayToPoolDate :: TestDeal a -> Date -> Date 
calcDayToPoolDate t calcDay 
  = CF.mflowDate $ last pFlows -- `debug` ("calDayToPoolDate"++show calcDay ++">>>>>"++show pFlows)
    where 
      pFlows = getPoolFlows t Nothing (Just calcDay) EI  -- II here is not being used


getPoolFlows :: TestDeal a -> Maybe Date -> Maybe Date -> RangeType -> [CF.TsRow]
getPoolFlows t@TestDeal{ pool = _pool } sd ed rt =
  case (sd,ed) of
    (Nothing,Nothing) ->  _trs
    (Nothing,Just _ed) -> case rt of 
                             EI -> filter (\x -> CF.getDate x <= _ed) _trs
    (Just _sd,Nothing) ->  cutBy Inc Future _sd _trs 
    (Just _sd,Just _ed) -> case rt of 
                             IE -> filter (\x -> (CF.getDate x >= _sd) && (CF.getDate x < _ed)) _trs
                             EI -> filter (\x -> (CF.getDate x > _sd) && (CF.getDate x <= _ed)) _trs
  where
    _projCf = fromMaybe (CF.CashFlowFrame []) (P.futureCf _pool)
    _trs =  CF.getTsCashFlowFrame _projCf


testTrigger :: P.Asset a => TestDeal a -> Date -> Trigger -> Bool 
testTrigger t d trigger@Trigger{ trgStatus=st,trgCurable=cure,trgCondition=cond } 
  | not cure && st = True 
  | otherwise = testPre d t cond 


updateTrigger :: P.Asset a => TestDeal a -> Date -> Trigger -> Trigger
updateTrigger t d trigger@Trigger{ trgStatus=st,trgCurable=cure,trgCondition=cond}
  | testTrigger t d trigger = trigger {trgStatus = True}  
  | otherwise = trigger


pricingAssets :: PricingMethod -> [ACM.AssetUnion] -> Date -> Amount 
pricingAssets (BalanceFactor currentfactor defaultfactor) assets d = 0 

calcLiquidationAmount :: PricingMethod -> P.Pool a -> Date -> Amount
calcLiquidationAmount alm pool d 
  = case alm of 
      BalanceFactor currentFactor defaultFactor ->
          case P.futureCf pool of 
            Nothing -> 0  -- `debug` ("No futureCF")
            Just _futureCf@(CashFlowFrame trs) ->
                let 
                  poolInflow = CF.getEarlierTsCashFlowFrame _futureCf d -- `debug` ("liq:"++show _futureCf++"D"++ show d)
                  earlierTxns = cutBy Exc Past d trs
                  currentDefaulBal = sum $ map (\x -> (CF.mflowDefault x) - (CF.mflowRecovery x) - (CF.mflowLoss x)) earlierTxns
                in 
                  case poolInflow of 
                    Nothing -> 0  -- `debug` ("No pool Inflow")
                    Just _ts ->   -- TODO need to check if missing last row
                        (mulBR (CF.mflowBalance _ts) currentFactor) + (mulBR currentDefaulBal defaultFactor) 

      PV discountRate recoveryPct ->
          case P.futureCf pool of
            Nothing -> 0 
            Just (CashFlowFrame trs) ->
                let 
                  futureTxns = cutBy Inc Future d trs
                  earlierTxns = cutBy Exc Past d trs 
                  pvCf = sum $ map (\x -> AN.pv2  discountRate  d (CF.getDate x) (CF.tsTotalCash x)) futureTxns 
                  currentDefaulBal = sum $ map (\x -> (CF.mflowDefault x) - (CF.mflowRecovery x) - (CF.mflowLoss x)) earlierTxns
                in 
                  pvCf + mulBI currentDefaulBal recoveryPct

liquidatePool :: PricingMethod -> T.Day -> String -> TestDeal a -> TestDeal a
liquidatePool lq d accName t =
  t {accounts = Map.adjust updateFn accName accs} -- `debug` ("Accs->"++show(accs))
  where
     proceeds = calcLiquidationAmount lq (pool t) d
     updateFn = A.deposit proceeds d LiquidationProceeds
     accs = accounts t



calcDueFee :: P.Asset a => TestDeal a -> Date -> F.Fee -> F.Fee
calcDueFee t calcDay f@(F.Fee fn (F.FixFee amt) fs fd fdDay fa _ _)
  | isJust fdDay = f  
  | calcDay >= fs && (isNothing fdDay) = f{ F.feeDue = amt, F.feeDueDate = Just calcDay} -- `debug` ("DEBUG--> init with amt "++show(fd)++show amt)
  | otherwise = f

calcDueFee t calcDay f@(F.Fee fn (F.AnnualRateFee feeBase r) fs fd Nothing fa lpd _)
  | calcDay >= fs = calcDueFee t calcDay f {F.feeDueDate = Just fs }
  | otherwise = f 

calcDueFee t calcDay f@(F.Fee fn (F.AnnualRateFee feeBase _r) fs fd (Just _fdDay) fa lpd _)
  = f{ F.feeDue=fd+newDue, F.feeDueDate = Just newDueDay }  -- `debug` ("Fee DUE new Due "++show calcDay ++show baseBal ++show(newDue))                   
      where 
        accrueStart = _fdDay
        collectionEndDay = calcDayToPoolDate t calcDay
        (baseBal,newDueDay) = case feeBase of
                                CurrentPoolBalance ->  (CF.mflowWeightAverageBalance accrueStart calcDay $ getPoolFlows t Nothing Nothing II,collectionEndDay)-- `debug` ("FeeBase" ++ show (getPoolFlows t Nothing Nothing II))
                                -- CurrentPoolBegBalance ->  CF.mflowWeightAverageBalance accrueStart calcDay $ getPoolFlows t Nothing Nothing
                                OriginalPoolBalance -> (mulBR (P.getIssuanceField (pool t) IssuanceBalance) (yearCountFraction DC_ACT_365F accrueStart calcDay),collectionEndDay)
                                OriginalBondBalance -> (mulBR (queryDeal t OriginalBondBalance) (yearCountFraction DC_ACT_365F accrueStart calcDay),calcDay)
                                CurrentBondBalance -> (Map.foldr (\v a-> a + L.weightAverageBalance accrueStart calcDay v ) 0.0 (bonds t),calcDay)
                                CurrentBondBalanceOf bns -> (Map.foldr (\v a-> a + L.weightAverageBalance accrueStart calcDay v ) 0.0 (getBondByName t (Just bns)),calcDay)
        r = toRational $ queryDealRate t _r 
        newDue = mulBR baseBal r

calcDueFee t calcDay f@(F.Fee fn (F.PctFee (PoolCurCollection its) r ) fs fd fdDay fa lpd _)
  = f { F.feeDue = newDueFee, F.feeDueDate = Just calcDay } -- `debug` ("BAL"++show baseBal++"New Fee Due"++ show newDueFee)
    where 
      baseBal = sum [ queryDeal t (PoolCollectionHistory it lastBegDay calcDay)  | it <- its ]
      newDueFee = fd + mulBR baseBal (toRational (queryDealRate t r))
      lastBegDay = case fdDay of
                     (Just _fdDay) -> _fdDay
                     Nothing -> fs

calcDueFee t calcDay f@(F.Fee fn (F.PctFee ds _r ) fs fd fdDay fa lpd _)
  = f { F.feeDue = fd + mulBR baseBal r, F.feeDueDate = Just calcDay }
    where 
      r = toRational $ queryDealRate t _r
      baseBal = queryDeal t (patchDateToStats calcDay ds)
      lastBegDay = case fdDay of
                     (Just _fdDay) -> _fdDay
                     Nothing -> fs

calcDueFee t calcDay f@(F.Fee fn (F.FeeFlow ts)  fs fd _ fa mflpd _)
  = f{ F.feeDue = newFeeDue
      ,F.feeDueDate = Just calcDay
      ,F.feeType = F.FeeFlow futureDue} 
    where
      (currentNewDue,futureDue) = splitTsByDate ts calcDay 
      cumulativeDue = sumValTs currentNewDue
      newFeeDue =  cumulativeDue + fd  

calcDueFee t calcDay f@(F.Fee fn (F.RecurFee p amt)  fs fd mLastAccDate fa _ _)
  | periodGaps == 0 = f 
  | otherwise = f{ F.feeDue = amt * fromIntegral periodGaps + fd
                 , F.feeDueDate = Just (T.addDays 1 calcDay) } -- `debug` ("periods"++show periodGaps)
  where
    accDates = case mLastAccDate of 
                      Nothing -> genSerialDatesTill2 NO_IE (T.addDays 1 fs) p calcDay 
                      Just lastAccDate -> genSerialDatesTill2 NO_IE lastAccDate p calcDay 
    periodGaps = length accDates 


calcDueFee t calcDay f@(F.Fee fn (F.NumFee p s amt) fs fd Nothing fa lpd _)
  | calcDay >= fs = calcDueFee t calcDay f {F.feeDueDate = Just fs }
  | otherwise = f 

calcDueFee t calcDay f@(F.Fee fn (F.NumFee p s amt) fs fd (Just _fdDay) fa lpd _)
  | _fdDay == calcDay = f 
  | periodGap == 0 = f 
  | otherwise = f { F.feeDue = fd+newFeeDueAmt , F.feeDueDate = Just calcDay } 
  where 
    dueDates = projDatesByPattern p _fdDay (pred calcDay)
    periodGap = length dueDates  -- `debug` ("Due Dates"++ show dueDates)
    baseCount = queryDealInt t (patchDateToStats calcDay s) calcDay
    newFeeDueAmt = fromRational $ mulBInt amt $ baseCount * periodGap -- `debug` ("amt"++show amt++">>"++show baseCount++">>"++show periodGap)

calcDueFee t calcDay f@(F.Fee fn (F.TargetBalanceFee dsDue dsPaid) fs fd _ fa lpd _)
  = f { F.feeDue = dueAmt, F.feeDueDate = Just calcDay}
    where
      dsDueD = patchDateToStats calcDay dsDue 
      dsPaidD = patchDateToStats calcDay dsPaid
      dueAmt = max 0 $ (queryDeal t dsDueD) - (queryDeal t dsPaidD)

disableLiqProvider :: P.Asset a => TestDeal a -> Date -> CE.LiqFacility -> CE.LiqFacility
disableLiqProvider _ d liq@CE.LiqFacility{CE.liqEnds = Just endDate } 
  | d > endDate = liq{CE.liqCredit = Just 0}
  | otherwise = liq

disableLiqProvider _ d liq@CE.LiqFacility{CE.liqEnds = Nothing }  = liq

updateLiqProvider :: P.Asset a => TestDeal a -> Date -> CE.LiqFacility -> CE.LiqFacility
updateLiqProvider t d liq@CE.LiqFacility{CE.liqType = liqType, CE.liqCredit = curCredit} -- refresh available balance
  = disableLiqProvider t d $ liq { CE.liqCredit = newCredit } 
    where 
      newCredit = case liqType of 
                     CE.ReplenishSupport _ b -> max b <$> curCredit
                     CE.ByPct ds _r -> min (mulBR (queryDeal t ds) _r) <$> curCredit
                     _ -> curCredit

updateLiqProvider t d liq = disableLiqProvider t d liq

calcDueInt :: P.Asset a => TestDeal a -> Date -> L.Bond -> L.Bond
calcDueInt t calc_date b@(L.Bond _ _ oi io _ r dp di Nothing _ lastPrinPay _ ) 
 | calc_date <= closingDate = b
 | otherwise = calcDueInt t calc_date (b {L.bndDueIntDate = Just closingDate })
   where 
     closingDate = getClosingDate (dates t)

calcDueInt t calc_date b@(L.Bond bn L.Z bo bi bond_bal bond_rate _ _ _ lstIntPay _ _) 
  = b {L.bndDueInt = 0 }

calcDueInt t calc_date b@(L.Bond bn L.Equity bo (L.InterestByYield y) bond_bal _ _ int_due _ lstIntPay _ mStmt)
  = b {L.bndDueInt = newDue }  -- `debug` ("Yield Due Int >>"++ show bn++">> new due"++ show newDue++">> old due"++ show int_due )
  where
  newDue = L.backoutDueIntByYield calc_date b

calcDueInt t calc_date b@(L.Bond bn bt bo bi bond_bal bond_rate _ int_due (Just int_due_date) lstIntPay _ _ ) 
  | calc_date == int_due_date = b
  | otherwise = b {L.bndDueInt = new_due_int+int_due,L.bndDueIntDate = Just calc_date }  --  `debug` ("Due INT"++show calc_date ++">>"++show(bn)++">>"++show int_due++">>"++show(new_due_int))
              where
                dc = case bi of 
                       L.Floater _ _ _ _ _dc _ _ -> _dc 
                       L.Fix _ _dc -> _dc 
                new_due_int = calcInt (bond_bal+int_due) int_due_date calc_date bond_rate dc  -- `debug` ("Bond bal"++show bond_bal++">>"++show int_due_date++">>"++ show calc_date++">>"++show bond_rate)


calcDuePrin :: P.Asset a => TestDeal a -> T.Day -> L.Bond -> L.Bond
calcDuePrin t calc_date b@(L.Bond bn L.Sequential bo bi bond_bal _ prin_arr int_arrears _ _ _ _) =
  b {L.bndDuePrin = duePrin} 
  where
    duePrin = bond_bal 

calcDuePrin t calc_date b@(L.Bond bn (L.Lockout cd) bo bi bond_bal _ prin_arr int_arrears _ _ _ _) =
  if cd > calc_date then 
    b {L.bndDuePrin = 0}
  else
    b {L.bndDuePrin = duePrin}
  where
    duePrin = bond_bal 

calcDuePrin t calc_date b@(L.Bond bn (L.PAC schedule) bo bi bond_bal _ prin_arr int_arrears _ _ _ _) =
  b {L.bndDuePrin = duePrin} -- `debug` ("bn >> "++bn++"Due Prin set=>"++show(duePrin) )
  where
    scheduleDue = getValOnByDate schedule calc_date  
    duePrin = max (bond_bal - scheduleDue) 0 -- `debug` ("In PAC ,target balance"++show(schedule)++show(calc_date)++show(scheduleDue))

calcDuePrin t calc_date b@(L.Bond bn (L.PAC_Anchor schedule bns) bo bi bond_bal _ prin_arr int_arrears _ _ _ _) =
  b {L.bndDuePrin = duePrin} -- `debug` ("bn >> "++bn++"Due Prin set=>"++show(duePrin) )
  where
    scheduleDue = getValOnByDate schedule calc_date
    anchor_bond_balance = queryDeal t (CurrentBondBalanceOf bns)
    duePrin = if anchor_bond_balance > 0 then
                 max (bond_bal - scheduleDue) 0
              else
                 bond_bal

calcDuePrin t calc_date b@(L.Bond bn L.Z bo bi bond_bal bond_rate prin_arr int_arrears _ lstIntPay _ _) =
  if all isZbond activeBnds then
      b {L.bndDuePrin = bond_bal} -- `debug` ("bn >> "++bn++"Due Prin set=>"++show(duePrin) )
  else 
      b {L.bndDuePrin = 0, L.bndBalance = new_bal, L.bndLastIntPay=Just calc_date} -- `debug` ("bn >> "++bn++"Due Prin set=>"++show(duePrin) )
  where
    isZbond (L.Bond _ L.Z _ _ _ _ _ _ _ _ _ _) = True
    isZbond L.Bond {} = False
    
    activeBnds = filter (\x -> L.bndBalance x > 0) (Map.elems (bonds t))
    new_bal = bond_bal + dueInt
    lastIntPayDay = case lstIntPay of
                      Just pd -> pd
                      Nothing -> getClosingDate (dates t)
    dueInt = calcInt bond_bal lastIntPayDay calc_date bond_rate DC_ACT_365F

calcDuePrin t calc_date b@(L.Bond bn L.Equity bo bi bond_bal _ prin_arr int_arrears _ _ _ _) =
  b {L.bndDuePrin = bond_bal }


priceAssetUnion :: ACM.AssetUnion -> Date -> PricingMethod  -> [AP.AssumptionBuilder] -> PriceResult
priceAssetUnion (ACM.MO m) d pm aps = P.priceAsset m d pm aps 
priceAssetUnion (ACM.LO m) d pm aps = P.priceAsset m d pm aps
priceAssetUnion (ACM.IL m) d pm aps = P.priceAsset m d pm aps
priceAssetUnion (ACM.LS m) d pm aps = P.priceAsset m d pm aps


-- | this would used in `static` revolving ,which assumes the revolving pool will decrease
splitAssetUnion :: [Rate] -> ACM.AssetUnion -> [ACM.AssetUnion]
splitAssetUnion rs (ACM.MO m) = [ ACM.MO a | a <- P.splitWith m rs]
splitAssetUnion rs (ACM.LO m) = [ ACM.LO a | a <- P.splitWith m rs]
splitAssetUnion rs (ACM.IL m) = [ ACM.IL a | a <- P.splitWith m rs]
splitAssetUnion rs (ACM.LS m) = [ ACM.LS a | a <- P.splitWith m rs]

buyRevolvingPool :: Date -> [Rate] -> RevolvingPool -> ([ACM.AssetUnion],RevolvingPool)
buyRevolvingPool _ rs rp@(StaticAsset assets) 
  = let 
      splitedAssets = splitAssetUnion rs <$> assets
      assetBought = head <$> splitedAssets
      assetRemains = last <$> splitedAssets 
    in 
      (assetBought ,StaticAsset assetRemains)

buyRevolvingPool _ rs rp@(ConstantAsset assets)
  = let 
      splitedAssets = splitAssetUnion rs <$> assets
      assetBought = head <$> splitedAssets
    in 
      (assetBought ,rp)

buyRevolvingPool d rs rp@(AssetCurve aus)
  = let
      assets = lookupAssetAvailable rp d 
      splitedAssets = splitAssetUnion rs <$> assets
      assetBought = head <$> splitedAssets
    in 
      (assetBought, rp)

projAssetUnion :: ACM.AssetUnion -> Date -> [AP.AssumptionBuilder] -> CF.CashFlowFrame
projAssetUnion (ACM.MO ast) d assumps = CF.cfInsertHead (CF.MortgageFlow d (P.getCurrentBal ast) 0 0 0 0 0 0 0 Nothing Nothing) $ P.projCashflow ast d assumps
projAssetUnion (ACM.LO ast) d assumps = CF.cfInsertHead (CF.LoanFlow d (P.getCurrentBal ast) 0 0 0 0 0 0 0) $ P.projCashflow ast d assumps
projAssetUnion (ACM.IL ast) d assumps = CF.cfInsertHead (CF.LoanFlow d (P.getCurrentBal ast) 0 0 0 0 0 0 0) $ P.projCashflow ast d assumps
projAssetUnion (ACM.LS ast) d assumps = CF.cfInsertHead (CF.LeaseFlow d (P.getCurrentBal ast) 0 ) $ P.projCashflow ast d assumps

data RunContext a = RunContext{
                  runPoolFlow:: CF.CashFlowFrame
                  ,revolvingAssump:: Maybe (RevolvingPool ,[AP.AssumptionBuilder])
                  }

updateOriginDate2 :: Date -> ACM.AssetUnion -> ACM.AssetUnion
updateOriginDate2 d (ACM.LO m) = ACM.LO $ updateOriginDate m (P.calcAlignDate m d)
updateOriginDate2 d (ACM.MO m) = ACM.MO $ updateOriginDate m (P.calcAlignDate m d)
updateOriginDate2 d (ACM.IL m) = ACM.IL $ updateOriginDate m (P.calcAlignDate m d)
updateOriginDate2 d (ACM.LS m) = ACM.LS $ updateOriginDate m (P.calcAlignDate m d)

evalExtraSupportBalance :: Date -> TestDeal a -> W.ExtraSupport  -> [Balance]
evalExtraSupportBalance d t@TestDeal{accounts=accMap} (W.SupportAccount an _) = [A.accBalance $ accMap Map.! an]
evalExtraSupportBalance d t@TestDeal{liqProvider=Just liqMap} (W.SupportLiqFacility liqName) = [ fromMaybe 0 (CE.liqCredit (liqMap Map.! liqName))]
evalExtraSupportBalance d t (W.MultiSupport supports) = concat $ evalExtraSupportBalance d t <$> supports

drawExtraSupport :: Date -> Amount -> W.ExtraSupport -> TestDeal a -> (TestDeal a,Amount)
drawExtraSupport d amt (W.SupportAccount an (Just (W.ByAccountDraw ln))) t@TestDeal{accounts=accMap, ledgers= Just ledgerMap}
  = let 
      drawAmt = min (A.accBalance (accMap Map.! an)) amt
      oustandingAmt = amt - drawAmt
    in 
      (t {accounts = Map.adjust (A.draw drawAmt d Empty) an accMap
         ,ledgers = Just $ Map.adjust (LD.entryLog drawAmt d (TxnDirection Debit)) ln ledgerMap}
      , oustandingAmt)

drawExtraSupport d amt (W.SupportAccount an Nothing) t@TestDeal{accounts=accMap} 
  = let 
      drawAmt = min (A.accBalance (accMap Map.! an)) amt
      oustandingAmt = amt - drawAmt
    in 
      (t {accounts = Map.adjust (A.draw drawAmt d Empty) an accMap }
      , oustandingAmt) 

drawExtraSupport d amt (W.SupportLiqFacility liqName) t@TestDeal{liqProvider= Just liqMap}
  = let 
      drawAmt = min amt $ fromMaybe 0 $ CE.liqCredit $ liqMap Map.! liqName
      oustandingAmt = amt - drawAmt
    in 
      (t {liqProvider = Just (Map.adjust (CE.draw drawAmt d) liqName liqMap)}
      , oustandingAmt)

drawExtraSupport d amt (W.MultiSupport supports) t
  = foldr 
      (\support (deal,remainAmt) -> drawExtraSupport d remainAmt support deal) 
      (t,amt) 
      supports

performActionWrap :: P.Asset a => Date -> (TestDeal a, RunContext a, [ResultComponent]) -> W.Action -> (TestDeal a, RunContext a, [ResultComponent])
performActionWrap d 
                  (t@TestDeal{ accounts = accsMap }
                  ,rc@RunContext{runPoolFlow=pcf@(CF.CashFlowFrame (tr:trs))
                                ,revolvingAssump=Just (assetForSale,perfAssumps)}
                  ,logs)
                  (W.BuyAsset ml pricingMethod accName) 
   = (t { accounts = newAccMap }, newRc, logs )
    where 
      _assets = lookupAssetAvailable assetForSale d
      assets = updateOriginDate2 d <$> _assets 
                
      valuationOnAvailableAssets = sum [ getPriceValue (priceAssetUnion ast d pricingMethod perfAssumps)  | ast <- assets ]  -- `debug` ("Revolving >> after shift "++ show assets)
      accBal = A.accBalance $ accsMap Map.! accName -- `debug` ("Av")
      limitAmt = case ml of 
                   Just (DS ds) -> queryDeal t (patchDateToStats d ds)
                   Just (DueCapAmt amt) -> amt
                   Nothing -> accBal

      availBal = min limitAmt accBal
      purchaseAmt = case assetForSale of 
                    (StaticAsset _) -> min availBal valuationOnAvailableAssets -- `debug` ("Valuation on rpool"++show valuationOnAvailableAssets)
                    ConstantAsset _ -> availBal 
                    AssetCurve _ -> min availBal valuationOnAvailableAssets   

      purchaseRatio = purchaseAmt / valuationOnAvailableAssets
      purchaseRatios = toRational <$> [purchaseRatio,1-purchaseRatio]

      (assetBought,poolAfterBought) = buyRevolvingPool d purchaseRatios assetForSale
      newAccMap = Map.adjust (A.draw purchaseAmt d PurchaseAsset) accName accsMap
      
      -- newBoughtPcf = (CF.shiftCfToStartDate d) <$> [ projAssetUnion ast d perfAssumps | ast <- assetBought ]
      newBoughtPcf = [ projAssetUnion (updateOriginDate2 d ast) d perfAssumps | ast <- assetBought ]
      poolCurrentTr = CF.buildBegTsRow d tr
      currentPoolFlow = CF.cfInsertHead poolCurrentTr pcf
      --newPcf = foldl CF.mergePoolCf pcf newBoughtPcf  `debug` ("reolvoing cf"++show d++"\n"++show newBoughtPcf++"\n"++"pool cf 1st"++show (CF.cfAt pcf 0))
      newPcf = foldl CF.mergePoolCf currentPoolFlow newBoughtPcf -- `debug` ("reolvoing cf"++show d++"\n"++show newBoughtPcf++"\n"++"pool cf 1st"++show (CF.cfAt pcf 0))
      newRc = rc {runPoolFlow = newPcf
                 ,revolvingAssump = Just (poolAfterBought,perfAssumps)} -- `debug` ("new pool flow"++show newPcf)

performActionWrap d (t, rc, logs) (W.WatchVal ms dss)
  = (t, rc, newLogs ++ logs)
    where 
      newLogs = [ InspectBal d ds (queryDeal t ds) | ds <- dss ] 

performActionWrap d (t, rc, logs) (W.ActionWithPre p actions) 
  | testPre d t p = foldl (performActionWrap d) (t,rc,logs) actions
  | otherwise = (t, rc, logs)

performActionWrap d (t, rc, logs) (W.ActionWithPre2 p actionsTrue actionsFalse) 
  | testPre d t p = foldl (performActionWrap d) (t,rc,logs) actionsTrue
  | otherwise = foldl (performActionWrap d) (t,rc,logs) actionsFalse

performActionWrap d (t,rc, logs) a = (performAction d t a,rc,logs) -- `debug` ("DEBUG: Action on "++ show a)

performAction :: P.Asset a => Date -> TestDeal a -> W.Action -> TestDeal a
performAction d t (W.ActionWithPre2 _pre actionsTrue actionsFalse)
  | testPre d t _pre = foldl (performAction d) t actionsTrue 
  | otherwise  = foldl (performAction d) t actionsFalse 

performAction d t (W.ActionWithPre _pre actions)
  | testPre d t _pre = foldl (performAction d) t actions 
  | otherwise  = t

performAction d t@TestDeal{accounts=accMap} (W.Transfer Nothing an1 an2 mComment) =
  t {accounts = accMapAfterDeposit}
  where
    sourceAcc = accMap Map.! an1
    transferAmt = max 0 (A.accBalance sourceAcc)
    comment = Transfer an1 an2
    accMapAfterDraw = Map.adjust (A.draw transferAmt d comment ) an1 accMap
    accMapAfterDeposit = Map.adjust (A.deposit transferAmt d  comment) an2 accMapAfterDraw

performAction d t@TestDeal{ledgers= Just ledgerM} (W.BookBy (W.PDL ds ledgersList)) =
  t {ledgers = Just newLedgerM}
  where 
    bookedLedger = [(queryDeal t ledgerCap
                     ,queryTxnAmt (ledgerM Map.! ledgerName) (TxnDirection Debit))
                     | (ledgerName, ledgerCap) <- ledgersList ]
    ledgerNames = fst <$> ledgersList

    amtToBook = queryDeal t ds
    amtBooked = sum $ snd <$> bookedLedger
    newAmtToBook = amtToBook - amtBooked
    availableBalances = [ a-b | (a,b) <- bookedLedger]
    amtBookedToLedgers = paySeqLiabilitiesAmt newAmtToBook availableBalances
    
    newLedgerM = foldr 
                   (\(ln,amt) acc -> Map.adjust (LD.entryLog amt d (TxnDirection Debit)) ln acc)
                   ledgerM
                   (zip ledgerNames amtBookedToLedgers)

performAction d t@TestDeal{accounts=accMap, ledgers = Just ledgerM} (W.Transfer (Just (ClearLedger ln)) an1 an2 mComment) =
  t {accounts = accMapAfterDeposit, ledgers = Just newLedgerM}  -- `debug` ("ABCD "++show(d))
  where
    sourceAcc = accMap Map.! an1
    targetAcc = accMap Map.! an2 -- `debug` ("Target>>"++an2)
    targetAmt = queryDeal t (LedgerBalance [ln]) -- assuming (debit -> positvie)
    transferAmt = min (A.accBalance sourceAcc) targetAmt -- `debug` ("Clear PDL"++show d++ show targetAmt)
 
    accMapAfterDraw = Map.adjust (A.draw transferAmt d (TransferBy an1 an2 (ClearLedger ln))) an1 accMap -- `debug` (">>PDL >>Ledger bal"++show d ++ show targetAmt)
    accMapAfterDeposit = Map.adjust (A.deposit transferAmt d (TransferBy an1 an2 (ClearLedger ln))) an2 accMapAfterDraw

    newLedgerM = Map.adjust 
                   (LD.entryLog (negate transferAmt) d (TxnDirection Credit))
                   ln 
                   ledgerM

performAction d t@TestDeal{accounts=accMap} (W.Transfer (Just limit) an1 an2 mComment) =
  t {accounts = accMapAfterDeposit}  
  where
    sourceAcc = accMap Map.! an1
    targetAcc = accMap Map.! an2 
    formulaAmount = case limit of 
                      DuePct r -> mulBR (A.accBalance sourceAcc) r
                      DueCapAmt a -> min a (A.accBalance sourceAcc)
                      DS ds -> queryDeal t (patchDateToStats d ds)
                      TillSource -> queryDeal t (ReserveExcessAt d [an1])
                      TillTarget -> queryDeal t (ReserveAccGapAt d [an2])
    transferAmt = min (max formulaAmount 0) (A.accBalance sourceAcc) 

    accMapAfterDraw = Map.adjust (A.draw transferAmt d (TransferBy an1 an2 limit)) an1 accMap
    accMapAfterDeposit = Map.adjust (A.deposit transferAmt d (TransferBy an1 an2 limit)) an2 accMapAfterDraw


performAction d t@TestDeal{fees=feeMap, accounts =accMap} (W.PayFee mLimit an fns mSupport) =
  case mSupport of 
    Just support -> fst $ drawExtraSupport d supportPaidOut support dealAfterAcc
    Nothing -> dealAfterAcc
  where
    feesToPay = map (feeMap Map.!) fns
    feeDueAmts = case mLimit of 
                   Nothing -> map F.feeDue feesToPay  
                   Just (DuePct pct) -> map (\x -> mulBR (F.feeDue x) pct ) feesToPay
                   Just (DueCapAmt amt) -> prorataFactors (F.feeDue <$> feesToPay) amt

    availAccBal = A.accBalance (accMap Map.! an)
    availBal = case mSupport of
                 Nothing -> availAccBal
                 Just support -> availAccBal + sum (evalExtraSupportBalance d t support)

    -- total actual pay out
    actualPaidOut = min availBal $ sum feeDueAmts -- `debug` ("Fee Due Amounts"++show(feeDueAmts))

    feesAmountToBePaid = zip feesToPay $ prorataFactors feeDueAmts actualPaidOut
    feesPaid = map (\(f,amt) -> F.payFee d amt f) feesAmountToBePaid
    -- fee map updated
    feeMapUpdated = Map.union (Map.fromList $ zip fns feesPaid) feeMap
    -- update primary account map
    accPaidOut = min actualPaidOut availAccBal
    dealAfterAcc = t {accounts = Map.adjust (A.draw accPaidOut d (SeqPayFee fns)) an accMap
                     ,fees = feeMapUpdated}

    supportPaidOut = actualPaidOut - accPaidOut

performAction d t@TestDeal{bonds=bndMap,accounts=accMap} (W.PayInt Nothing an bnds mSupport) =
  case mSupport of 
    Just support -> fst $ drawExtraSupport d supportPaidOut support dealAfterAcc
    Nothing -> dealAfterAcc
  where
    acc = accMap Map.! an
    availAccBal = A.accBalance acc
    bndsToPay = map (bndMap Map.!) bnds

    bndsDueAmts = map L.bndDueInt bndsToPay
    bndsNames = map L.bndName bndsToPay

    availBal = case mSupport of
                 Nothing -> availAccBal
                 Just support -> availAccBal + sum ( evalExtraSupportBalance d t support)
    actualPaidOut = min availBal $ sum bndsDueAmts -- `debug` ("due mats"++ show bndsDueAmts ++">>"++ show availBal)
    bndsAmountToBePaid = zip bndsToPay $ prorataFactors bndsDueAmts availBal -- `debug` ("prorata"++ show (prorataFactors bndsDueAmts availBal) )

    -- bond map updated
    bndsPaid = map (\(l,amt) -> L.payInt d amt l) bndsAmountToBePaid
    bndMapUpdated = Map.union (Map.fromList $ zip bndsNames bndsPaid) bndMap
    -- primary account paid out
    accPaidOut = min actualPaidOut availAccBal
    dealAfterAcc = t {accounts = Map.adjust (A.draw actualPaidOut d (PayInt bnds)) an accMap
                     ,bonds = bndMapUpdated}
    supportPaidOut = actualPaidOut - accPaidOut
    

performAction d t (W.AccrueAndPayInt mLimit an bnds mSupport) =
  let 
    dealWithBondDue = performAction d t (W.CalcBondInt bnds)
  in 
    performAction d dealWithBondDue (W.PayInt mLimit an bnds mSupport)

performAction d t (W.CalcAndPayFee mLimit ans fees mSupport) =
  let 
    dealWithFeeDue = performAction d t (W.CalcFee fees)
  in 
    performAction d dealWithFeeDue (W.PayFee mLimit ans fees mSupport)

performAction d t@TestDeal{bonds=bndMap,accounts=accMap} (W.PayIntResidual Nothing an bndName) =
  t {accounts = accMapAfterPay, bonds = bndMapAfterPay}
  where
    availBal = A.accBalance $ accMap Map.! an
    accMapAfterPay = Map.adjust (A.draw availBal d (PayYield bndName)) an accMap
    bndMapAfterPay = Map.adjust (L.payYield d availBal) bndName bndMap

performAction d t@TestDeal{fees=feeMap,accounts=accMap} (W.PayFeeResidual mlimit an feeName) =
  t {accounts = accMapAfterPay, fees = feeMapAfterPay}
  where
    availBal = A.accBalance $ accMap Map.! an
    paidOutAmt = case mlimit of
                   Just (DuePct pct) -> mulBR availBal pct
                   Just (DueCapAmt cap) ->  min cap availBal
                   Nothing -> availBal
    accMapAfterPay = Map.adjust (A.draw paidOutAmt d (PayFeeYield feeName)) an accMap
    feeMapAfterPay = Map.adjust (F.payResidualFee d paidOutAmt) feeName feeMap

-- ^ pay bond till its balance as pct of total balance
--performAction d t@TestDeal{bonds=bndMap,accounts=accMap} (W.PayPrin (Just (RemainBalPct pct)) an [bndName])=  --Need to replace with formula
--  t {accounts = accMapAfterPay, bonds = bndMapAfterPay}
--  where
--    availBal = A.accBalance $ accMap Map.! an
--    targetBnd = bndMap Map.! bndName
--    targetBndBal = L.bndBalance targetBnd
--
--    otherBndBal = queryDeal t CurrentBondBalance - targetBndBal
--
--    _pct = fromRational pct
--    dueAmount = (1/(1-_pct)) * (targetBndBal * (1-_pct) - (_pct * otherBndBal))
--    actAmount = min availBal $ max dueAmount 0
--
--    accMapAfterPay = Map.adjust
--                        (A.draw actAmount d (PayPrin [bndName])) 
--                        an
--                        accMap
--    bndMapAfterPay = Map.adjust (L.payPrin d actAmount) bndName bndMap

performAction d t@TestDeal{bonds=bndMap,accounts=accMap} (W.PayPrin (Just (DS ds)) an bnds Nothing)=  --Need to replace with formula
  t {accounts = accMapAfterPay, bonds =bndsUpdated}
  where
    availBal = A.accBalance $ accMap Map.! an
    
    bndsToPay = filter (\x -> L.bndBalance x > 0) $ map (bndMap Map.!) bnds
    bndsWithDue = map (calcDuePrin t d) bndsToPay  --
    bndsDueAmts = map L.bndDuePrin bndsWithDue
    
    payAmount = min (sum bndsDueAmts) $ min availBal $ queryDeal t $ patchDateToStats d ds -- `debug` ("Query with "++show (patchedDs))

    bndsAmountToBePaid = zip bndsToPay $ prorataFactors bndsDueAmts payAmount  -- (bond, amt-allocated)

    bndsPaid = map (\(b,amt) -> L.payPrin d amt b) bndsAmountToBePaid
    bndsUpdated = Map.union (Map.fromList $ zip bnds bndsPaid) bndMap


    accMapAfterPay = Map.adjust
                        (A.draw payAmount d (TxnComments [PayPrin bnds,UsingDS ds])) 
                        an
                        accMap  -- `debug` ("payOutAmt"++show (queryDeal t patchedDs))

performAction d t@TestDeal{bonds=bndMap,accounts=accMap} (W.PayPrin Nothing an bnds Nothing) =
  t {accounts = accMapAfterPay, bonds = bndMapUpdated} 
  where
    acc = accMap Map.! an

    bndsToPay = filter (\x -> L.bndBalance x > 0) $ map (bndMap Map.!) bnds
    availBal = A.accBalance acc
    bndsWithDue = map (calcDuePrin t d) bndsToPay  --
    bndsDueAmts = map L.bndDuePrin bndsWithDue

    actualPaidOut = min availBal $ sum bndsDueAmts -- `debug` ("bonds totoal due ->"++show(bndsDueAmts))
    bndsAmountToBePaid = zip bndsWithDue (prorataFactors bndsDueAmts availBal)
    bndsPaid = map (\(l,amt) -> L.payPrin d amt l) bndsAmountToBePaid --  `debug` ("pay prin->>>To"++show(bnds))

    bndMapUpdated =  Map.union (Map.fromList $ zip bnds bndsPaid) bndMap
    accMapAfterPay = Map.adjust (A.draw actualPaidOut d (PayPrin bnds)) an accMap

performAction d t@TestDeal{accounts=accMap, bonds=bndMap} (W.PayPrinResidual an bnds) = 
  t {accounts = accMapAfterPay, bonds = bndMapUpdated} -- `debug` ("Bond Prin Pay Result"++show(bndMapUpdated))
  where
    acc = accMap Map.! an

    bndsToPay = filter (\x -> L.bndBalance x > 0) $ map (bndMap Map.!) bnds
    availBal = A.accBalance acc
    bndsDueAmts = map L.bndBalance bndsToPay

    actualPaidOut = min availBal $ sum  bndsDueAmts -- `debug` ("bonds totoal due ->"++show(bndsDueAmts))
    bndsAmountToBePaid = zip bndsToPay (prorataFactors bndsDueAmts availBal)
    bndsPaid = map (\(l,amt) -> L.payPrin d amt l) bndsAmountToBePaid --  `debug` ("pay prin->>>To"++show(bnds))

    bndMapUpdated =  Map.union (Map.fromList $ zip bnds bndsPaid) bndMap
    accMapAfterPay = Map.adjust (A.draw actualPaidOut d (PayPrin bnds)) an accMap

performAction d t@TestDeal{accounts=accMap} (W.LiquidatePool lm an) =
  t {accounts = accMapAfterLiq } -- TODO need to remove assets
  where
    liqAmt = calcLiquidationAmount lm (pool t) d
    accMapAfterLiq = Map.adjust (A.deposit liqAmt d (LiquidationProceeds)) an accMap

performAction d t@TestDeal{fees=feeMap} (W.CalcFee fns) 
  = t {fees = Map.union newFeeMap feeMap }
  where 
    newFeeMap = Map.map
                  (calcDueFee t d) $
                  getFeeByName t (Just fns)

performAction d t@TestDeal{bonds=bndMap} (W.CalcBondInt bns) 
  = t {bonds = Map.union newBondMap bndMap}
  where 
    newBondMap = Map.map 
                  (calcDueInt t d) $
                  getBondByName t (Just bns)

performAction d t@TestDeal{accounts=accs, liqProvider = Just _liqProvider} (W.LiqSupport limit pName CE.LiqToAcc an)
  = t { accounts = newAccMap, liqProvider = Just newLiqMap } -- `debug` ("Using LImit"++ show limit)
  where 
      newLiqMapUpdated = Map.adjust (updateLiqProvider t d) pName _liqProvider 
      _transferAmt = case limit of 
                      Nothing -> 0 -- `debug` ("limit on nothing"++ show limit)
                      Just (DS (ReserveAccGap [an])) -> queryDeal t (ReserveAccGapAt d [an]) -- `debug` ("Query Gap"++ show (queryDeal t (ReserveAccGapAt d [an])))
                      Just (DS ds) -> queryDeal t (patchDateToStats d ds) -- `debug` ("hit with ds"++ show ds)
                      _ -> error "Failed on formula passed" -- `debug` ("limit on last"++ show limit)
      transferAmt = case CE.liqCredit $ newLiqMapUpdated Map.! pName of 
                       Nothing -> _transferAmt
                       Just _availBal -> min _transferAmt _availBal  -- `debug` ("transfer amt"++ show _transferAmt )
      newAccMap = Map.adjust (A.deposit transferAmt d (LiquidationSupport pName)) an accs
      newLiqMap = Map.adjust (CE.draw transferAmt d ) pName newLiqMapUpdated

performAction d t@TestDeal{fees=feeMap,liqProvider = Just _liqProvider} (W.LiqSupport limit pName CE.LiqToFee fn)
  = t { fees = newFeeMap, liqProvider = Just newLiqMap }
  where 
      _transferAmt = case limit of 
                      Nothing -> 0 
                      Just (DS (CurrentDueFee [fn]))
                        -> queryDeal t (CurrentDueFee [fn])
                      _ -> 0
      transferAmt = case CE.liqCredit $  _liqProvider Map.! pName of 
                       Nothing -> _transferAmt
                       Just _availBal -> min _transferAmt _availBal
     -- transferAmt = min _transferAmt $ CE.liqBalance $  _liqProvider Map.! pName
      newFeeMap = Map.adjust (F.payFee d transferAmt) fn feeMap
      newLiqMap = Map.adjust (CE.draw transferAmt d ) pName _liqProvider 

performAction d t@TestDeal{bonds=bndMap,liqProvider = Just _liqProvider} (W.LiqSupport limit pName CE.LiqToBondInt bn)
  = t { bonds = newBondMap, liqProvider = Just newLiqMap }
  where 
      _transferAmt = case limit of 
                      Nothing -> 0 
                      Just (DS (CurrentDueBondInt [bn]))
                        -> queryDeal t (CurrentDueBondInt [bn])
                      _ -> 0
      transferAmt = case CE.liqCredit $  _liqProvider Map.! pName of 
                       Nothing -> _transferAmt
                       Just _availBal -> min _transferAmt _availBal
      --transferAmt = min _transferAmt $ CE.liqBalance $  _liqProvider Map.! pName
      newBondMap = Map.adjust (L.payInt d transferAmt ) bn bndMap
      newLiqMap = Map.adjust (CE.draw transferAmt d ) pName _liqProvider 

performAction d t@TestDeal{accounts=accs,liqProvider = Just _liqProvider} (W.LiqRepay limit rpt an pName)
  = t { accounts = newAccMap, liqProvider = Just newLiqMap }
  where 
      liqDue = CE.liqBalance $ _liqProvider Map.! pName
      transferAmt = case limit of 
                      Nothing -> min liqDue $ A.accBalance $ accs Map.! an
                      _ -> 0 -- to be implement
      newAccMap = Map.adjust (A.draw transferAmt d (LiquidationSupport pName)) an accs
      newLiqMap = Map.adjust (CE.repay transferAmt d rpt ) pName _liqProvider 

performAction d t@TestDeal{accounts=accs,liqProvider = Just _liqProvider} (W.LiqYield limit an pName)
  = t { accounts = newAccMap, liqProvider = Just newLiqMap }
  where 
      transferAmt = case limit of 
                      Nothing -> A.accBalance $ accs Map.! an
                      _ -> 0 -- to be implement
      newAccMap = Map.adjust (A.draw transferAmt d (LiquidationSupport pName)) an accs
      newLiqMap = Map.adjust (CE.repay transferAmt d CE.LiqBal ) pName _liqProvider 

performAction d t@TestDeal{liqProvider = Just _liqProvider} (W.LiqAccrue n)
  = t {liqProvider = Just updatedLiqProvider}
    where 
      updatedLiqProvider = Map.adjust (CE.accrueLiqProvider d ) n _liqProvider

performAction d t@TestDeal{rateSwap = Just rtSwap } (W.SwapAccrue sName)
  = t { rateSwap = Just newRtSwap } 
    where 
        refBal = case  (HE.rsNotional (rtSwap Map.! sName)) of 
                   (HE.Fixed b) -> b
                   (HE.Base ds) -> queryDeal t (patchDateToStats d ds)
                   (HE.Schedule ts) -> fromRational $ getValByDate ts Inc d
                   
        newRtSwap = Map.adjust 
                      (HE.accrueIRS d)
                      sName
                      (Map.adjust (HE.updateRefBalance refBal) sName rtSwap)

performAction d t@TestDeal{rateSwap = Just rtSwap, accounts = accsMap } (W.SwapReceive accName sName)
  = t { rateSwap = Just newRtSwap, accounts = newAccMap }
    where 
        receiveAmt = max 0 $ HE.rsNetCash $ rtSwap Map.! sName
        newRtSwap = Map.adjust (HE.receiveIRS d) sName rtSwap -- `debug` ("REceiv AMT"++ show receiveAmt)
        newAccMap = Map.adjust (A.deposit receiveAmt d SwapInSettle) accName accsMap

performAction d t@TestDeal{rateSwap = Just rtSwap, accounts = accsMap } (W.SwapPay accName sName)
  = t { rateSwap = Just newRtSwap, accounts = newAccMap }
    where 
        payoutAmt = negate $ HE.rsNetCash $ rtSwap Map.! sName
        availBal = A.accBalance $ accsMap Map.! accName
        amtToPay = min payoutAmt availBal
        newRtSwap = Map.adjust (HE.payoutIRS d amtToPay) sName rtSwap
        newAccMap = Map.adjust (A.draw amtToPay d SwapOutSettle) accName accsMap

performAction d t@TestDeal{rateSwap = Just rtSwap, accounts = accsMap } (W.SwapSettle accName sName)
  = let 
      t1 = performAction d t (W.SwapAccrue sName)
      t2 = performAction d t1 (W.SwapReceive accName sName)
    in 
      performAction d t2 (W.SwapPay accName sName)


performAction d t@TestDeal{ triggers = Just trgM } (W.RunTrigger loc idx)
  = t { triggers = Just (Map.insert loc newTrgLst trgM) }
    where 
      trg = (trgM Map.! loc) !! idx
      newTrg = updateTrigger t d trg
      newTrgLst = replace (trgM Map.! loc) idx newTrg

performAction d t action =  error $ "failed to match action"++show action