{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Deal.DealAction (performActionWrap,performAction,calcDueFee
                       ,testTrigger,RunContext(..),updateLiqProvider
                       ,calcDueInt,priceAssetUnion
                       ,priceAssetUnionList,inspectVars) 
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
import AssetClass.MixedAsset

import qualified Call as C
import qualified InterestRate as IR
import qualified Analytics as AN

import Deal.DealBase
import Deal.DealQuery
import Deal.DealDate

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
import Cashflow (CashFlowFrame(CashFlowFrame))
import Control.Lens hiding (element)
import Control.Lens.TH

debug = flip trace


getPoolFlows :: TestDeal a -> Maybe Date -> Maybe Date -> RangeType -> [CF.TsRow]
getPoolFlows t@TestDeal{ pool = _pool } sd ed rt =
  case (sd,ed) of
    (Nothing,Nothing)   ->  _trs
    (Nothing,Just _ed)  ->  cutBy Inc Past _ed _trs
    (Just _sd,Nothing)  ->  cutBy Inc Future _sd _trs 
    (Just _sd,Just _ed) ->  sliceBy rt _sd _ed _trs
  where
    (CF.CashFlowFrame _trs) = fromMaybe (CF.CashFlowFrame []) (P.futureCf _pool)

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
calcLiquidationAmount (BalanceFactor currentFactor defaultFactor ) pool d 
  = case P.futureCf pool of 
      Nothing -> 0  -- `debug` ("No futureCF")
      Just _futureCf@(CashFlowFrame trs) ->
        let 
          earlierTxns = cutBy Inc Past d trs
          currentCumulativeDefaultBal = sum $ map (\x -> (CF.mflowDefault x) - (CF.mflowRecovery x) - (CF.mflowLoss x)) earlierTxns
        in 
          case earlierTxns of 
            [] -> 0  -- `debug` ("No pool Inflow")
            _ -> (mulBR (CF.mflowBalance (last earlierTxns)) currentFactor) + (mulBR currentCumulativeDefaultBal defaultFactor) 
            -- TODO need to check if missing last row

calcLiquidationAmount (PV discountRate recoveryPct) pool d 
  = case P.futureCf pool of
      Nothing -> 0 
      Just (CashFlowFrame trs) ->
          let 
            futureTxns = cutBy Inc Future d trs
            earlierTxns = cutBy Exc Past d trs 
            pvCf = sum $ map (\x -> AN.pv2  discountRate  d (CF.getDate x) (CF.tsTotalCash x)) futureTxns 
            currentDefaulBal = sum $ map (\x -> (CF.mflowDefault x) - (CF.mflowRecovery x) - (CF.mflowLoss x)) earlierTxns
          in 
            pvCf + mulBI currentDefaulBal recoveryPct

liquidatePool :: PricingMethod -> Date -> String -> TestDeal a -> TestDeal a
liquidatePool lq d accName t =
  t {accounts = Map.adjust updateFn accName accs} -- `debug` ("Accs->"++show(accs))
  where
     proceeds = calcLiquidationAmount lq (pool t) d
     updateFn = A.deposit proceeds d LiquidationProceeds
     accs = accounts t


calcDueFee :: P.Asset a => TestDeal a -> Date -> F.Fee -> F.Fee
calcDueFee t calcDay f@(F.Fee fn (F.FixFee amt) fs fd fdDay fa _ _)
  | isJust fdDay = f  
  | calcDay >= fs && isNothing fdDay = f{ F.feeDue = amt, F.feeDueDate = Just calcDay} -- `debug` ("DEBUG--> init with amt "++show(fd)++show amt)
  | otherwise = f

calcDueFee t calcDay f@(F.Fee fn (F.AnnualRateFee feeBase r) fs fd Nothing fa lpd _)
  | calcDay >= fs = calcDueFee t calcDay f {F.feeDueDate = Just fs }
  | otherwise = f 

calcDueFee t calcDay f@(F.Fee fn (F.AnnualRateFee feeBase _r) fs fd (Just _fdDay) fa lpd _)
  = f{ F.feeDue=fd+newDue, F.feeDueDate = Just calcDay }  -- `debug` ("Fee DUE new Due "++show calcDay ++show baseBal ++show(newDue))                   
      where 
        accrueStart = _fdDay
        baseBal = case feeBase of
                    CurrentPoolBalance ->  CF.mflowWeightAverageBalance accrueStart calcDay $ getPoolFlows t Nothing Nothing II
                    OriginalPoolBalance -> mulBR (P.getIssuanceField (pool t) IssuanceBalance) (yearCountFraction DC_ACT_365F accrueStart calcDay)
                    OriginalBondBalance -> mulBR (queryDeal t OriginalBondBalance) (yearCountFraction DC_ACT_365F accrueStart calcDay)
                    CurrentBondBalance -> Map.foldr (\v a-> a + L.weightAverageBalance accrueStart calcDay v ) 0.0 (bonds t)
                    CurrentBondBalanceOf bns -> Map.foldr (\v a-> a + L.weightAverageBalance accrueStart calcDay v ) 0.0 (getBondByName t (Just bns))
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
      dueAmt = max 0 $ queryDeal t dsDueD - queryDeal t dsPaidD

calcDueFee t@TestDeal{ pool = pool } calcDay f@(F.Fee fn (F.ByCollectPeriod amt) fs fd fdday fa lpd _)
  = f {F.feeDue = dueAmt + fd, F.feeDueDate = Just calcDay}
    where 
      txnsDates = getDate <$> maybe [] CF.getTsCashFlowFrame (view P.poolFutureCf pool)
      pastPeriods = case fdday of 
                      Nothing ->  subDates II fs calcDay txnsDates
                      Just lastFeeDueDay -> subDates EI lastFeeDueDay calcDay txnsDates
      dueAmt = fromRational $ mulBInt amt (length pastPeriods)

calcDueFee t calcDay f@(F.Fee fn (F.AmtByTbl _ ds tbl) fs fd fdday fa lpd _)
  = f {F.feeDue = dueAmt + fd, F.feeDueDate = Just calcDay}
    where 
      lookupVal = queryDeal t (patchDateToStats calcDay ds)
      dueAmt = fromMaybe 0.0 $ lookupTable tbl Down (lookupVal >=)

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
calcDueInt t calc_date b@(L.Bond _ _ oi io _ _ r dp di Nothing _ lastPrinPay _ ) 
 | calc_date <= closingDate = b
 | otherwise = calcDueInt t calc_date (b {L.bndDueIntDate = Just closingDate })
   where 
     closingDate = getClosingDate (dates t)

calcDueInt t calc_date b@(L.Bond bn L.Z bo bi _ bond_bal bond_rate _ _ _ lstIntPay _ _) 
  = b {L.bndDueInt = 0 }

calcDueInt t calc_date b@(L.Bond bn L.Equity bo (L.InterestByYield y) _ bond_bal _ _ int_due _ lstIntPay _ mStmt)
  = b {L.bndDueInt = newDue }  -- `debug` ("Yield Due Int >>"++ show bn++">> new due"++ show newDue++">> old due"++ show int_due )
  where
    newDue = L.backoutDueIntByYield calc_date b

calcDueInt t calc_date b@(L.Bond bn bt bo bi _ bond_bal bond_rate _ int_due (Just int_due_date) lstIntPay _ _ ) 
  | calc_date == int_due_date = b
  | otherwise = b {L.bndDueInt = new_due_int+int_due,L.bndDueIntDate = Just calc_date }  --  `debug` ("Due INT"++show calc_date ++">>"++show(bn)++">>"++show int_due++">>"++show(new_due_int))
              where
                dc = case bi of 
                       L.Floater _ _ _ _ _dc _ _ -> _dc 
                       L.Fix _ _dc -> _dc 
                       _ -> DC_ACT_365F
                new_due_int = IR.calcInt (bond_bal+int_due) int_due_date calc_date bond_rate dc  -- `debug` ("Bond bal"++show bond_bal++">>"++show int_due_date++">>"++ show calc_date++">>"++show bond_rate)


calcDuePrin :: P.Asset a => TestDeal a -> T.Day -> L.Bond -> L.Bond
calcDuePrin t calc_date b@(L.Bond bn L.Sequential bo bi _ bond_bal _ prin_arr int_arrears _ _ _ _) =
  b {L.bndDuePrin = duePrin} 
  where
    duePrin = bond_bal 

calcDuePrin t calc_date b@(L.Bond bn (L.Lockout cd) bo bi _ bond_bal _ prin_arr int_arrears _ _ _ _) =
  if cd > calc_date then 
    b {L.bndDuePrin = 0}
  else
    b {L.bndDuePrin = duePrin}
  where
    duePrin = bond_bal 

calcDuePrin t calc_date b@(L.Bond bn (L.PAC schedule) bo bi _ bond_bal _ prin_arr int_arrears _ _ _ _) =
  b {L.bndDuePrin = duePrin} -- `debug` ("bn >> "++bn++"Due Prin set=>"++show(duePrin) )
  where
    scheduleDue = getValOnByDate schedule calc_date  
    duePrin = max (bond_bal - scheduleDue) 0 -- `debug` ("In PAC ,target balance"++show(schedule)++show(calc_date)++show(scheduleDue))

calcDuePrin t calc_date b@(L.Bond bn (L.PacAnchor schedule bns) bo bi _ bond_bal _ prin_arr int_arrears _ _ _ _) =
  b {L.bndDuePrin = duePrin} -- `debug` ("bn >> "++bn++"Due Prin set=>"++show(duePrin) )
  where
    scheduleDue = getValOnByDate schedule calc_date
    anchor_bond_balance = queryDeal t (CurrentBondBalanceOf bns)
    duePrin = if anchor_bond_balance > 0 then
                 max (bond_bal - scheduleDue) 0
              else
                 bond_bal

calcDuePrin t calc_date b@(L.Bond bn L.Z bo bi _ bond_bal bond_rate prin_arr int_arrears _ lstIntPay _ _) =
  if all isZbond activeBnds then
      b {L.bndDuePrin = bond_bal} -- `debug` ("bn >> "++bn++"Due Prin set=>"++show(duePrin) )
  else 
      b {L.bndDuePrin = 0, L.bndBalance = new_bal, L.bndLastIntPay=Just calc_date} -- `debug` ("bn >> "++bn++"Due Prin set=>"++show(duePrin) )
  where
    isZbond (L.Bond _ L.Z _ _ _ _ _ _ _ _ _ _ _) = True
    isZbond L.Bond {} = False
    
    activeBnds = filter (\x -> L.bndBalance x > 0) (Map.elems (bonds t))
    new_bal = bond_bal + dueInt
    lastIntPayDay = case lstIntPay of
                      Just pd -> pd
                      Nothing -> getClosingDate (dates t)
    dueInt = IR.calcInt bond_bal lastIntPayDay calc_date bond_rate DC_ACT_365F

calcDuePrin t calc_date b@(L.Bond bn L.Equity bo bi _ bond_bal _ prin_arr int_arrears _ _ _ _) =
  b {L.bndDuePrin = bond_bal }


priceAssetUnion :: ACM.AssetUnion -> Date -> PricingMethod  -> AP.AssetPerf -> Maybe [RateAssumption] -> PriceResult
priceAssetUnion (ACM.MO m) d pm aps = P.priceAsset m d pm aps 
priceAssetUnion (ACM.LO m) d pm aps = P.priceAsset m d pm aps
priceAssetUnion (ACM.IL m) d pm aps = P.priceAsset m d pm aps
priceAssetUnion (ACM.LS m) d pm aps = P.priceAsset m d pm aps

priceAssetUnionList :: [ACM.AssetUnion] -> Date -> PricingMethod  -> AP.ApplyAssumptionType -> Maybe [RateAssumption] -> [PriceResult]
priceAssetUnionList assetList d pm (AP.PoolLevel assetPerf) mRates 
  = [ priceAssetUnion asset d pm assetPerf mRates | asset <- assetList ]


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


data RunContext a = RunContext{
                  runPoolFlow:: CF.CashFlowFrame
                  ,revolvingAssump:: Maybe (RevolvingPool ,AP.ApplyAssumptionType)
                  ,revolvingInterestRateAssump:: Maybe [RateAssumption]
                  }

updateOriginDate2 :: Date -> ACM.AssetUnion -> ACM.AssetUnion
updateOriginDate2 d (ACM.LO m) = ACM.LO $ updateOriginDate m (P.calcAlignDate m d)
updateOriginDate2 d (ACM.MO m) = ACM.MO $ updateOriginDate m (P.calcAlignDate m d)
updateOriginDate2 d (ACM.IL m) = ACM.IL $ updateOriginDate m (P.calcAlignDate m d)
updateOriginDate2 d (ACM.LS m) = ACM.LS $ updateOriginDate m (P.calcAlignDate m d)

-- ^ get available supports in balance
evalExtraSupportBalance :: P.Asset a => Date -> TestDeal a -> W.ExtraSupport  -> [Balance]
evalExtraSupportBalance d t (W.WithCondition pre s) 
  | testPre d t pre = evalExtraSupportBalance d t s
  | otherwise = [0]
evalExtraSupportBalance d t@TestDeal{accounts=accMap} (W.SupportAccount an _) = [A.accBalance $ accMap Map.! an]
evalExtraSupportBalance d t@TestDeal{liqProvider=Just liqMap} (W.SupportLiqFacility liqName) = [ fromMaybe 0 (CE.liqCredit (liqMap Map.! liqName))]
evalExtraSupportBalance d t (W.MultiSupport supports) = concat $ evalExtraSupportBalance d t <$> supports


-- ^ draw support from a deal , return updated deal,and remaining oustanding amount
drawExtraSupport :: Date -> Amount -> W.ExtraSupport -> TestDeal a -> (TestDeal a,Amount)
drawExtraSupport d amt (W.SupportAccount an (Just (W.ByAccountDraw ln))) t@TestDeal{accounts=accMap, ledgers= Just ledgerMap}
  = let 
      drawAmt = min (A.accBalance (accMap Map.! an)) amt
      oustandingAmt = amt - drawAmt
    in 
      (t {accounts = Map.adjust (A.draw drawAmt d Types.SupportDraw) an accMap
         ,ledgers = Just $ Map.adjust (LD.entryLog drawAmt d (TxnDirection Debit)) ln ledgerMap}
      , oustandingAmt)

drawExtraSupport d amt (W.SupportAccount an Nothing) t@TestDeal{accounts=accMap} 
  = let 
      drawAmt = min (A.accBalance (accMap Map.! an)) amt
      oustandingAmt = amt - drawAmt
    in 
      (t {accounts = Map.adjust (A.draw drawAmt d Types.SupportDraw) an accMap }
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

inspectVars :: P.Asset a => TestDeal a -> Date -> DealStats -> ResultComponent
inspectVars t d ds =                     
  case getDealStatType ds of 
    RtnRate -> InspectRate d ds $ queryDealRate t (patchDateToStats d ds)
    RtnBool -> InspectBool d ds $ queryDealBool t (patchDateToStats d ds)
    RtnInt  -> InspectInt d ds $ queryDealInt t (patchDateToStats d ds) d
    _       -> InspectBal d ds $ queryDeal t (patchDateToStats d ds)

showInspection :: ResultComponent -> String
showInspection (InspectRate d ds r) = show r
showInspection (InspectBool d ds r) = show r
showInspection (InspectInt d ds r) = show r
showInspection (InspectBal d ds r) = show r
showInspection x = error $ "not implemented for showing ResultComponent " ++ show x

performActionWrap :: P.Asset a => Date -> (TestDeal a, RunContext a, [ResultComponent]) -> W.Action -> (TestDeal a, RunContext a, [ResultComponent])
performActionWrap d 
                  (t@TestDeal{ accounts = accsMap }
                  ,rc@RunContext{runPoolFlow=pcf@(CF.CashFlowFrame (tr:trs))
                                ,revolvingAssump=Just (assetForSale,perfAssumps)
                                ,revolvingInterestRateAssump = mRates}
                  ,logs)
                  (W.BuyAsset ml pricingMethod accName) 
   = (t { accounts = newAccMap }, newRc, logs )
    where 
      _assets = lookupAssetAvailable assetForSale d
      assets = updateOriginDate2 d <$> _assets -- `debug` ("Asset on revolv"++ show _assets)
                
      valuationOnAvailableAssets = sum $ getPriceValue <$> priceAssetUnionList assets d pricingMethod perfAssumps mRates 
      accBal = A.accBalance $ accsMap Map.! accName -- `debug` ("Av")
      limitAmt = case ml of 
                   Just (DS ds) -> queryDeal t (patchDateToStats d ds)
                   Just (DueCapAmt amt) -> amt
                   Nothing -> accBal

      availBal = min limitAmt accBal -- `debug` ("Value on r -asset "++ show valuationOnAvailableAssets)
      purchaseAmt = case assetForSale of 
                      (StaticAsset _) -> min availBal valuationOnAvailableAssets -- `debug` ("Valuation on rpool"++show valuationOnAvailableAssets)
                      ConstantAsset _ -> availBal 
                      AssetCurve _ -> min availBal valuationOnAvailableAssets   

      purchaseRatio = divideBB purchaseAmt valuationOnAvailableAssets -- `debug` ("Purchase Amt"++show purchaseAmt)
      purchaseRatios = toRational <$> [purchaseRatio,1-purchaseRatio]

      (assetBought,poolAfterBought) = buyRevolvingPool d purchaseRatios assetForSale -- `debug` ("purchase ratio"++ show purchaseRatios)
      newAccMap = Map.adjust (A.draw purchaseAmt d PurchaseAsset) accName accsMap
      
      -- newBoughtPcf = (CF.shiftCfToStartDate d) <$> [ projAssetUnion ast d perfAssumps | ast <- assetBought ]
      (CashFlowFrame newBoughtTxn) = fst $ projAssetUnionList [updateOriginDate2 d ast | ast <- assetBought ] d perfAssumps mRates -- `debug` ("Asset bought"++ show assetBought)
      -- poolCurrentTr = CF.buildBegTsRow d tr
      -- currentPoolFlow = CF.cfInsertHead poolCurrentTr pcf -- `debug` ("Inserting new tr"++ show poolCurrentTr)
      -- currentPoolFlow = CF.patchBeginBalance d pcf
      --newPcf = foldl CF.mergePoolCf pcf newBoughtPcf  `debug` ("reolvoing cf"++show d++"\n"++show newBoughtPcf++"\n"++"pool cf 1st"++show (CF.cfAt pcf 0))
      -- newPcf = CF.CashFlowFrame $ CF.mergePoolCf currentPoolFlow newBoughtPcf --  `debug` ("reolvoing after insert"++ show currentPoolFlow)
      newPcf = CF.CashFlowFrame $ CF.combineTss [] (tr:trs) newBoughtTxn  -- `debug` ("reolvoing first txn\n"++ show (head newBoughtTxn))
      newRc = rc {runPoolFlow = newPcf
                 ,revolvingAssump = Just (poolAfterBought, perfAssumps)}  -- `debug` ("new pool flow\n"++show newPcf++"\n")

performActionWrap d 
                  (t
                  ,rc@RunContext{runPoolFlow=pcf@(CF.CashFlowFrame (tr:trs))
                                ,revolvingAssump=Nothing
                                ,revolvingInterestRateAssump = mRates}
                  ,logs)
                  (W.BuyAsset ml pricingMethod accName)
  = error $ "Missing revolving Assumption(asset assumption & asset to buy)" ++ show (name t)

performActionWrap d (t, rc, logs) (W.WatchVal ms dss)
  = (t, rc, logs ++ [newLog])
    where 
      newLog =  InspectWaterfall d ms dss $ [ showInspection (inspectVars t d ds) | ds <- dss ] 

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
    accMapAfterDraw = Map.adjust (A.draw transferAmt d comment) an1 accMap
    accMapAfterDeposit = Map.adjust (A.deposit transferAmt d comment) an2 accMapAfterDraw

performAction d t@TestDeal{ledgers= Just ledgerM} (W.BookBy (W.ByDS ledger dr ds)) =
  let  
    amtToBook = queryDeal t ds
    newLedgerM = Map.adjust (LD.entryLog amtToBook d (TxnDirection dr)) ledger ledgerM
  in 
    t {ledgers = Just newLedgerM} 

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
  t {accounts = accMapAfterDeposit, ledgers = Just newLedgerM}  
  where
    sourceAcc = accMap Map.! an1
    targetAcc = accMap Map.! an2 
    targetAmt = queryDeal t (LedgerBalance [ln]) 
    transferAmt = min (A.accBalance sourceAcc) targetAmt 
 
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

performAction d t@TestDeal{fees=feeMap, accounts=accMap} (W.PayFeeBySeq mLimit an fns mSupport) =
  let 
    availAccBal = A.accBalance (accMap Map.! an)
    supportAvail = case mSupport of 
                     Just support -> sum ( evalExtraSupportBalance d t support)
                     Nothing -> 0
    amtAvailable = case mLimit of
                     Nothing -> availAccBal + supportAvail
                     Just (DS ds) -> min (availAccBal + supportAvail) $ queryDeal t (patchDateToStats d ds)
                     Just (DueCapAmt amt) -> min amt $ availAccBal + supportAvail
    feesToPay = map (feeMap Map.!) fns
    feeDueAmts = map F.feeDue feesToPay  
    actualPaidOut = min amtAvailable $ sum feeDueAmts

    feesAmountToBePaid = zip feesToPay $ paySeqLiabilitiesAmt actualPaidOut feeDueAmts
    
    feesPaid = map (\(f,amt) -> F.payFee d amt f) feesAmountToBePaid
    -- update primary account map
    accPaidOut = min actualPaidOut availAccBal
    dealAfterAcc = t {accounts = Map.adjust (A.draw accPaidOut d (SeqPayFee fns)) an accMap
                     ,fees = Map.fromList (zip fns feesPaid) <> feeMap}

    supportPaidOut = actualPaidOut - accPaidOut
  in 
    case mSupport of 
      Just support -> fst $ drawExtraSupport d supportPaidOut support dealAfterAcc
      Nothing -> dealAfterAcc

performAction d t@TestDeal{fees=feeMap, accounts=accMap} (W.PayFee mLimit an fns mSupport) =
  case mSupport of 
    Just support -> fst $ drawExtraSupport d supportPaidOut support dealAfterAcc
    Nothing -> dealAfterAcc
  where
    availAccBal = A.accBalance (accMap Map.! an)
    supportAvail = case mSupport of 
                     Just support -> sum ( evalExtraSupportBalance d t support)
                     Nothing -> 0
    amtAvailable = case mLimit of
                     Nothing -> availAccBal + supportAvail
                     Just (DS ds) -> min (availAccBal + supportAvail) $ queryDeal t (patchDateToStats d ds)
                     Just (DueCapAmt amt) -> min amt $ availAccBal + supportAvail

    feesToPay = map (feeMap Map.!) fns
    feeDueAmts = map F.feeDue feesToPay  
                   -- Just (DuePct pct) -> map (\x -> mulBR (F.feeDue x) pct ) feesToPay
                   -- Just (DueCapAmt amt) -> prorataFactors (F.feeDue <$> feesToPay) amt
    -- total actual pay out
    actualPaidOut = min amtAvailable $ sum feeDueAmts -- `debug` ("Fee Due Amounts"++show(feeDueAmts))

    feesAmountToBePaid = zip feesToPay $ prorataFactors feeDueAmts actualPaidOut
    feesPaid = map (\(f,amt) -> F.payFee d amt f) feesAmountToBePaid
    -- update primary account map
    accPaidOut = min actualPaidOut availAccBal
    dealAfterAcc = t {accounts = Map.adjust (A.draw accPaidOut d (SeqPayFee fns)) an accMap
                     ,fees = Map.fromList (zip fns feesPaid) <> feeMap}

    supportPaidOut = sum feeDueAmts - accPaidOut

performAction d t (W.AccrueAndPayIntBySeq mLimit an bnds mSupport)
  = let 
      dealWithBondDue = performAction d t (W.CalcBondInt bnds)
    in 
      performAction d dealWithBondDue (W.PayIntBySeq mLimit an bnds mSupport)

performAction d t@TestDeal{bonds=bndMap, accounts=accMap, liqProvider=liqMap} (W.PayIntBySeq mLimit an bnds mSupport)
  = let 
      accBal = A.accBalance $ accMap Map.! an
      supportAvail = case mSupport of 
                       Just support -> sum ( evalExtraSupportBalance d t support)
                       Nothing -> 0
      amtAvailable = case mLimit of
                       Nothing -> accBal + supportAvail
                       Just (DS ds) -> min (accBal + supportAvail) $ queryDeal t (patchDateToStats d ds)
                       Just (DueCapAmt amt) -> min amt (accBal + supportAvail)
                       _ -> error $ "Not support for limit when pay int by seq" ++ show mLimit
      bndsList = (Map.!) bndMap <$> bnds
      dueAmts = L.bndDueInt <$> bndsList
      actualPaids = paySeqLiabilitiesAmt amtAvailable dueAmts
      -- update bond paid
      bondsPaid = uncurry (L.payInt d) <$> zip actualPaids bndsList
      -- update account 
      accPay = min (sum actualPaids) accBal
      -- update liq Provider
      supportPay = sum actualPaids - accPay
      dAfterSupport = case mSupport of 
                        Nothing -> t
                        Just s -> fst $ drawExtraSupport d supportPay s t
    in
      dAfterSupport { bonds = Map.fromList (zip bnds bondsPaid) <> bndMap
                     , accounts = Map.adjust (A.draw accPay d (PayInt bnds)) an accMap }


performAction d t@TestDeal{bonds=bndMap,accounts=accMap} (W.PayInt mLimit an bnds mSupport) =
  case mSupport of 
    Just support -> fst $ drawExtraSupport d supportPaidOut support dealAfterAcc
    Nothing -> dealAfterAcc
  where
    accBal = A.accBalance $ accMap Map.! an
    supportAvail = case mSupport of 
                     Just support -> sum ( evalExtraSupportBalance d t support)
                     Nothing -> 0
    
    availBal = case mLimit of 
                 Nothing -> accBal + supportAvail
                 Just (DS ds) -> min (accBal + supportAvail) $ queryDeal t (patchDateToStats d ds)
                 Just (DueCapAmt amt) -> min (accBal + supportAvail) amt
                 _ -> error ("Not support limit type for pay int" <> show mLimit)

    bndsToPay = map (bndMap Map.!) bnds
    bndsDueAmts = map L.bndDueInt bndsToPay
    bndsNames = map L.bndName bndsToPay

    actualPaidOut = min availBal $ sum bndsDueAmts -- `debug` ("due mats"++ show bndsDueAmts ++">>"++ show availBal)
    bndsAmountToBePaid = zip bndsToPay $ prorataFactors bndsDueAmts actualPaidOut

    -- bond map updated
    bndsPaid = map (\(l,amt) -> L.payInt d amt l) bndsAmountToBePaid
    -- primary account paid out
    accPaidOut = min actualPaidOut accBal
    dealAfterAcc = t {accounts = Map.adjust (A.draw actualPaidOut d (PayInt bnds)) an accMap
                     ,bonds = Map.fromList (zip bndsNames bndsPaid) <> bndMap }
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

performAction d t@TestDeal{bonds=bndMap,accounts=accMap} (W.PayIntResidual mLimit an bndName) =
  t {accounts = Map.adjust (A.draw limitAmt d (PayYield bndName)) an accMap
    , bonds = Map.adjust (L.payYield d limitAmt) bndName bndMap}
  where
    availBal = A.accBalance $ accMap Map.! an
    limitAmt = case mLimit of 
                 Nothing -> availBal
                 Just (DS ds) -> min availBal $ queryDeal t (patchDateToStats d ds)
                 Just (DueCapAmt amt) -> min availBal amt

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


performAction d t@TestDeal{bonds=bndMap,accounts=accMap} (W.PayPrinBySeq mLimit an bnds mSupport)= 
  dAfterSupport { bonds = Map.fromList (zip bndsToPayNames bondsPaid) <> bndMap
               , accounts = Map.adjust (A.draw accPay d (PayPrin bnds)) an accMap }
  where 
    accBal = A.accBalance $ accMap Map.! an
    supportAvail = case mSupport of 
                     Just support -> sum ( evalExtraSupportBalance d t support)
                     Nothing -> 0
    amtAvailable = case mLimit of
                     Nothing -> accBal + supportAvail
                     Just (DS ds) -> min (accBal + supportAvail) $ queryDeal t (patchDateToStats d ds)
                     Just (DueCapAmt amt) -> min amt (accBal + supportAvail)
                     _ -> error $ "Not support for limit when pay prin by seq" ++ show mLimit
    bndsList = (Map.!) bndMap <$> bnds
 
    bndsToPay = filter (not . L.isPaidOff) bndsList
    bndsToPayNames = L.bndName <$> bndsToPay
    bndsWithDue = calcDuePrin t d <$> bndsToPay
    bndsDueAmts = L.bndDuePrin <$> bndsWithDue
                 
    payAmount = min (sum bndsDueAmts) amtAvailable

    actualPaids = paySeqLiabilitiesAmt payAmount bndsDueAmts
    -- update bond paid
    bondsPaid = uncurry (L.payPrin d) <$> zip actualPaids bndsToPay
    -- update account 
    accPay = min (sum actualPaids) accBal
    -- update liq Provider
    supportPay = sum actualPaids - accPay
    dAfterSupport = case mSupport of 
                      Nothing -> t
                      Just s -> fst $ drawExtraSupport d supportPay s t


performAction d t@TestDeal{bonds=bndMap,accounts=accMap} (W.PayPrin (Just (DS ds)) an bnds Nothing) = 
  t {accounts = accMapAfterPay, bonds = bndsUpdated}
  where
    availBal = A.accBalance $ accMap Map.! an
    
    bndsToPay = filter (not . L.isPaidOff) $ map (bndMap Map.!) bnds
    bndsToPayNames = L.bndName <$> bndsToPay
    bndsDueAmts = L.bndDuePrin . calcDuePrin t d <$> bndsToPay
    payAmount = min (sum bndsDueAmts) $ min availBal $ queryDeal t $ patchDateToStats d ds -- `debug` ("Query with "++show (patchedDs))

    bndsAmountToBePaid = zip bndsToPay $ prorataFactors bndsDueAmts payAmount  -- (bond, amt-allocated)

    bndsPaid = map (\(b,amt) -> L.payPrin d amt b) bndsAmountToBePaid
    bndsUpdated = Map.union (Map.fromList $ zip bndsToPayNames bndsPaid) bndMap

    accMapAfterPay = Map.adjust
                        (A.draw payAmount d (TxnComments [PayPrin bnds,UsingDS ds])) 
                        an
                        accMap

performAction d t@TestDeal{bonds=bndMap,accounts=accMap} (W.PayPrin Nothing an bnds Nothing) =
  t {accounts = accMapAfterPay, bonds = bndMapUpdated} 
  where
    acc = accMap Map.! an
    bndsToPay = filter (not . L.isPaidOff) $ map (bndMap Map.!) bnds
    bndsToPayNames = L.bndName <$> bndsToPay
    availBal = A.accBalance acc
    bndsWithDue = map (calcDuePrin t d) bndsToPay  --
    bndsDueAmts = map L.bndDuePrin bndsWithDue

    actualPaidOut = min availBal $ sum bndsDueAmts -- `debug` ("bonds totoal due ->"++show(bndsDueAmts))
    bndsAmountToBePaid = zip bndsWithDue (prorataFactors bndsDueAmts availBal)
    bndsPaid = map (\(l,amt) -> L.payPrin d amt l) bndsAmountToBePaid --  `debug` ("pay prin->>>To"++show(bnds))

    bndMapUpdated =  Map.union (Map.fromList $ zip bndsToPayNames bndsPaid) bndMap
    accMapAfterPay = Map.adjust (A.draw actualPaidOut d (PayPrin bnds)) an accMap

performAction d t@TestDeal{accounts=accMap, bonds=bndMap} (W.PayPrinResidual an bnds) = 
  t {accounts = accMapAfterPay, bonds = bndMapUpdated} -- `debug` ("Bond Prin Pay Result"++show(bndMapUpdated))
  where
    acc = accMap Map.! an

    bndsToPay = filter (not . L.isPaidOff) $ map (bndMap Map.!) bnds
    bndsToPayNames = L.bndName <$> bndsToPay
    availBal = A.accBalance acc
    bndsDueAmts = map L.bndBalance bndsToPay

    actualPaidOut = min availBal $ sum  bndsDueAmts -- `debug` ("bonds totoal due ->"++show(bndsDueAmts))
    bndsAmountToBePaid = zip bndsToPay (prorataFactors bndsDueAmts availBal)
    bndsPaid = map (\(l,amt) -> L.payPrin d amt l) bndsAmountToBePaid --  `debug` ("pay prin->>>To"++show(bnds))

    bndMapUpdated =  Map.union (Map.fromList $ zip bndsToPayNames bndsPaid) bndMap
    accMapAfterPay = Map.adjust (A.draw actualPaidOut d (PayPrin bnds)) an accMap

performAction d t@TestDeal{accounts=accMap} (W.LiquidatePool lm an) =
  t {accounts = accMapAfterLiq } -- TODO need to remove assets
  where
    liqAmt = calcLiquidationAmount lm (pool t) d
    accMapAfterLiq = Map.adjust (A.deposit liqAmt d LiquidationProceeds) an accMap

performAction d t@TestDeal{fees=feeMap} (W.CalcFee fns) 
  = t {fees = Map.union newFeeMap feeMap }
  where 
    newFeeMap = Map.map (calcDueFee t d) $ getFeeByName t (Just fns)

performAction d t@TestDeal{bonds=bndMap} (W.CalcBondInt bns) 
  = t {bonds = Map.union newBondMap bndMap}
  where 
    newBondMap = Map.map (calcDueInt t d) $ getBondByName t (Just bns)

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
      newFeeMap = Map.adjust (F.payFee d transferAmt) fn feeMap
      newLiqMap = Map.adjust (CE.draw transferAmt d ) pName _liqProvider 

performAction d t@TestDeal{bonds=bndMap,liqProvider = Just _liqProvider} (W.LiqSupport limit pName CE.LiqToBondInt bn)
  = t { bonds = newBondMap, liqProvider = Just newLiqMap }
  where 
      _transferAmt = case limit of 
                      Nothing -> 0 
                      Just (DS (CurrentDueBondInt [bn])) -> queryDeal t (CurrentDueBondInt [bn])
                      _ -> error $ "Not implement the limit"++ show limit++"For Pay Yield to liqProvider"
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
      cap = min liqDue $ A.accBalance $ accs Map.! an
      transferAmt = case limit of 
                      Just (DS ds) -> min cap $ queryDeal t (patchDateToStats d ds)
                      Nothing -> cap
                      _ -> error $ "Not implement the limit"++ show limit++"For Repay to liqProvider"
      newAccMap = Map.adjust (A.draw transferAmt d (LiquidationSupport pName)) an accs
      newLiqMap = Map.adjust (CE.repay transferAmt d rpt ) pName _liqProvider 

performAction d t@TestDeal{accounts=accs,liqProvider = Just _liqProvider} (W.LiqYield limit an pName)
  = t { accounts = newAccMap, liqProvider = Just newLiqMap }
  where 
      cap = A.accBalance $ accs Map.! an
      transferAmt = case limit of 
                      Nothing -> cap
                      Just (DS ds) -> min cap $ queryDeal t (patchDateToStats d ds)
                      _ -> error $ "Not implement the limit"++ show limit++"For Pay Yield to liqProvider"
      newAccMap = Map.adjust (A.draw transferAmt d (LiquidationSupport pName)) an accs
      newLiqMap = Map.adjust (CE.repay transferAmt d CE.LiqBal ) pName _liqProvider 

performAction d t@TestDeal{liqProvider = Just _liqProvider} (W.LiqAccrue n)
  = t {liqProvider = Just updatedLiqProvider}
    where 
      updatedLiqProvider = Map.adjust (CE.accrueLiqProvider d ) n _liqProvider

performAction d t@TestDeal{rateSwap = Just rtSwap } (W.SwapAccrue sName)
  = t { rateSwap = Just newRtSwap } 
    where 
        refBal = case  HE.rsNotional (rtSwap Map.! sName) of 
                   (HE.Fixed b) -> b
                   (HE.Base ds) -> queryDeal t (patchDateToStats d ds)
                   (HE.Schedule ts) -> fromRational $ getValByDate ts Inc d
                   
        newRtSwap = Map.adjust 
                      (HE.accrueIRS d)
                      sName
                      (Map.adjust (set HE.rsRefBalLens refBal) sName rtSwap)

performAction d t@TestDeal{rateSwap = Just rtSwap, accounts = accsMap } (W.SwapReceive accName sName)
  = t { rateSwap = Just newRtSwap, accounts = newAccMap }
    where 
        receiveAmt = max 0 $ HE.rsNetCash $ rtSwap Map.! sName
        newRtSwap = Map.adjust (HE.receiveIRS d) sName rtSwap -- `debug` ("REceiv AMT"++ show receiveAmt)
        newAccMap = Map.adjust (A.deposit receiveAmt d SwapInSettle) accName accsMap

performAction d t@TestDeal{rateCap = Just rcM, accounts = accsMap } (W.CollectRateCap accName sName)
  = t { rateCap = Just newRcSwap, accounts = newAccMap }
    where 
        receiveAmt = max 0 $ HE.rcNetCash $ rcM Map.! sName
        newRcSwap = Map.adjust (HE.receiveRC d) sName rcM -- `debug` ("REceiv AMT"++ show receiveAmt)
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
      t2 = performAction d t (W.SwapReceive accName sName)
    in 
      performAction d t2 (W.SwapPay accName sName)


performAction d t@TestDeal{ triggers = Just trgM } (W.RunTrigger loc tName)
  = t { triggers = Just (Map.insert loc newMap trgM) }
    where 
      newMap = Map.adjust (updateTrigger t d) tName (trgM Map.! loc)

performAction d t action =  error $ "failed to match action>>"++show action++">>Deal"++show (name t)
