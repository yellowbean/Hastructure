{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Deal.DealAction (performActionWrap,performAction,calcDueFee
                       ,testTrigger,RunContext(..),updateLiqProvider
                       ,calcDueInt,priceAssetUnion
                       ,priceAssetUnionList,inspectVars) 
  where

import qualified Accounts as A
import qualified Ledger as LD
import qualified Asset as Ast
import qualified Pool as P
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
import GHC.Real (infinity)
import Deal.DealQuery (patchDatesToStats)
import Data.OpenApi (HasPatch(patch))

debug = flip trace

getPoolFlows :: Ast.Asset a => TestDeal a -> Maybe Date -> Maybe Date -> RangeType -> [CF.TsRow]
getPoolFlows t@TestDeal{ pool = _pool } sd ed rt =
  case (sd,ed) of
    (Nothing,Nothing)   ->  trs
    (Nothing,Just _ed)  ->  cutBy Inc Past _ed trs
    (Just _sd,Nothing)  ->  cutBy Inc Future _sd trs 
    (Just _sd,Just _ed) ->  sliceBy rt _sd _ed trs
  where
    trs = getAllCollectedTxnsList t Nothing


-- ^ 
testTrigger :: Ast.Asset a => TestDeal a -> Date -> Trigger -> Trigger
testTrigger t d trigger@Trigger{trgStatus=st,trgCurable=curable,trgCondition=cond,trgStmt = tStmt} 
  | not curable && st = trigger
  | otherwise = let 
                  (memo, newSt) = testPre2 d t cond
                  newTxn = TrgTxn d newSt (Stmt.Tag memo)
                in 
                  trigger { trgStatus = newSt
                           , trgStmt = Stmt.appendStmt tStmt newTxn}


pricingAssets :: PricingMethod -> [ACM.AssetUnion] -> Date -> Amount 
pricingAssets (BalanceFactor currentfactor defaultfactor) assets d = 0 

calcLiquidationAmount :: Ast.Asset a => PricingMethod -> P.Pool a -> Date -> Amount
calcLiquidationAmount (BalanceFactor currentFactor defaultFactor ) pool d 
  = case P.futureCf pool of 
      Nothing -> 0  -- `debug` ("No futureCF")
      Just _futureCf@(CashFlowFrame _ trs) ->
        let 
          earlierTxns = cutBy Inc Past d trs
          currentCumulativeDefaultBal = sum $ map (\x -> CF.mflowDefault x - CF.mflowRecovery x - CF.mflowLoss x) earlierTxns
        in 
          case earlierTxns of 
            [] -> 0  -- `debug` ("No pool Inflow")
            _ -> (mulBR (CF.mflowBalance (last earlierTxns)) currentFactor) + (mulBR currentCumulativeDefaultBal defaultFactor)
            -- TODO need to check if missing last row

calcLiquidationAmount (PV discountRate recoveryPct) pool d 
  = case P.futureCf pool of
      Nothing -> 0 
      Just (CashFlowFrame _ trs) ->
          let 
            futureTxns = cutBy Inc Future d trs
            earlierTxns = cutBy Exc Past d trs 
            pvCf = sum $ map (\x -> AN.pv2  discountRate  d (CF.getDate x) (CF.tsTotalCash x)) futureTxns 
            currentDefaulBal = sum $ map (\x -> CF.mflowDefault x - CF.mflowRecovery x - CF.mflowLoss x) earlierTxns
          in 
            pvCf + mulBI currentDefaulBal recoveryPct

liquidatePool :: Ast.Asset a => PricingMethod -> Date -> String -> TestDeal a -> TestDeal a
liquidatePool lm d accName t@TestDeal { accounts = accs , pool = pool} =
  t {accounts = Map.adjust updateFn accName accs} -- `debug` ("Accs->"++show(accs))
  where
    proceedsByPool = case pool of 
                     MultiPool pm -> Map.map (\p -> calcLiquidationAmount lm p d) pm
                     SoloPool p -> Map.fromList [(PoolConsol,calcLiquidationAmount lm p d)]
                     ResecDeal uDeals -> error "Not implement on liquidate resec deal"
    
    proceeds = sum $ Map.elems proceedsByPool
    updateFn = A.deposit proceeds d LiquidationProceeds

-- actual payout amount to bond with due mounts
allocAmtToBonds :: W.PayOrderBy -> Amount -> [(L.Bond,Amount)] -> [(L.Bond,Amount)]
allocAmtToBonds theOrder amt bndsWithDue =
  case theOrder of 
    W.ByName -> 
      let 
        orderdBonds = sortBy (\(b1,_) (b2,_) -> compare (L.bndName b1) (L.bndName b2)) bndsWithDue
        orderedAmt = snd <$> orderdBonds
        r = paySeqLiabilitiesAmt amt orderedAmt
      in 
        zip (fst <$> orderdBonds) r
    W.ByProRataCurBal -> 
      let 
        r = prorataFactors (snd <$> bndsWithDue) amt -- `debug` ("bd amt"++ show amt)
      in 
        zip (fst <$> bndsWithDue) r -- `debug` ("r >>"++ show r)
    W.ByCurrentRate ->
      let 
        orderdBonds = sortBy (\(b1,_) (b2,_) -> flip compare (L.bndRate b1) (L.bndRate b2)) bndsWithDue
        orderedAmt = snd <$> orderdBonds
        r = paySeqLiabilitiesAmt amt orderedAmt
      in 
        zip (fst <$> orderdBonds) r
    W.ByMaturity ->
      let 
        orderdBonds = sortBy (\(b1@L.Bond{L.bndOriginInfo=bo1},_) (b2@L.Bond{L.bndOriginInfo=bo2},_) -> compare (L.maturityDate bo1) (L.maturityDate bo2)) bndsWithDue
        orderedAmt = snd <$> orderdBonds
        r = paySeqLiabilitiesAmt amt orderedAmt
      in 
        zip (fst <$> orderdBonds) r
    W.ByStartDate -> 
      let 
        orderdBonds = sortBy (\(b1@L.Bond{L.bndOriginInfo=bo1},_) (b2@L.Bond{L.bndOriginInfo=bo2},_) -> compare (L.originDate bo1) (L.originDate bo2)) bndsWithDue
        orderedAmt = snd <$> orderdBonds
        r = paySeqLiabilitiesAmt amt orderedAmt
      in 
        zip (fst <$> orderdBonds) r


calcDueFee :: Ast.Asset a => TestDeal a -> Date -> F.Fee -> F.Fee
calcDueFee t calcDay f@(F.Fee fn (F.FixFee amt) fs fd fdDay fa _ _)
  | isJust fdDay = f  
  | calcDay >= fs && isNothing fdDay = f{ F.feeDue = amt, F.feeDueDate = Just calcDay} -- `debug` ("DEBUG--> init with amt "++show(fd)++show amt)
  | otherwise = f

calcDueFee t calcDay f@(F.Fee fn (F.AnnualRateFee feeBase r) fs fd Nothing fa lpd _)
  | calcDay >= fs = calcDueFee t calcDay f {F.feeDueDate = Just fs }
  | otherwise = f 

-- ^ annualized % fee base on pool balance/amount
calcDueFee t@TestDeal{pool = pool} calcDay f@(F.Fee fn (F.AnnualRateFee feeBase _r) fs fd (Just _fdDay) fa lpd _)
  = f{ F.feeDue=fd+newDue, F.feeDueDate = Just calcDay }  -- `debug` ("Fee DUE new Due "++show newDue++"oldDue"++show fd)
      where 
        accrueStart = _fdDay
        patchedDs = patchDatesToStats t accrueStart calcDay feeBase
        baseBal = queryDeal t patchedDs
        r = toRational $ queryDealRate t _r -- `debug` ("Base "++ show calcDay ++">>"++ show baseBal++"From ds"++show patchedDs++"Fee Name"++fn)
        newDue = mulBR baseBal r -- `debug` ("Fee Name"++fn ++"Date"++ show [accrueStart, calcDay] ++ "base bal"++ show baseBal++"new rate"++show r)

-- ^ % fee base on pool balance/amount
calcDueFee t calcDay f@(F.Fee fn (F.PctFee (PoolCurCollection its mPns) r ) fs fd fdDay fa lpd _)
  = f { F.feeDue = newDueFee, F.feeDueDate = Just calcDay } -- `debug` ("BAL"++show baseBal++"New Fee Due"++ show newDueFee)
    where 
      -- baseBal = sum [ queryDeal t (PoolCollectionHistory it lastBegDay calcDay)  | it <- its ]
      txns = sliceBy EI lastBegDay calcDay $ getAllCollectedTxnsList t mPns
      baseBal = sum $ CF.lookupSource <$> txns <*> its
      newDueFee = fd + mulBR baseBal (toRational (queryDealRate t r))
      lastBegDay = fromMaybe fs fdDay

calcDueFee t calcDay f@(F.Fee fn (F.PctFee ds _r ) fs fd fdDay fa lpd _)
  = f { F.feeDue = fd + mulBR baseBal r, F.feeDueDate = Just calcDay }
    where 
      r = toRational $ queryDealRate t _r
      baseBal = queryDeal t (patchDateToStats calcDay ds)
      lastBegDay = fromMaybe fs fdDay

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
      txnsDates = getDate <$> getAllCollectedTxnsList t (Just [PoolConsol])
      pastPeriods = case fdday of 
                      Nothing ->  subDates II fs calcDay txnsDates
                      Just lastFeeDueDay -> subDates EI lastFeeDueDay calcDay txnsDates
      dueAmt = fromRational $ mulBInt amt (length pastPeriods)

calcDueFee t calcDay f@(F.Fee fn (F.AmtByTbl _ ds tbl) fs fd fdday fa lpd _)
  = f {F.feeDue = dueAmt + fd, F.feeDueDate = Just calcDay}
    where 
      lookupVal = queryDeal t (patchDateToStats calcDay ds)
      dueAmt = fromMaybe 0.0 $ lookupTable tbl Up (lookupVal >=)

disableLiqProvider :: Ast.Asset a => TestDeal a -> Date -> CE.LiqFacility -> CE.LiqFacility
disableLiqProvider _ d liq@CE.LiqFacility{CE.liqEnds = Just endDate } 
  | d > endDate = liq{CE.liqCredit = Just 0}
  | otherwise = liq

disableLiqProvider _ d liq@CE.LiqFacility{CE.liqEnds = Nothing }  = liq

updateLiqProvider :: Ast.Asset a => TestDeal a -> Date -> CE.LiqFacility -> CE.LiqFacility
updateLiqProvider t d liq@CE.LiqFacility{CE.liqType = liqType, CE.liqCredit = curCredit} -- refresh available balance
  = disableLiqProvider t d $ liq { CE.liqCredit = newCredit } 
    where 
      newCredit = case liqType of 
                    CE.ReplenishSupport _ b -> max b <$> curCredit
                    CE.ByPct ds _r -> min (mulBR (queryDeal t ds) _r) <$> curCredit
                    _ -> curCredit

updateLiqProvider t d liq = disableLiqProvider t d liq

calcDueInt :: Ast.Asset a => TestDeal a -> Date -> Maybe DealStats -> Maybe DealStats -> L.Bond -> L.Bond
calcDueInt t calc_date mBal mRate b@(L.BondGroup bMap) = L.BondGroup $ Map.map (calcDueInt t calc_date mBal mRate) bMap 
-- Not accrued
calcDueInt t calc_date mBal mRate b@(L.Bond _ _ oi io _ bal r dp _ di Nothing _ lastPrinPay _ ) 
 | calc_date <= closingDate = b
 | bal+di == 0 = b
 | otherwise = calcDueInt t calc_date mBal mRate (b {L.bndDueIntDate = Just closingDate })  -- `debug` ("hit")
   where 
     closingDate = getClosingDate (dates t)
-- Z bond
calcDueInt t calc_date _ _ b@(L.Bond bn L.Z bo bi _ bond_bal bond_rate _ _ _ _ lstIntPay _ _) 
  = b {L.bndDueInt = 0 }
-- accured by yield
calcDueInt t calc_date _ _ b@(L.Bond bn L.Equity bo (L.InterestByYield y) _ bond_bal _ _ int_due _ _ lstIntPay _ mStmt)
  = b {L.bndDueInt = newDue }  -- `debug` ("Yield Due Int >>"++ show bn++">> new due"++ show newDue++">> old due"++ show int_due )
  where
    newDue = L.backoutDueIntByYield calc_date b

-- accrued with interest over interest
calcDueInt t calc_date mBal mRate b@(L.Bond bn bt bo (L.WithIoI intInfo ioiIntInfo) _ bond_bal bond_rate _ intDue ioiIntDue (Just int_due_date) lstIntPay _ _ )
  = newBondWithIntInfo { L.bndInterestInfo = L.WithIoI intInfo ioiIntInfo}
    where 
      ioiRate = case ioiIntInfo of 
                  L.OverCurrRateBy factor -> bond_rate * fromRational (1+factor)
                  L.OverFixSpread spd -> bond_rate + spd
                  _ -> error "failed to match ioi rate type"
      newIoiInt = IR.calcInt intDue int_due_date calc_date ioiRate DC_ACT_365F
      ioiInt = newIoiInt + ioiIntDue -- add ioi int due with new accrued ioi int
      
      newBond = b { L.bndDueIntOverInt = ioiInt, L.bndInterestInfo = intInfo }
      newBondWithIntInfo = calcDueInt t calc_date mBal mRate newBond

-- accure interest by rate
calcDueInt t calc_date mBal mRate b@(L.Bond bn bt bo bi _ bond_bal bond_rate _ intDue _ (Just int_due_date) lstIntPay _ _ ) 
  | bond_bal == 0 = b
  | calc_date == int_due_date = b
  | otherwise = b {L.bndDueInt = newDueInt+intDue,L.bndDueIntDate = Just calc_date }  --  `debug` ("Due INT"++show calc_date ++">>"++show(bn)++">>"++show int_due++">>"++show(new_due_int))
              where
                dc = case bi of 
                       L.Floater _ _ _ _ _dc _ _ -> _dc 
                       L.Fix _ _dc -> _dc 
                       _ -> DC_ACT_365F
                overrideBal = maybe bond_bal (queryDeal t ) mBal
                overrideRate = maybe bond_rate (queryDealRate t) mRate
                newDueInt = IR.calcInt overrideBal int_due_date calc_date overrideRate dc -- `debug` ("Using Rate"++show calc_date ++">>Bal"++ show overrideBal)


calcDuePrin :: Ast.Asset a => TestDeal a -> T.Day -> L.Bond -> L.Bond
calcDuePrin t calc_date b@(L.BondGroup bMap) = L.BondGroup $ Map.map (calcDuePrin t calc_date) bMap
calcDuePrin t calc_date b@(L.Bond _ L.Sequential _ _ _ bondBal _ _ _ _ _ _ _ _)
  = b {L.bndDuePrin = bondBal } 

calcDuePrin t calc_date b@(L.Bond bn (L.Lockout cd) bo bi _ bondBal _ _ _ _ _ _ _ _) 
  | cd > calc_date = b {L.bndDuePrin = 0}
  | otherwise = b {L.bndDuePrin = bondBal }

calcDuePrin t calc_date b@(L.Bond bn (L.PAC schedule) _ _ _ bondBal _ _ _ _ _ _ _ _) =
  b {L.bndDuePrin = duePrin} -- `debug` ("bn >> "++bn++"Due Prin set=>"++show(duePrin) )
  where
    scheduleDue = getValOnByDate schedule calc_date  
    duePrin = max (bondBal - scheduleDue) 0 -- `debug` ("In PAC ,target balance"++show(schedule)++show(calc_date)++show(scheduleDue))

calcDuePrin t calc_date b@(L.Bond bn (L.PacAnchor schedule bns) _ _ _ bondBal _ _ _ _ _ _ _ _) =
  b {L.bndDuePrin = duePrin} -- `debug` ("bn >> "++bn++"Due Prin set=>"++show(duePrin) )
  where
    scheduleDue = getValOnByDate schedule calc_date
    anchor_bond_balance = queryDeal t (CurrentBondBalanceOf bns)
    duePrin = if anchor_bond_balance > 0 then
                 max (bondBal - scheduleDue) 0
              else
                 bondBal

calcDuePrin t calc_date b@(L.Bond bn L.Z bo bi _ bond_bal bond_rate prin_arr int_arrears _ _ lstIntPay _ _) =
  if all isZbond activeBnds then
      b {L.bndDuePrin = bond_bal} -- `debug` ("bn >> "++bn++"Due Prin set=>"++show(duePrin) )
  else 
      b {L.bndDuePrin = 0, L.bndBalance = new_bal, L.bndLastIntPay=Just calc_date} -- `debug` ("bn >> "++bn++"Due Prin set=>"++show(duePrin) )
  where
    isZbond (L.Bond _ L.Z _ _ _ _ _ _ _ _ _ _ _ _) = True
    isZbond L.Bond {} = False
    
    activeBnds = filter (\x -> L.bndBalance x > 0) (Map.elems (bonds t))
    new_bal = bond_bal + dueInt
    lastIntPayDay = case lstIntPay of
                      Just pd -> pd
                      Nothing -> getClosingDate (dates t)
    dueInt = IR.calcInt bond_bal lastIntPayDay calc_date bond_rate DC_ACT_365F

calcDuePrin t calc_date b@(L.Bond bn L.Equity bo bi _ bondBal _ _ _ _ _ _ _ _)
  = b {L.bndDuePrin = bondBal }


priceAssetUnion :: ACM.AssetUnion -> Date -> PricingMethod  -> AP.AssetPerf -> Maybe [RateAssumption] -> PriceResult
priceAssetUnion (ACM.MO m) d pm aps = Ast.priceAsset m d pm aps 
priceAssetUnion (ACM.LO m) d pm aps = Ast.priceAsset m d pm aps
priceAssetUnion (ACM.IL m) d pm aps = Ast.priceAsset m d pm aps
priceAssetUnion (ACM.LS m) d pm aps = Ast.priceAsset m d pm aps
priceAssetUnion (ACM.RE m) d pm aps = Ast.priceAsset m d pm aps
priceAssetUnion (ACM.PF m) d pm aps = Ast.priceAsset m d pm aps

priceAssetUnionList :: [ACM.AssetUnion] -> Date -> PricingMethod  -> AP.ApplyAssumptionType -> Maybe [RateAssumption] -> [PriceResult]
priceAssetUnionList assetList d pm (AP.PoolLevel assetPerf) mRates 
  = [ priceAssetUnion asset d pm assetPerf mRates | asset <- assetList ]


-- | this would used in `static` revolving ,which assumes the revolving pool will decrease
splitAssetUnion :: [Rate] -> ACM.AssetUnion -> [ACM.AssetUnion]
splitAssetUnion rs (ACM.MO m) = [ ACM.MO a | a <- Ast.splitWith m rs]
splitAssetUnion rs (ACM.LO m) = [ ACM.LO a | a <- Ast.splitWith m rs]
splitAssetUnion rs (ACM.IL m) = [ ACM.IL a | a <- Ast.splitWith m rs]
splitAssetUnion rs (ACM.LS m) = [ ACM.LS a | a <- Ast.splitWith m rs]
splitAssetUnion rs (ACM.RE m) = [ ACM.RE a | a <- Ast.splitWith m rs]

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
                  runPoolFlow:: Map.Map PoolId CF.CashFlowFrame
                  ,revolvingAssump:: Maybe (Map.Map String (RevolvingPool ,AP.ApplyAssumptionType))
                  ,revolvingInterestRateAssump:: Maybe [RateAssumption]
                  }

updateOriginDate2 :: Date -> ACM.AssetUnion -> ACM.AssetUnion
updateOriginDate2 d (ACM.LO m) = ACM.LO $ updateOriginDate m (Ast.calcAlignDate m d)
updateOriginDate2 d (ACM.MO m) = ACM.MO $ updateOriginDate m (Ast.calcAlignDate m d)
updateOriginDate2 d (ACM.IL m) = ACM.IL $ updateOriginDate m (Ast.calcAlignDate m d)
updateOriginDate2 d (ACM.LS m) = ACM.LS $ updateOriginDate m (Ast.calcAlignDate m d)
updateOriginDate2 d (ACM.RE m) = ACM.RE $ updateOriginDate m (Ast.calcAlignDate m d)


-- ^ get available supports in balance
evalExtraSupportBalance :: Ast.Asset a => Date -> TestDeal a -> W.ExtraSupport  -> [Balance]
evalExtraSupportBalance d t (W.WithCondition pre s) 
  | testPre d t pre = evalExtraSupportBalance d t s
  | otherwise = [0]
evalExtraSupportBalance d t@TestDeal{accounts=accMap} (W.SupportAccount an _) = [A.accBalance $ accMap Map.! an]
evalExtraSupportBalance d t@TestDeal{liqProvider=Just liqMap} (W.SupportLiqFacility liqName) = [ fromMaybe (fromRational (toRational infinity)) (CE.liqCredit (liqMap Map.! liqName))] -- `debug` ("Returning"++ show [ fromMaybe 1e100 (CE.liqCredit (liqMap Map.! liqName))])
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
      theLiqProvider = liqMap Map.! liqName
      drawAmt = case CE.liqCredit theLiqProvider of 
                  Nothing -> amt -- `debug` ("From amt"++ show amt)
                  Just b -> min amt b -- `debug` ("From Just"++ show b++">>"++show amt)
      oustandingAmt = amt - drawAmt -- `debug` ("Draw Amt"++show drawAmt++">>"++ show amt ++">>>")
    in 
      (t {liqProvider = Just (Map.adjust (CE.draw drawAmt d) liqName liqMap)}
      , oustandingAmt)

drawExtraSupport d amt (W.MultiSupport supports) t
  = foldr 
      (\support (deal,remainAmt) -> drawExtraSupport d remainAmt support deal) 
      (t,amt) 
      supports

inspectVars :: Ast.Asset a => TestDeal a -> Date -> DealStats -> ResultComponent
inspectVars t d ds =                     
  case getDealStatType ds of 
    RtnRate -> InspectRate d ds $ queryDealRate t (patchDateToStats d ds)
    RtnBool -> InspectBool d ds $ queryDealBool t (patchDateToStats d ds) d
    RtnInt  -> InspectInt d ds $ queryDealInt t (patchDateToStats d ds) d
    _       -> InspectBal d ds $ queryDeal t (patchDateToStats d ds)

showInspection :: ResultComponent -> String
showInspection (InspectRate d ds r) = show r
showInspection (InspectBool d ds r) = show r
showInspection (InspectInt d ds r) = show r
showInspection (InspectBal d ds r) = show r
showInspection x = error $ "not implemented for showing ResultComponent " ++ show x


performActionWrap :: Ast.Asset a => Date -> (TestDeal a, RunContext a, [ResultComponent]) -> W.Action -> (TestDeal a, RunContext a, [ResultComponent])
performActionWrap d 
                  (t@TestDeal{ accounts = accsMap }
                  ,rc@RunContext{runPoolFlow=pFlowMap
                                ,revolvingAssump=Just rMap
                                ,revolvingInterestRateAssump = mRates}
                  ,logs)
                  (W.BuyAsset ml pricingMethod accName pId) 
   = (t { accounts = newAccMap }, newRc, logs )
    where 
      (assetForSale::RevolvingPool, perfAssumps::AP.ApplyAssumptionType) = head $ Map.elems rMap
      _assets = lookupAssetAvailable assetForSale d
      assets = updateOriginDate2 d <$> _assets -- `debug` ("Asset on revolv"++ show _assets)
                
      valuationOnAvailableAssets = sum $ getPriceValue <$> priceAssetUnionList assets d pricingMethod perfAssumps mRates 
      accBal = A.accBalance $ accsMap Map.! accName 
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
      
      (CashFlowFrame _ newBoughtTxn) = CF.consolidateCashFlow $ fst $ projAssetUnionList [updateOriginDate2 d ast | ast <- assetBought ] d perfAssumps mRates  --  `debug` ("Asset bought"++ show [updateOriginDate2 d ast | ast <- assetBought ])
      newPcf = let 
                 pIdToChange = fromMaybe PoolConsol pId
               in 
                 Map.adjust (\(CF.CashFlowFrame st trs) -> 
                              let 
                                dsInterval = getDate <$> trs -- `debug` (">>> agg interval : "++ show (getDate <$> trs ))
                              in 
                                CF.CashFlowFrame st $ CF.aggTsByDates (CF.combineTss [] trs newBoughtTxn) dsInterval) -- `debug` ("date"++show d ++"\n>>Asset bought txn\n"++ show newBoughtTxn++"\n >>existing txns\n"++ show trs++"\n>>> consoled\n"++ show (CF.combineTss [] trs newBoughtTxn)) )
                            pIdToChange
                            pFlowMap 
      newRc = rc {runPoolFlow = newPcf
                 ,revolvingAssump = Just (Map.fromList [("Consol" ,(poolAfterBought, perfAssumps))])}  

performActionWrap d 
                  (t@TestDeal{ accounts = accsMap }
                  ,rc@RunContext{runPoolFlow=pFlowMap
                                ,revolvingAssump=Just rMap
                                ,revolvingInterestRateAssump = mRates}
                  ,logs)
                  (W.BuyAssetFrom ml pricingMethod accName (Just sourcePoolName) pId) 
   = (t { accounts = newAccMap }, newRc, logs )
    where 
      (assetForSale::RevolvingPool, perfAssumps::AP.ApplyAssumptionType) = rMap Map.! sourcePoolName
      _assets = lookupAssetAvailable assetForSale d
      assets = updateOriginDate2 d <$> _assets -- `debug` ("Asset on revolv"++ show _assets)
                
      valuationOnAvailableAssets = sum $ getPriceValue <$> priceAssetUnionList assets d pricingMethod perfAssumps mRates 
      accBal = A.accBalance $ accsMap Map.! accName 
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
      
      (CashFlowFrame _ newBoughtTxn) = fst $ projAssetUnionList [updateOriginDate2 d ast | ast <- assetBought ] d perfAssumps mRates  -- `debug` ("Asset bought"++ show [updateOriginDate2 d ast | ast <- assetBought ])
      -- newPcf = CF.CashFlowFrame $ CF.combineTss [] (tr:trs) newBoughtTxn  -- `debug` ("reolvoing first txn\n"++ show (head newBoughtTxn))
      newPcf = let 
                 pIdToChange = fromMaybe PoolConsol pId
               in 
                 Map.adjust (\(CF.CashFlowFrame st trs) -> 
                              let 
                                dsInterval = getDate <$> trs -- `debug` (">>> agg interval : "++ show (getDate <$> trs ))
                              in 
                                CF.CashFlowFrame st $ CF.aggTsByDates (CF.combineTss [] trs newBoughtTxn) dsInterval) 
                            pIdToChange
                            pFlowMap -- `debug` ("date"++show d ++">>Asset bought txn"++ show newBoughtTxn)

      newRc = rc {runPoolFlow = newPcf
                 ,revolvingAssump = Just (Map.insert sourcePoolName (poolAfterBought, perfAssumps) rMap)}  


performActionWrap d 
                  (t
                  ,rc@RunContext{runPoolFlow=pcf
                                ,revolvingAssump=Nothing
                                ,revolvingInterestRateAssump = mRates}
                  ,logs)
                  (W.BuyAsset ml pricingMethod accName _)
  = error $ "Missing revolving Assumption(asset assumption & asset to buy)" ++ name t

performActionWrap d (t, rc, logs) (W.WatchVal ms dss)
  = (t, rc, logs ++ [newLog])
    where 
      newLog =  InspectWaterfall d ms dss $ [ showInspection (inspectVars t d ds) | ds <- dss ] 

performActionWrap d (t, rc, logs) (W.ActionWithPre p actions) 
  | testPre d t p = foldl (performActionWrap d) (t,rc,logs) actions
  | otherwise = (t, rc, logs)
   -- where 
   -- trgsToRun = preHasTrigger p
   -- trgsEffects = []
   -- (newT, newRc) = runEffects (t, rc) d trgsEffects 
    

performActionWrap d (t, rc, logs) (W.ActionWithPre2 p actionsTrue actionsFalse) 
  | testPre d t p = foldl (performActionWrap d) (t,rc,logs) actionsTrue
  | otherwise = foldl (performActionWrap d) (t,rc,logs) actionsFalse

performActionWrap d (t, rc, logs) a = (performAction d t a,rc,logs) -- `debug` ("DEBUG: Action on "++ show a)

performAction :: Ast.Asset a => Date -> TestDeal a -> W.Action -> TestDeal a
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
    
    feesToPay = map (feeMap Map.!) fns
    feeDueAmts = map F.feeDue feesToPay
    feeTotalDueAmt = sum feeDueAmts

    amtAvailable = availAccBal + supportAvail
                   -- Just (DuePct pct) -> map (\x -> mulBR (F.feeDue x) pct ) feesToPay
                   -- Just (DueCapAmt amt) -> prorataFactors (F.feeDue <$> feesToPay) amt
    dueAmtAfterCap = case mLimit of 
                      Nothing -> feeTotalDueAmt
                      Just (DS ds) -> min (queryDeal t (patchDateToStats d ds)) feeTotalDueAmt
                      Just (DueCapAmt amt) -> min amt feeTotalDueAmt
                      Just (DuePct pct) -> mulBR feeTotalDueAmt pct
    -- total actual pay out
    actualPaidOut = min amtAvailable dueAmtAfterCap 

    feesAmountToBePaid = zip feesToPay $ prorataFactors feeDueAmts actualPaidOut
    feesPaid = map (\(f,amt) -> F.payFee d amt f) feesAmountToBePaid
    -- update primary account map
    accPaidOut = min actualPaidOut availAccBal -- `debug` ("Actual paid out"++ show actualPaidOut++" acc bal"++ show availAccBal ++">>"++ show (snd <$> feesAmountToBePaid)++">>"++ show fns)
    dealAfterAcc = t {accounts = Map.adjust (A.draw accPaidOut d (SeqPayFee fns)) an accMap
                     ,fees = Map.fromList (zip fns feesPaid) <> feeMap}

    supportPaidOut = dueAmtAfterCap - accPaidOut

performAction d t (W.AccrueAndPayIntBySeq mLimit an bnds mSupport)
  = let 
      dealWithBondDue = performAction d t (W.CalcBondInt bnds Nothing Nothing)
    in 
      performAction d dealWithBondDue (W.PayIntBySeq mLimit an bnds mSupport)

performAction d t@TestDeal{bonds=bndMap, accounts=accMap, liqProvider=liqMap} (W.PayIntOverIntBySeq mLimit an bnds mSupport)
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
      dueAmts = L.bndDueIntOverInt <$> bndsList
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


performAction d t@TestDeal{bonds=bndMap,accounts=accMap} (W.PayIntOverInt mLimit an bnds mSupport) =
  case mSupport of 
    Just support -> fst $ drawExtraSupport d supportPaidOut support dealAfterAcc -- `debug` (">>support"++ show mSupport)
    Nothing -> dealAfterAcc -- `debug` ("not mSupport"++ show mSupport)
  where
    accBal = A.accBalance $ accMap Map.! an
    supportAvail = case mSupport of 
                     Just support -> sum ( evalExtraSupportBalance d t support)
                     Nothing -> 0 -- `debug` ("Not support found"++ show mSupport)
    
    availBal = case mLimit of 
                 Nothing -> accBal + supportAvail --`debug` ("Support Avail"++ show supportAvail) 
                 Just (DS ds) -> min (accBal + supportAvail) $ queryDeal t (patchDateToStats d ds)
                 Just (DueCapAmt amt) -> min (accBal + supportAvail) amt
                 _ -> error ("Not support limit type for pay int" <> show mLimit)

    bndsToPay = map (bndMap Map.!) bnds
    bndsDueAmts = map L.bndDueIntOverInt bndsToPay
    bndsNames = map L.bndName bndsToPay

    actualPaidOut = min availBal $ sum bndsDueAmts -- `debug` ("due mats"++ show bndsDueAmts ++">>"++ show availBal)
    bndsAmountToBePaid = zip bndsToPay $ prorataFactors bndsDueAmts actualPaidOut

    -- bond map updated
    bndsPaid = map (\(l,amt) -> L.payInt d amt l) bndsAmountToBePaid
    -- primary account paid out
    accPaidOut = min actualPaidOut accBal
    dealAfterAcc = t {accounts = Map.adjust (A.draw accPaidOut d (PayInt bnds)) an accMap -- `debug` ("Actual Account draw"++ show actual)
                     ,bonds = Map.fromList (zip bndsNames bndsPaid) <> bndMap }
    supportPaidOut = actualPaidOut - accPaidOut -- `debug` ("Support Paid Out"++ show actualPaidOut++">>"++show accPaidOut)


performAction d t@TestDeal{bonds=bndMap,accounts=accMap} (W.PayInt mLimit an bnds mSupport) =
  case mSupport of 
    Just support -> fst $ drawExtraSupport d supportPaidOut support dealAfterAcc -- `debug` (">>support"++ show mSupport)
    Nothing -> dealAfterAcc -- `debug` ("not mSupport"++ show mSupport)
  where
    accBal = A.accBalance $ accMap Map.! an
    supportAvail = case mSupport of 
                     Just support -> sum ( evalExtraSupportBalance d t support)
                     Nothing -> 0 -- `debug` ("Not support found"++ show mSupport)
    
    availBal = case mLimit of 
                 Nothing -> accBal + supportAvail --`debug` ("Support Avail"++ show supportAvail) 
                 Just (DS ds) -> min (accBal + supportAvail) $ queryDeal t (patchDateToStats d ds)
                 Just (DueCapAmt amt) -> min (accBal + supportAvail) amt
                 _ -> error ("Not support limit type for pay int" <> show mLimit)

    bndsToPay = map (bndMap Map.!) bnds
    bndsDueAmts = map L.totalDueInt bndsToPay
    -- bndsDueIoIAmts = map L.bndDueIntOverInt bndsToPay
    bndsNames = map L.bndName bndsToPay

    actualPaidOut = min availBal $ sum bndsDueAmts -- `debug` ("due mats"++ show bndsDueAmts ++">>"++ show availBal)
    bndsAmountToBePaid = zip bndsToPay $ prorataFactors bndsDueAmts actualPaidOut

    -- bond map updated
    bndsPaid = map (\(l,amt) -> L.payInt d amt l) bndsAmountToBePaid
    -- primary account paid out
    accPaidOut = min actualPaidOut accBal
    dealAfterAcc = t {accounts = Map.adjust (A.draw accPaidOut d (PayInt bnds)) an accMap -- `debug` ("Actual Account draw"++ show actual)
                     ,bonds = Map.fromList (zip bndsNames bndsPaid) <> bndMap }
    supportPaidOut = actualPaidOut - accPaidOut -- `debug` ("Support Paid Out"++ show actualPaidOut++">>"++show accPaidOut)
    

performAction d t (W.AccrueAndPayInt mLimit an bnds mSupport) =
  let 
    dealWithBondDue = performAction d t (W.CalcBondInt bnds Nothing Nothing)
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


performAction d t@TestDeal{bonds=bndMap,accounts=accMap} (W.PayPrinGroup mLimit an bndGrpName by mSupport)= 
  dAfterSupport { bonds = Map.insert bndGrpName (L.BondGroup bndMapAfterPay) bndMap 
               , accounts = Map.adjust (A.draw accPay d (PayPrin [bndGrpName])) an accMap }
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
    
    L.BondGroup bndsMap = bndMap Map.! bndGrpName
    bndsToPay = Map.filter (not . L.isPaidOff) bndsMap
    
    bndsWithDueMap = Map.map (calcDuePrin t d) bndsToPay
    bndsDueAmtsMap = Map.map (\x -> (x, L.bndDuePrin x)) bndsWithDueMap
    totalDueAmount = sum $ snd <$> Map.elems bndsDueAmtsMap -- `debug` (">date"++show d++" due amt"++show bndsDueAmtsMap)
    payAmount = min totalDueAmount amtAvailable  -- `debug` (">date total available"++ show amtAvailable)
    
    -- actualPaids =  paySeqLiabilitiesAmt payAmount bndsDueAmts
    payOutPlan = allocAmtToBonds by payAmount (Map.elems bndsDueAmtsMap) -- `debug` (">date"++ show payAmount)
    payOutPlanWithBondName = [ (L.bndName bnd,amt) | (bnd,amt) <- payOutPlan] -- `debug` (">date"++show d++"payOutPlan"++ show payOutPlan)
    -- update bond paid
    bndMapAfterPay = foldr 
                      (\(bndName, _amt) acc -> Map.adjust (L.payPrin d _amt) bndName acc)
                      bndsMap
                      payOutPlanWithBondName -- `debug` (">date"++show d++"payoutPlan"++ show payOutPlanWithBondName)
    -- update account 
    accPay = min (sum (snd <$> payOutPlanWithBondName)) accBal
    -- update liq Provider
    supportPay = sum (snd <$> payOutPlanWithBondName) - accPay
    dAfterSupport = case mSupport of 
                      Nothing -> t
                      Just s -> fst $ drawExtraSupport d supportPay s t



performAction d t@TestDeal{bonds=bndMap} (W.AccrueAndPayIntGroup mLimit an bndName by mSupport)
  = let 
       dAfterAcc = performAction d t (W.AccrueIntGroup [bndName])-- `debug` ("Acc due int grp"++ show (getDueInt (bndMap Map.! bndName)))
    in 
       performAction d dAfterAcc (W.PayIntGroup mLimit an bndName by mSupport)


performAction d t@TestDeal{bonds=bndMap} (W.AccrueIntGroup bndNames)
  = t {bonds = Map.union bondGrpAccrued bndMap}
    where 
        bondGrp = Map.filterWithKey (\k _ -> S.member k (S.fromList bndNames)) bndMap
        bondGrpAccrued = Map.map (calcDueInt t d Nothing Nothing) bondGrp



performAction d t@TestDeal{bonds=bndMap,accounts=accMap} (W.PayIntGroup mLimit an bndGrpName by mSupport)
  = dAfterSupport { bonds = Map.insert bndGrpName (L.BondGroup bndMapAfterPay) bndMap 
                  , accounts = Map.adjust (A.draw accPay d (PayInt [bndGrpName])) an accMap }
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
    
    L.BondGroup bndsMap = bndMap Map.! bndGrpName
    bndsToPay = Map.filter (not . L.isPaidOff) bndsMap
    
    bndsWithDueMap = Map.map (calcDueInt t d Nothing Nothing) bndsToPay
    bndsDueAmtsMap = Map.map (\x -> (x, L.totalDueInt x)) bndsWithDueMap
    totalDueAmount = sum $ snd <$> Map.elems bndsDueAmtsMap
    payAmount = min totalDueAmount amtAvailable -- actual payout total amount
    
    -- actualPaids =  paySeqLiabilitiesAmt payAmount bndsDueAmts
    payOutPlan = allocAmtToBonds by payAmount (Map.elems bndsDueAmtsMap)
    payOutPlanWithBondName = [ (L.bndName bnd,amt) | (bnd,amt) <- payOutPlan]
    -- update bond paid
    bndMapAfterPay = foldr 
                      (\(bndName, _amt) acc -> Map.adjust (L.payInt d _amt) bndName acc)
                      bndsMap
                      payOutPlanWithBondName
    -- update account 
    accPay = min (sum (snd <$> payOutPlanWithBondName)) accBal
    -- update liq Provider
    supportPay = sum (snd <$> payOutPlanWithBondName) - accPay
    dAfterSupport = case mSupport of 
                      Nothing -> t
                      Just s -> fst $ drawExtraSupport d supportPay s t




performAction d t@TestDeal{bonds=bndMap,accounts=accMap} (W.PayPrinWithDue an bnds Nothing) =
  t {accounts = accMapAfterPay, bonds = bndMapUpdated}
  where
    acc = accMap Map.! an
    availBal = A.accBalance acc
    bndsToPay = filter (not . L.isPaidOff) $ map (bndMap Map.!) bnds
    bndsToPayNames = L.bndName <$> bndsToPay
    bndsDueAmts = L.bndDuePrin <$> bndsToPay
    actualPaidOut = min availBal $ sum bndsDueAmts
    bndsAmountToBePaid = zip bndsToPay $ prorataFactors bndsDueAmts actualPaidOut
    bndsPaid = map (\(l,amt) -> L.payPrin d amt l) bndsAmountToBePaid
    bndMapUpdated = Map.union (Map.fromList $ zip bndsToPayNames bndsPaid) bndMap
    accMapAfterPay = Map.adjust (A.draw actualPaidOut d (PayPrin bnds)) an accMap


performAction d t@TestDeal{bonds=bndMap,accounts=accMap} (W.PayPrin (Just (DS ds)) an bnds Nothing) = 
  t {accounts = accMapAfterPay, bonds = bndsUpdated}
  where
    availBal = A.accBalance $ accMap Map.! an
    
    bndsToPay = filter (not . L.isPaidOff) $ map (bndMap Map.!) bnds
    bndsToPayNames = L.bndName <$> bndsToPay
    bndsDueAmts = L.bndDuePrin . calcDuePrin t d <$> bndsToPay 
    payAmount = min (sum bndsDueAmts) $ min availBal $ queryDeal t $ patchDateToStats d ds  

    bndsAmountToBePaid = zip bndsToPay $ prorataFactors bndsDueAmts payAmount  -- (bond, amt-allocated)

    bndsPaid = map (\(b,amt) -> L.payPrin d amt b) bndsAmountToBePaid
    bndsUpdated = Map.union (Map.fromList $ zip bndsToPayNames bndsPaid) bndMap

    accMapAfterPay = Map.adjust
                        (A.draw payAmount d (TxnComments [PayPrin bnds,UsingDS ds])) 
                        an
                        accMap

performAction d t@TestDeal{bonds=bndMap,accounts=accMap} (W.PayPrin Nothing an bnds mSupport) =
  case mSupport of
    Just support -> fst $ drawExtraSupport d supportPaidOut support dealAfterAcc
    Nothing -> dealAfterAcc
    -- t {accounts = accMapAfterPay, bonds = bndMapUpdated} 
  where
    acc = accMap Map.! an
   
    supportAvail = case mSupport of 
                     Just support -> sum ( evalExtraSupportBalance d t support)
                     Nothing -> 0 -- `debug` ("Not support found"++ show mSupport)
       
    availBal = A.accBalance acc + supportAvail
    
    bndsToPay = filter (not . L.isPaidOff) $ map (bndMap Map.!) bnds
    bndsToPayNames = L.bndName <$> bndsToPay

    bndsWithDue = map (calcDuePrin t d) bndsToPay  --
    bndsDueAmts = map L.bndDuePrin bndsWithDue

    actualPaidOut = min availBal $ sum bndsDueAmts -- `debug` ("bonds to pay"++show bnds ++"bonds totoal due ->"++show(bndsDueAmts))
    
    bndsAmountToBePaid = zip bndsWithDue (prorataFactors bndsDueAmts actualPaidOut)
    bndsPaid = map (\(l,amt) -> L.payPrin d amt l) bndsAmountToBePaid --  `debug` ("pay prin->>>To"++show(bnds))

    accPaidOut = min actualPaidOut $ A.accBalance acc
    supportPaidOut = actualPaidOut - accPaidOut

    dealAfterAcc = t {
      accounts = Map.adjust (A.draw accPaidOut d (PayPrin bnds)) an accMap
      , bonds = Map.union (Map.fromList $ zip bndsToPayNames bndsPaid) bndMap
    }


performAction d t@TestDeal{accounts=accMap, bonds=bndMap} (W.PayPrinResidual an bnds) = 
  t {accounts = accMapAfterPay, bonds = bndMapUpdated} -- `debug` ("Bond Prin Pay Result"++show(bndMapUpdated))
  where
    acc = accMap Map.! an

    bndsToPay = filter (not . L.isPaidOff) $ map (bndMap Map.!) bnds
    bndsToPayNames = L.bndName <$> bndsToPay
    availBal = A.accBalance acc
    bndsDueAmts = map L.getCurBalance bndsToPay

    actualPaidOut = min availBal $ sum bndsDueAmts -- `debug` ("bonds totoal due ->"++show(bndsDueAmts))
    bndsAmountToBePaid = zip bndsToPay (prorataFactors bndsDueAmts actualPaidOut)
    bndsPaid = map (\(l,amt) -> L.payPrin d amt l) bndsAmountToBePaid  -- `debug` ("pay bonds "++show bnds ++"pay prin->>>To"++show(prorataFactors bndsDueAmts availBal))

    bndMapUpdated =  Map.union (Map.fromList $ zip bndsToPayNames bndsPaid) bndMap
    accMapAfterPay = Map.adjust (A.draw actualPaidOut d (PayPrin bnds)) an accMap

performAction d t@TestDeal{accounts=accMap, bonds=bndMap} (W.FundWith mlimit an bond) = 
  t {accounts = accMapAfterFund, bonds= bndMapUpdated }
  where
    fundAmt = case mlimit of 
                Just (DS ds) -> queryDeal t (patchDateToStats d ds)
                Just (DueCapAmt amt) -> amt
                _ -> error $ "must specify fund amount for bond "++ show bond
                
    accMapAfterFund = Map.adjust (A.deposit fundAmt d (FundWith bond fundAmt)) an accMap
    bndMapUpdated = Map.adjust ((L.fundWith d fundAmt) . (calcDueInt t d Nothing Nothing)) bond bndMap
    -- bndMapUpdated = Map.adjust ((calcDueInt t d Nothing Nothing) ) bond bndMap

performAction d t@TestDeal{bonds=bndMap} (W.WriteOff mlimit bnd)
  = t {bonds = bndMapUpdated}
  where 
    writeAmt = case mlimit of
                  Just (DS ds) -> queryDeal t (patchDateToStats d ds)
                  Just (DueCapAmt amt) -> amt
                  Nothing -> L.bndBalance $ bndMap Map.! bnd
                  x -> error $ "not supported type to determine the amount to write off"++ show x

    writeAmtCapped = min writeAmt $ L.bndBalance $ bndMap Map.! bnd
    bndMapUpdated = Map.adjust ((L.writeOff d writeAmtCapped) . (calcDueInt t d Nothing Nothing)) bnd bndMap


performAction d t@TestDeal{accounts=accMap, pool = pool} (W.LiquidatePool lm an) =
  t {accounts = accMapAfterLiq } -- TODO need to remove assets/cashflow frame
  where
    liqAmtByPool = case pool of 
                     MultiPool pm -> Map.map (\p -> calcLiquidationAmount lm p d) pm
                     SoloPool p -> Map.fromList [(PoolConsol,calcLiquidationAmount lm p d)]
                     ResecDeal uDeals -> error "Not implement on liquidate resec deal"

    liqAmt = sum $ Map.elems liqAmtByPool
    accMapAfterLiq = Map.adjust (A.deposit liqAmt d LiquidationProceeds) an accMap

performAction d t@TestDeal{fees=feeMap} (W.CalcFee fns) 
  = t {fees = Map.union newFeeMap feeMap }
  where 
    newFeeMap = Map.map (calcDueFee t d) $ getFeeByName t (Just fns)

performAction d t@TestDeal{bonds=bndMap} (W.CalcBondInt bns mBalDs mRateDs) 
  = t {bonds = Map.union newBondMap bndMap}
  where 
    newBondMap = Map.map (calcDueInt t d mBalDs mRateDs) $ getBondsByName t (Just bns)

performAction d t@TestDeal{bonds=bndMap} (W.CalcBondPrin2 mLimit bnds) 
  = t {bonds = newBndMap} -- `debug` ("New map after calc due"++ show (Map.mapWithKey (\k v -> (k, L.bndDuePrin v)) newBndMap))
  where 
    limitCap = case mLimit of 
                 Just (DS ds) -> queryDeal t (patchDateToStats d ds)
                 Just (DueCapAmt amt) -> amt
                 Nothing -> 0

    bndsToPay = filter (not . L.isPaidOff) $ map (bndMap Map.!) bnds
    bndsToPayNames = L.bndName <$> bndsToPay
    bndsDueAmts = L.bndDuePrin . calcDuePrin t d <$> bndsToPay
    
    bndsAmountToBePaid = zip bndsToPayNames $ prorataFactors bndsDueAmts limitCap
    
    newBndMap = foldr 
                  (\(bn,amt) acc -> Map.adjust (\b -> b {L.bndDuePrin = amt})  bn acc) 
                  bndMap 
                  bndsAmountToBePaid -- `debug` ("Calc Bond Prin"++ show bndsAmountToBePaid)

performAction d t@TestDeal{bonds=bndMap} (W.CalcBondPrin mLimit accName bnds mSupport) 
  = t {bonds = newBndMap} -- `debug` ("New map after calc due"++ show (Map.mapWithKey (\k v -> (k, L.bndDuePrin v)) newBndMap))
  where 
    accBal = A.accBalance $ accounts t Map.! accName
    availBal = case mSupport of 
                Nothing -> accBal
                Just support -> accBal + sum ( evalExtraSupportBalance d t support)

    limitCap = case mLimit of 
                 Just (DS ds) -> queryDeal t (patchDateToStats d ds)
                 Just (DueCapAmt amt) -> amt
                 Nothing -> availBal

    bndsToPay = filter (not . L.isPaidOff) $ map (bndMap Map.!) bnds
    bndsToPayNames = L.bndName <$> bndsToPay
    bndsDueAmts = L.bndDuePrin . calcDuePrin t d <$> bndsToPay
    
    payAmount = min availBal limitCap

    bndsAmountToBePaid = zip bndsToPayNames $ prorataFactors bndsDueAmts payAmount  -- (bond, amt-allocated)
    
    newBndMap = foldr 
                  (\(bn,amt) acc -> Map.adjust (\b -> b {L.bndDuePrin = amt})  bn acc) 
                  bndMap 
                  bndsAmountToBePaid -- `debug` ("Calc Bond Prin"++ show bndsAmountToBePaid)


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
                      Just (DS ds) -> min cap $ queryDeal t (patchDateToStats d ds) -- `debug` ("Cap acc"++ show cap)
                      Nothing -> cap
                      _ -> error $ "Not implement the limit"++ show limit++"For Repay to liqProvider"
      newAccMap = Map.adjust (A.draw transferAmt d (LiquidationSupport pName)) an accs -- `debug` ("repay liq amt"++ show transferAmt)
      newLiqMap = Map.adjust (CE.repay transferAmt d rpt ) pName _liqProvider 

performAction d t@TestDeal{accounts=accs,liqProvider = Just _liqProvider} (W.LiqYield limit an pName)
  = t { accounts = newAccMap, liqProvider = Just newLiqMap }
  where 
      cap = A.accBalance $ accs Map.! an
      transferAmt = case limit of 
                      Nothing -> cap
                      Just (DS ds) -> min cap $ queryDeal t (patchDateToStats d ds) -- `debug` ("L Repay Yield:Cap"++show cap++">>>"++ show (queryDeal t (patchDateToStats d ds))++">>>ds >>"++ show ds)
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


performAction d t@TestDeal{ triggers = Just trgM } (W.RunTrigger loc tNames)
  = t { triggers = Just (Map.insert loc newMap trgM) }
    where 
      -- newMap = Map.adjust (updateTrigger t d) tName (trgM Map.! loc)
      triggerM = trgM Map.! loc
      newMap = foldr 
                (Map.adjust (testTrigger t d))
                triggerM
                tNames

performAction d t action =  error $ "failed to match action>>"++show action++">>Deal"++show (name t)
