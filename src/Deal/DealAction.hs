{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Deal.DealAction (performActionWrap,performAction,calcDueFee
                       ,testTrigger,RunContext(..),updateLiqProvider
                       ,calcDueInt,priceAssetUnion
                       ,priceAssetUnionList,inspectVars,inspectListVars) 
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
import Control.Applicative

import Debug.Trace
import Cashflow (CashFlowFrame(CashFlowFrame))
import Control.Lens hiding (element)
import Control.Lens.TH
import Control.Monad
import GHC.Real (infinity)
import Data.OpenApi (HasPatch(patch))

debug = flip trace

-- ^ 
testTrigger :: Ast.Asset a => TestDeal a -> Date -> Trigger -> Either String Trigger
testTrigger t d trigger@Trigger{trgStatus=st,trgCurable=curable,trgCondition=cond,trgStmt = tStmt} 
  | not curable && st = Right trigger
  | otherwise = let 
                  (memo, newStM) = testPre2 d t cond
                in 
                  do 
                    newSt <- newStM
                    return trigger { trgStatus = newSt
                                    , trgStmt = Stmt.appendStmt tStmt (TrgTxn d newSt (Stmt.Tag memo))}


pricingAssets :: PricingMethod -> [(ACM.AssetUnion,AP.AssetPerf)] -> Maybe [RateAssumption] -> Date 
              -> Either String [PriceResult]
pricingAssets pm assetsAndAssump ras d 
 = let 
    pricingResults = (\(ast,perf) -> priceAssetUnion ast d pm perf ras) <$> assetsAndAssump
   in
    sequenceA pricingResults


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


calcDueFee :: Ast.Asset a => TestDeal a -> Date -> F.Fee -> Either String F.Fee
calcDueFee t calcDay f@(F.Fee fn (F.FixFee amt) fs fd fdDay fa _ _)
  | isJust fdDay = Right f 
  | calcDay >= fs && isNothing fdDay = Right f { F.feeDue = amt, F.feeDueDate = Just calcDay} -- `debug` ("DEBUG--> init with amt "++show(fd)++show amt)
  | otherwise = Right f

calcDueFee t calcDay f@(F.Fee fn (F.AnnualRateFee feeBase r) fs fd Nothing fa lpd _)
  | calcDay >= fs = calcDueFee t calcDay f {F.feeDueDate = Just fs }
  | otherwise = Right f 

-- ^ annualized % fee base on pool balance amount
calcDueFee t@TestDeal{pool = pool} calcDay f@(F.Fee fn (F.AnnualRateFee feeBase _r) fs fd (Just _fdDay) fa lpd _)
  = let 
      accrueStart = _fdDay
      patchedDs = patchDatesToStats t accrueStart calcDay feeBase
    in 
      do
        r <- queryCompound t calcDay _r 
        baseBal <- queryCompound t calcDay patchedDs
        let newDue = baseBal * r 
        return f { F.feeDue=fd+ fromRational newDue, F.feeDueDate = Just calcDay }

calcDueFee t calcDay f@(F.Fee fn (F.PctFee ds _r ) fs fd fdDay fa lpd _)
  = let 
      lastBegDay = fromMaybe fs fdDay
    in
      do
        r <-  queryCompound t calcDay _r
        baseBal <- queryCompound t calcDay (patchDateToStats calcDay ds)
        return f { F.feeDue = fd + fromRational (baseBal * r), F.feeDueDate = Just calcDay }

calcDueFee t calcDay f@(F.Fee fn (F.FeeFlow ts)  fs fd _ fa mflpd _)
  = Right $
      f{ F.feeDue = newFeeDue
      ,F.feeDueDate = Just calcDay
      ,F.feeType = F.FeeFlow futureDue} 
    where
      (currentNewDue,futureDue) = splitTsByDate ts calcDay 
      cumulativeDue = sumValTs currentNewDue
      newFeeDue =  cumulativeDue + fd  

calcDueFee t calcDay f@(F.Fee fn (F.RecurFee p amt)  fs fd mLastAccDate fa _ _)
  | periodGaps == 0 = Right f 
  | otherwise = Right f { F.feeDue = amt * fromIntegral periodGaps + fd
                        , F.feeDueDate = Just (T.addDays 1 calcDay) } -- `debug` ("periods"++show periodGaps)
  where
    accDates = case mLastAccDate of 
                      Nothing -> genSerialDatesTill2 NO_IE (T.addDays 1 fs) p calcDay 
                      Just lastAccDate -> genSerialDatesTill2 NO_IE lastAccDate p calcDay 
    periodGaps = length accDates 

calcDueFee t calcDay f@(F.Fee fn (F.NumFee p s amt) fs fd Nothing fa lpd _)
  | calcDay >= fs = calcDueFee t calcDay f {F.feeDueDate = Just fs }
  | otherwise = Right f 

calcDueFee t calcDay f@(F.Fee fn (F.NumFee p s amt) fs fd (Just _fdDay) fa lpd _)
  | _fdDay == calcDay = Right f 
  | periodGap == 0 = Right f 
  | otherwise = do 
                  baseCount <- queryCompound t calcDay (patchDateToStats calcDay s)
                  let newFeeDueAmt = (fromRational baseCount) * amt * fromIntegral periodGap -- `debug` ("amt"++show amt++">>"++show baseCount++">>"++show periodGap)
                  return f { F.feeDue = fd+newFeeDueAmt , F.feeDueDate = Just calcDay } 
  where 
    dueDates = projDatesByPattern p _fdDay (pred calcDay)
    periodGap = length dueDates  -- `debug` ("Due Dates"++ show dueDates)

calcDueFee t calcDay f@(F.Fee fn (F.TargetBalanceFee dsDue dsPaid) fs fd _ fa lpd _)
  = do 
      let dsDueD = patchDateToStats calcDay dsDue 
      let dsPaidD = patchDateToStats calcDay dsPaid
      dueAmt <- max 0 <$> (liftA2) (-) (queryCompound t calcDay dsDueD) (queryCompound t calcDay dsPaidD)
      return f { F.feeDue = fromRational dueAmt, F.feeDueDate = Just calcDay}

calcDueFee t@TestDeal{ pool = pool } calcDay f@(F.Fee fn (F.ByCollectPeriod amt) fs fd fdday fa lpd _)
  = Right $ f {F.feeDue = dueAmt + fd, F.feeDueDate = Just calcDay}
    where 
      txnsDates = getDate <$> getAllCollectedTxnsList t (Just [PoolConsol])
      pastPeriods = case fdday of 
                      Nothing ->  subDates II fs calcDay txnsDates
                      Just lastFeeDueDay -> subDates EI lastFeeDueDay calcDay txnsDates
      dueAmt = fromRational $ mulBInt amt (length pastPeriods)

calcDueFee t calcDay f@(F.Fee fn (F.AmtByTbl _ ds tbl) fs fd fdday fa lpd _)
  = do
      lookupVal <- queryCompound t calcDay (patchDateToStats calcDay ds)
      let dueAmt = fromMaybe 0.0 $ lookupTable tbl Up ( fromRational lookupVal >=)
      return f {F.feeDue = dueAmt + fd, F.feeDueDate = Just calcDay}

disableLiqProvider :: Ast.Asset a => TestDeal a -> Date -> CE.LiqFacility -> CE.LiqFacility
disableLiqProvider _ d liq@CE.LiqFacility{CE.liqEnds = Just endDate } 
  | d > endDate = liq{CE.liqCredit = Just 0}
  | otherwise = liq

disableLiqProvider _ d liq@CE.LiqFacility{CE.liqEnds = Nothing }  = liq


 -- refresh available balance
 ---- for Replenish Support and ByPct
updateLiqProvider :: Ast.Asset a => TestDeal a -> Date -> CE.LiqFacility -> CE.LiqFacility
updateLiqProvider t d liq@CE.LiqFacility{CE.liqType = liqType, CE.liqCredit = curCredit}
  = disableLiqProvider t d $ liq { CE.liqCredit = newCredit } 
    where 
      -- TODO ,need to remove due int and due fee
      newCredit = case liqType of 
                    --  CE.ReplenishSupport _ b -> max b <$> curCredit
                    CE.ByPct ds _r ->  case (* _r) <$> (queryCompound t d (patchDateToStats d ds)) of
                                         Left y -> Nothing -- TODO tobe fix error
                                         Right x -> (min (fromRational x)) <$> curCredit
                    _ -> curCredit


calcDueInt :: Ast.Asset a => TestDeal a -> Date -> Maybe DealStats -> Maybe DealStats -> L.Bond -> Either String L.Bond
calcDueInt t calc_date mBal mRate b@(L.BondGroup bMap) 
  = do 
      m  <- mapM (calcDueInt t calc_date mBal mRate) bMap 
      return $ L.BondGroup m 
-- Not accrued
calcDueInt t calc_date mBal mRate b@(L.Bond _ _ oi io _ bal r dp _ di Nothing _ lastPrinPay _ ) 
 | calc_date <= closingDate = Right b
 | bal+di == 0 = Right b
 | otherwise = calcDueInt t calc_date mBal mRate (b {L.bndDueIntDate = Just closingDate })  -- `debug` ("hit")
   where 
     closingDate = getClosingDate (dates t)
-- Z bond
calcDueInt t calc_date _ _ b@(L.Bond bn L.Z bo bi _ bond_bal bond_rate _ _ _ _ lstIntPay _ _) 
  = Right $ b {L.bndDueInt = 0 }
-- accured by yield
calcDueInt t calc_date _ _ b@(L.Bond bn L.Equity bo (L.InterestByYield y) _ bond_bal _ _ int_due _ _ lstIntPay _ mStmt)
  = Right $ b {L.bndDueInt = newDue }  -- `debug` ("Yield Due Int >>"++ show bn++">> new due"++ show newDue++">> old due"++ show int_due )
  where
    newDue = L.backoutDueIntByYield calc_date b

-- accrued with interest over interest
calcDueInt t calc_date mBal mRate b@(L.Bond bn bt bo (L.WithIoI intInfo ioiIntInfo) _ bond_bal bond_rate _ intDue ioiIntDue (Just int_due_date) lstIntPay _ _ )
  = 
    let
      ioiRate = case ioiIntInfo of 
                  L.OverCurrRateBy factor -> bond_rate * fromRational (1+factor)
                  L.OverFixSpread spd -> bond_rate + spd
                  _ -> error "failed to match ioi rate type"
      newIoiInt = IR.calcInt intDue int_due_date calc_date ioiRate DC_ACT_365F
      ioiInt = newIoiInt + ioiIntDue -- add ioi int due with new accrued ioi int
      
      newBond = b { L.bndDueIntOverInt = ioiInt, L.bndInterestInfo = intInfo }
    in 
      do 
        newBondWithIntInfo <- calcDueInt t calc_date mBal mRate newBond
        return newBondWithIntInfo { L.bndInterestInfo = L.WithIoI intInfo ioiIntInfo}
-- accure interest by rate
calcDueInt t calc_date mBal mRate b@(L.Bond bn bt bo bi _ bond_bal bond_rate _ intDue _ (Just int_due_date) lstIntPay _ _ ) 
  | bond_bal == 0 =  Right $ b
  | calc_date == int_due_date =  Right $ b
  | otherwise = 
      let 
        dc = case bi of 
               L.Floater _ _ _ _ _dc _ _ -> _dc 
               L.Fix _ _dc -> _dc 
               _ -> DC_ACT_365F
      in 
        do
          overrideRate <- maybe (Right bond_rate) ((fromRational <$>) . (queryCompound t calc_date)) mRate
          overrideBal <- maybe (Right bond_bal) ((fromRational <$>) . (queryCompound t calc_date)) mBal
          let newDueInt = IR.calcInt overrideBal int_due_date calc_date overrideRate dc -- `debug` ("Using Rate"++show calc_date ++">>Bal"++ show overrideBal)
          return b {L.bndDueInt = newDueInt+intDue, L.bndDueIntDate = Just calc_date }  --  `debug` ("Due INT"++show calc_date ++">>"++show(bn)++">>"++show int_due++">>"++show(new_due_int))
       
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


priceAssetUnion :: ACM.AssetUnion -> Date -> PricingMethod  -> AP.AssetPerf -> Maybe [RateAssumption] 
                -> Either String PriceResult
priceAssetUnion (ACM.MO m) d pm aps mras = Ast.priceAsset m d pm aps mras Inc
priceAssetUnion (ACM.LO m) d pm aps mras = Ast.priceAsset m d pm aps mras Inc
priceAssetUnion (ACM.IL m) d pm aps mras = Ast.priceAsset m d pm aps mras Inc
priceAssetUnion (ACM.LS m) d pm aps mras = Ast.priceAsset m d pm aps mras Inc 
priceAssetUnion (ACM.RE m) d pm aps mras = Ast.priceAsset m d pm aps mras Inc
priceAssetUnion (ACM.PF m) d pm aps mras = Ast.priceAsset m d pm aps mras Inc
priceAssetUnion (ACM.FA m) d pm aps mras = Ast.priceAsset m d pm aps mras Inc

priceAssetUnionList :: [ACM.AssetUnion] -> Date -> PricingMethod  -> AP.ApplyAssumptionType -> Maybe [RateAssumption] 
                    -> Either String [PriceResult]
priceAssetUnionList assetList d pm (AP.PoolLevel assetPerf) mRates 
  = sequenceA [ priceAssetUnion asset d pm assetPerf mRates | asset <- assetList ]


-- | this would used in `static` revolving ,which assumes the revolving pool will decrease
splitAssetUnion :: [Rate] -> ACM.AssetUnion -> [ACM.AssetUnion]
splitAssetUnion rs (ACM.MO m) = [ ACM.MO a | a <- Ast.splitWith m rs]
splitAssetUnion rs (ACM.LO m) = [ ACM.LO a | a <- Ast.splitWith m rs]
splitAssetUnion rs (ACM.IL m) = [ ACM.IL a | a <- Ast.splitWith m rs]
splitAssetUnion rs (ACM.LS m) = [ ACM.LS a | a <- Ast.splitWith m rs]
splitAssetUnion rs (ACM.RE m) = [ ACM.RE a | a <- Ast.splitWith m rs]
splitAssetUnion rs (ACM.FA m) = [ ACM.FA a | a <- Ast.splitWith m rs]

-- ^ return assets bought and pool after bought
buyRevolvingPool :: Date -> Rate -> RevolvingPool -> ([ACM.AssetUnion],RevolvingPool)
buyRevolvingPool _ 0 rp = ([],rp)
buyRevolvingPool _ r rp@(StaticAsset assets) 
  = let 
      splitRatios = if r >= 1 then 
                      [1.0,0]
                    else
                      [r,1-r]
      splitedAssets = splitAssetUnion splitRatios <$> assets
      assetBought = head <$> splitedAssets
      assetRemains = last <$> splitedAssets 
    in 
      (assetBought ,StaticAsset assetRemains)

buyRevolvingPool _ r rp@(ConstantAsset assets)
  = let 
      splitedAssets = splitAssetUnion [r,0] <$> assets
      assetBought = head <$> splitedAssets
    in 
      (assetBought ,rp)

buyRevolvingPool d r rp@(AssetCurve aus)
  = let
      splitRatios = if r >= 1 then 
                      [1.0,0]
                    else
                      [r,1-r]
      assets = lookupAssetAvailable rp d 
      splitedAssets = splitAssetUnion splitRatios <$> assets
      assetBought = head <$> splitedAssets
    in 
      (assetBought, rp)


data RunContext a = RunContext{
                  runPoolFlow:: Map.Map PoolId CF.CashFlowFrame
                  ,revolvingAssump:: Maybe (Map.Map String (RevolvingPool ,AP.ApplyAssumptionType))
                  ,revolvingInterestRateAssump:: Maybe [RateAssumption]
                  }
                  deriving (Show)

updateOriginDate2 :: Date -> ACM.AssetUnion -> ACM.AssetUnion
updateOriginDate2 d (ACM.LO m) = ACM.LO $ updateOriginDate m (Ast.calcAlignDate m d)
updateOriginDate2 d (ACM.MO m) = ACM.MO $ updateOriginDate m (Ast.calcAlignDate m d)
updateOriginDate2 d (ACM.IL m) = ACM.IL $ updateOriginDate m (Ast.calcAlignDate m d)
updateOriginDate2 d (ACM.LS m) = ACM.LS $ updateOriginDate m (Ast.calcAlignDate m d)
updateOriginDate2 d (ACM.RE m) = ACM.RE $ updateOriginDate m (Ast.calcAlignDate m d)


-- ^ get available supports in balance
evalExtraSupportBalance :: Ast.Asset a => Date -> TestDeal a -> W.ExtraSupport -> Either String Balance
evalExtraSupportBalance d t (W.WithCondition pre s) 
  = do
      flag <- testPre d t pre
      if flag then 
        evalExtraSupportBalance d t s
      else
        return 0
evalExtraSupportBalance d t@TestDeal{accounts=accMap} (W.SupportAccount an _) 
  = return $ A.accBalance $ accMap Map.! an
evalExtraSupportBalance d t@TestDeal{liqProvider=Just liqMap} (W.SupportLiqFacility liqName) 
  = return 1e100
  -- = [ fromMaybe 1e100 (CE.liqCredit (liqMap Map.! liqName))] -- `debug` ("Returning"++ show [ fromMaybe 1e100 (CE.liqCredit (liqMap Map.! liqName))])
  -- = [ fromMaybe (fromRational (toRational infinity)) (CE.liqCredit (liqMap Map.! liqName))] -- `debug` ("Returning"++ show [ fromMaybe 1e100 (CE.liqCredit (liqMap Map.! liqName))])
evalExtraSupportBalance d t (W.MultiSupport supports) 
  = sum <$> (sequenceA [ (evalExtraSupportBalance d t sp) | sp <- supports ])


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

inspectListVars :: Ast.Asset a => TestDeal a -> Date -> [DealStats] -> Either String [ResultComponent]
inspectListVars t d dss = sequenceA [ inspectVars t d ds | ds <- dss]                     

inspectVars :: Ast.Asset a => TestDeal a -> Date -> DealStats -> Either String ResultComponent
inspectVars t d ds =                     
  case getDealStatType ds of 
    RtnRate -> do 
                 q <- queryCompound t d (patchDateToStats d ds)
                 return $ InspectRate d ds $ fromRational q
    RtnBool -> do 
                 q <- queryDealBool t (patchDateToStats d ds) d
                 return $ InspectBool d ds q 
    RtnInt  -> do 
                 q <- queryCompound t d (patchDateToStats d ds)
                 return $ InspectInt d ds $ round . fromRational $ q
    _       -> do 
                 q <- queryCompound t d (patchDateToStats d ds)
                 return $ InspectBal d ds $ fromRational q 

showInspection :: ResultComponent -> String
showInspection (InspectRate d ds r) = show r
showInspection (InspectBool d ds r) = show r
showInspection (InspectInt d ds r) = show r
showInspection (InspectBal d ds r) = show r
showInspection x = error $ "not implemented for showing ResultComponent " ++ show x


calcAvailFund :: Ast.Asset a => TestDeal a -> Date -> A.Account -> Maybe W.ExtraSupport -> Either String Balance
calcAvailFund t d acc Nothing = Right $ A.accBalance acc
calcAvailFund t d acc (Just support) = ((A.accBalance acc) +) <$> evalExtraSupportBalance d t support

-- ^ Deal, Date , cap balance, due balance
applyLimit :: Ast.Asset a => TestDeal a -> Date -> Balance -> Balance -> Maybe Limit -> Balance
applyLimit t d availBal dueBal Nothing = availBal
applyLimit t d availBal dueBal (Just limit) = 
    case limit of 
      DueCapAmt amt -> min amt availBal
      DS ds -> min (queryDeal t (patchDateToStats d ds)) availBal
      DuePct pct -> min availBal $ mulBR dueBal pct 

calcAvailAfterLimit :: Ast.Asset a => TestDeal a -> Date -> A.Account -> Maybe W.ExtraSupport 
                    -> Balance -> (Maybe Limit) -> Either String Balance
calcAvailAfterLimit t d acc mSupport dueAmt mLimit 
  = let 
      availFund = case mSupport of 
                    Nothing -> Right $ A.accBalance acc
                    Just support -> ((A.accBalance acc) +) <$> evalExtraSupportBalance d t support
    in
      (min dueAmt) <$> 
      case mLimit of
        Nothing -> availFund
        Just (DueCapAmt amt) -> min amt <$> availFund
        Just (DS ds) -> liftA2 min (fromRational <$> (queryCompound t d (patchDateToStats d ds))) availFund
        Just (DuePct pct) -> min (mulBR dueAmt pct) <$> availFund 
        _ -> Left ("Failed to find <limit> type"++ show mLimit)


updateSupport :: Ast.Asset a => Date -> Maybe W.ExtraSupport -> Balance -> TestDeal a -> TestDeal a
updateSupport _ Nothing _ t = t
updateSupport d (Just support) bal t = fst $ drawExtraSupport d bal support t

performActionWrap :: Ast.Asset a => Date -> (TestDeal a, RunContext a, [ResultComponent]) 
                  -> W.Action -> Either String (TestDeal a, RunContext a, [ResultComponent])

performActionWrap d (t, rc, logs) (W.BuyAsset ml pricingMethod accName pId) 
   = performActionWrap d (t, rc, logs) (W.BuyAssetFrom ml pricingMethod accName (Just "Consol") pId)

performActionWrap d 
                  (t@TestDeal{ accounts = accsMap , pool = pt}
                  ,rc@RunContext{runPoolFlow=pFlowMap
                                ,revolvingAssump=Just rMap
                                ,revolvingInterestRateAssump = mRates}
                  ,logs)
                  (W.BuyAssetFrom ml pricingMethod accName mRevolvingPoolName  pId) 
   = 
    let 
      revolvingPoolName = fromMaybe "Consol" mRevolvingPoolName
      (assetForSale::RevolvingPool, perfAssumps::AP.ApplyAssumptionType) =  rMap Map.! revolvingPoolName  -- `debug` ("Getting pool"++ revolvingPoolName) 

      _assets = lookupAssetAvailable assetForSale d
      assets = updateOriginDate2 d <$> _assets  -- `debug` ("Asset on revolv"++ show _assets)
                
      accBal = A.accBalance $ accsMap Map.! accName 
      pIdToChange = fromMaybe PoolConsol pId --`debug` ("purchase date"++ show d++ "\n" ++ show assetBought)
      
    in
      do
        limitAmt <- case ml of 
                      Just (DS ds) -> queryCompound t d (patchDateToStats d ds)
                      Just (DueCapAmt amt) -> Right (toRational amt)
                      Nothing -> Right (toRational accBal)
        let availBal = min (fromRational limitAmt) accBal  -- `debug` ("Date"++ show d ++" Value on r -asset "++ show valuationOnAvailableAssets)
        valOnAvailableAssets <- priceAssetUnionList assets d pricingMethod perfAssumps mRates 
        let valuationOnAvailableAssets = sum $ getPriceValue <$> valOnAvailableAssets
        let purchaseAmt = case assetForSale of 
                            (StaticAsset _) -> min availBal valuationOnAvailableAssets -- `debug` ("Valuation on rpool"++show valuationOnAvailableAssets)
                            ConstantAsset _ -> availBal 
                            AssetCurve _ -> min availBal valuationOnAvailableAssets   
        let purchaseRatio = divideBB purchaseAmt valuationOnAvailableAssets -- `debug` ("Date"++ show d ++ " Purchase Amt"++show purchaseAmt++">> avail balance"++ show availBal )
        let (assetBought,poolAfterBought) = buyRevolvingPool d (toRational purchaseRatio) assetForSale  -- `debug` ("date "++ show d ++ "purchase ratio"++ show purchaseRatio)
        let boughtAssetBal =  sum $ curBal <$> assetBought -- `debug` ("Asset bought 0 \n"++ show assetBought++ "pflow map\n"++ show pFlowMap++" p id to change\n"++ show pIdToChange)
        -- update runtime balance
        let newPt = case pt of 
                      MultiPool pm -> MultiPool $ Map.adjust
                                                    (over P.poolIssuanceStat (Map.adjust (+ boughtAssetBal) RuntimeCurrentPoolBalance))  
                                                    pIdToChange
                                                    pm
                      ResecDeal _ -> error "Not implement on buy resec deal"

        let newAccMap = Map.adjust (A.draw purchaseAmt d (PurchaseAsset revolvingPoolName boughtAssetBal)) accName accsMap -- `debug` ("Asset bought total bal"++ show boughtAssetBal)
        cfFrameBought <- projAssetUnionList [updateOriginDate2 d ast | ast <- assetBought ] d perfAssumps mRates  -- `debug` ("Date: " ++ show d ++ "Asset bought"++ show [updateOriginDate2 d ast | ast <- assetBought ])
        let cfBought = fst cfFrameBought 
        let newPcf = Map.adjust (\cfOrigin@(CF.CashFlowFrame st trs) -> 
                                let 
                                  dsInterval = getDate <$> trs  --  `debug` ("Date"++ show d ++ "origin cf \n"++ show cfOrigin)
                                  boughtCfDates = getDate <$> view CF.cashflowTxn cfBought -- `debug` ("Date"++ show d++ "Cf bought 0\n"++ show cfBought)

                                  newAggDates = case (dsInterval,boughtCfDates) of 
                                                  ([],[]) -> []
                                                  (_,[]) -> [] -- `debug` ("hit with non cash date from bought"++ show dsInterval) 
                                                  ([],_) -> boughtCfDates
                                                  (oDs,bDs) -> 
                                                    let 
                                                      lastOdate = last oDs
                                                      lastBdate = last bDs
                                                    in 
                                                      if lastOdate > lastBdate then 
                                                        []
                                                      else 
                                                        sliceDates (SliceAfter lastOdate) bDs

                                  mergedCf = CF.mergePoolCf2 cfOrigin cfBought -- `debug` ("Buy Date : "++show d ++ "CF bought \n"++ show (over CF.cashflowTxn (slice 0 30) cfBought) )
                                in 
                                  over CF.cashflowTxn (`CF.aggTsByDates` (dsInterval ++ newAggDates)) mergedCf ) -- `debug` ("Date "++show d++" Merged CF\n"++ show mergedCf))
                            pIdToChange
                            pFlowMap --  `debug` ("pid To change"++ show pIdToChange++ "P flow map"++ show pFlowMap)

        let newRc = rc {runPoolFlow = newPcf -- `debug` (show d ++ "New run pool >> \n"++ show newPcf)
                        ,revolvingAssump = Just (Map.insert revolvingPoolName (poolAfterBought, perfAssumps) rMap)} 
        return (t { accounts = newAccMap , pool = newPt}, newRc, logs )

performActionWrap d 
                  (t
                  ,rc@RunContext{runPoolFlow=pcf
                                ,revolvingAssump=Nothing
                                ,revolvingInterestRateAssump=mRates}
                  ,logs)
                  (W.BuyAsset ml pricingMethod accName _)
  = Left $ "Missing revolving Assumption(asset assumption & asset to buy)" ++ name t

-- TODO need to set a limit to sell
performActionWrap d 
                  (t@TestDeal{accounts = accMap, pool = pt}  
                  ,rc@RunContext{runPoolFlow = pcf}
                  ,logs)
                  (W.LiquidatePool lm an mPid)
 = let
     liqFunction = \(p@P.Pool{ P.issuanceStat = m} ) 
                     -> over (P.poolFutureScheduleCf . _Just) (CF.extendCashFlow d) $ 
                        over (P.poolFutureCf . _Just) (CF.extendCashFlow d) $ 
                        p { P.issuanceStat = Just (Map.insert RuntimeCurrentPoolBalance 0 (fromMaybe Map.empty m)) }

     poolMapToLiq = case (pt, mPid) of 
                      (MultiPool pm, Nothing) -> pm
                      (MultiPool pm,Just pids) -> let
                                                   selectedPids = S.fromList pids
                                                   selectedPoolMap = Map.filterWithKey (\k v -> S.member k selectedPids) pm
                                                  in 
                                                   selectedPoolMap
                      (ResecDeal _,_) -> error "Not implement on liquidate resec deal"
     


     liqAmtByPool = Map.mapWithKey (\k p -> P.pricingPoolFlow d p (pcf Map.! k) lm) poolMapToLiq -- `debug` ("pool id to liq"++ show poolMapToLiq)
     
     liqAmt = sum $ Map.elems liqAmtByPool

     -- Update collected cashflow
     newPt = case (pt, mPid) of 
               (MultiPool pm, Nothing) -> MultiPool $ Map.map liqFunction pm
               (MultiPool pm, Just pids) -> let
                                            selectedPids = S.fromList pids
                                            selectedPoolMap = Map.filterWithKey (\k v -> S.member k selectedPids) pm
                                           in 
                                            MultiPool $ Map.union (Map.map liqFunction selectedPoolMap) pm
               (ResecDeal _,_) -> error "Not implement on liquidate resec deal"

     liqComment = LiquidationProceeds (fromMaybe [] mPid)
     accMapAfterLiq = Map.adjust (A.deposit liqAmt d liqComment) an accMap
     -- REMOVE future cf
     newPfInRc = foldr (Map.adjust (set CF.cashflowTxn [])) pcf  (Map.keys poolMapToLiq)
   in 
     Right (t {accounts = accMapAfterLiq , pool = newPt} , rc {runPoolFlow = newPfInRc}, logs )


performActionWrap d (t, rc, logs) (W.WatchVal ms dss)
  = (inspectListVars t d dss) >>= (\vs -> Right (t, rc, logs ++ [InspectWaterfall d ms dss (showInspection <$> vs)])) 
      -- vals <- sequenceA $ showInspection (inspectListVars t d dss)

performActionWrap d (t, rc, logs) (W.ActionWithPre p actions) 
  = do 
      flag <- testPre d t p 
      if flag then 
        foldM (performActionWrap d) (t,rc,logs) actions
      else
        return (t, rc, logs)
    

performActionWrap d (t, rc, logs) (W.ActionWithPre2 p actionsTrue actionsFalse) 
  = do 
      flag <- testPre d t p
      if flag then
        foldM (performActionWrap d) (t,rc,logs) actionsTrue
      else
        foldM (performActionWrap d) (t,rc,logs) actionsFalse

-- ^ go down to performAction
performActionWrap d (t, rc, logs) a 
  = do 
      dealAfterExe <- performAction d t a 
      return (dealAfterExe, rc, logs)

-- bookableAmount :: Ast.Asset a => TestDeal a -> Date -> LD.LedgerName -> Amount -> Either String Balance
-- bookableAmount t@TestDeal{ledgers = Just ledgerM} d lName amt 
--   = let 
--       ldg = ledgerM Map.! lName
--     in 
--       case (LD.ledgCap ldg) of
--         Nothing -> Right amt
--         Just ds -> do 
--                      bookable <- fromRational $ queryCompound t d ds
--                      return (min bookable amt)

-- bookLedgersBySeq :: Ast.Asset a => TestDeal a -> Date -> Amount -> BookDirection -> Maybe TxnComment -> [LD.LedgerName] -> Map.Map LD.LedgerName LD.Ledger -> Map.Map LD.LedgerName LD.Ledger
-- bookLedgersBySeq d amt dr lNames mComment lMap 
--   = 


performAction :: Ast.Asset a => Date -> TestDeal a -> W.Action -> Either String (TestDeal a)
performAction d t@TestDeal{accounts=accMap, ledgers = Just ledgerM} 
                (W.TransferAndBook mLimit an1 an2 (dr, lName) mComment)
  = let
      sourceAcc = accMap Map.! an1
      targetAcc = accMap Map.! an2 
      actualPaidOut = calcAvailAfterLimit t d (accMap Map.! an1) Nothing (A.accBalance sourceAcc) mLimit
    in 
      do 
        transferAmt <- actualPaidOut
        let accMapAfterDraw = Map.adjust (A.draw transferAmt d (TxnComments [Transfer an1 an2,(BookLedgerBy dr lName)])) an1 accMap -- `debug` (">>PDL >>Ledger bal"++show d ++ show targetAmt)
        let accMapAfterDeposit = Map.adjust (A.deposit transferAmt d (TxnComments [Transfer an1 an2,(BookLedgerBy dr lName)])) an2 accMapAfterDraw
        let newLedgerM = Map.adjust (LD.entryLog transferAmt d (TxnDirection dr)) lName ledgerM
        return t {accounts = accMapAfterDeposit, ledgers = Just newLedgerM}  

performAction d t@TestDeal{accounts=accMap} (W.Transfer mLimit an1 an2 mComment)
  = let
      sourceAcc = accMap Map.! an1
      targetAcc = accMap Map.! an2 
      actualPaidOut = calcAvailAfterLimit t d (accMap Map.! an1) Nothing (A.accBalance sourceAcc) mLimit
    in 
      do 
        transferAmt <- actualPaidOut
        let accMapAfterDraw = Map.adjust (A.draw transferAmt d (Transfer an1 an2)) an1 accMap -- `debug` (">>PDL >>Ledger bal"++show d ++ show targetAmt)
        let accMapAfterDeposit = Map.adjust (A.deposit transferAmt d (Transfer an1 an2)) an2 accMapAfterDraw
        return t {accounts = accMapAfterDeposit}  

-- foldM :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b

performAction d t@TestDeal{accounts=accMap} (W.TransferMultiple sourceAccList targetAcc mComment)
  = foldM (\acc (mLimit, sourceAccName) -> 
            performAction d acc (W.Transfer mLimit sourceAccName targetAcc mComment))
          t
          sourceAccList  

-- ^ book ledger 
performAction d t@TestDeal{ledgers= Just ledgerM} (W.BookBy (W.ByDS ledger dr ds)) =
  do
    amtToBook <- queryCompound t d ds
    let newLedgerM = Map.adjust (LD.entryLogByDr dr (fromRational amtToBook) d (TxnDirection dr)) ledger ledgerM
    return $ t {ledgers = Just newLedgerM } 

-- ^ it will book ledgers by order with mandatory caps which describes by a <formula> 
-- ^ ds -> value to book 
-- ^ ledgersList -> list of ledgers to book 
performAction d t@TestDeal{ledgers= Just ledgerM} (W.BookBy (W.PDL dr ds ledgersList)) =
  let
    ledgerCaps = sequenceA [ queryCompound t d ledgerCap | ledgerCap <- snd <$> ledgersList ]
    ledgerNames = fst <$> ledgersList
  in 
    do
      amtToBook <- queryCompound t d ds
      ledgCaps <- ledgerCaps
      let amtBookedToLedgers = paySeqLiabilitiesAmt (fromRational amtToBook) (fromRational <$> ledgCaps)
      let newLedgerM = foldr 
                         (\(ln,amt) acc -> Map.adjust (LD.entryLogByDr dr amt d (TxnDirection dr)) ln acc)
                         ledgerM
                         (zip ledgerNames amtBookedToLedgers)
      return $ t {ledgers = Just newLedgerM}

-- ^ pay fee sequentially
performAction d t@TestDeal{fees=feeMap, accounts=accMap} (W.PayFeeBySeq mLimit an fns mSupport) =
  let 
    availAccBal = A.accBalance (accMap Map.! an)
    feesToPay = map (feeMap Map.!) fns
    totalFeeDue = sum $ map F.feeDue feesToPay
    actualPaidOut = calcAvailAfterLimit t d (accMap Map.! an) mSupport totalFeeDue mLimit
  in
    do 
      paidOutAmt <- actualPaidOut
      let (feesPaid, remainAmt) = paySequentially d paidOutAmt F.feeDue (F.payFee d) [] feesToPay
      let accPaidOut = min availAccBal paidOutAmt
    
      let dealAfterAcc = t {accounts = Map.adjust (A.draw accPaidOut d (SeqPayFee fns)) an accMap
                           ,fees = Map.fromList (zip fns feesPaid) <> feeMap}

      let supportPaidOut = paidOutAmt - accPaidOut
      return $ updateSupport d mSupport supportPaidOut dealAfterAcc
    
-- ^ pay out fee in pro-rata fashion
performAction d t@TestDeal{fees=feeMap, accounts=accMap} (W.PayFee mLimit an fns mSupport) =
  let 
    availAccBal = A.accBalance (accMap Map.! an)
    feesToPay = map (feeMap Map.!) fns
    totalFeeDue = sum $ map F.feeDue feesToPay
    actualPaidOut = calcAvailAfterLimit t d (accMap Map.! an) mSupport totalFeeDue mLimit
  in
    do 
      paidOutAmt <- actualPaidOut
      let (feesPaid, remainAmt) = payProRata d paidOutAmt F.feeDue (F.payFee d) feesToPay
      let accPaidOut = min availAccBal paidOutAmt
    
      let dealAfterAcc = t {accounts = Map.adjust (A.draw accPaidOut d (SeqPayFee fns)) an accMap
                           ,fees = Map.fromList (zip fns feesPaid) <> feeMap}

      let supportPaidOut = paidOutAmt - accPaidOut
      return $ updateSupport d mSupport supportPaidOut dealAfterAcc


performAction d t (W.AccrueAndPayIntBySeq mLimit an bnds mSupport)
  = do
      dealWithBondDue <- performAction d t (W.CalcBondInt bnds Nothing Nothing)
      performAction d dealWithBondDue (W.PayIntBySeq mLimit an bnds mSupport)

performAction d t@TestDeal{bonds=bndMap, accounts=accMap, liqProvider=liqMap} 
                (W.PayIntOverIntBySeq mLimit an bnds mSupport)
  = let 
      availAccBal = A.accBalance (accMap Map.! an)
      bndsList = (Map.!) bndMap <$> bnds
      dueAmts = L.bndDueIntOverInt <$> bndsList
      totalDue = sum dueAmts
      actualPaidOut = calcAvailAfterLimit t d (accMap Map.! an) mSupport totalDue mLimit
    in
      do 
        paidOutAmt <- actualPaidOut
        let (bondsPaid, remainAmt) = paySequentially d paidOutAmt L.bndDueIntOverInt (L.payInt d) [] bndsList
        let accPaidOut = min availAccBal paidOutAmt
      
        let dealAfterAcc = t {accounts = Map.adjust (A.draw accPaidOut d (PayInt bnds)) an accMap
                             ,bonds = Map.fromList (zip bnds bondsPaid) <> bndMap}

        let supportPaidOut = paidOutAmt - accPaidOut
        return $ updateSupport d mSupport supportPaidOut dealAfterAcc


performAction d t@TestDeal{bonds=bndMap, accounts=accMap, liqProvider=liqMap} (W.PayIntBySeq mLimit an bnds mSupport)
   = let 
      availAccBal = A.accBalance (accMap Map.! an)
      bndsList = (Map.!) bndMap <$> bnds
      dueAmts = L.bndDueInt <$> bndsList
      totalDue = sum dueAmts
      actualPaidOut = calcAvailAfterLimit t d (accMap Map.! an) mSupport totalDue mLimit
    in
      do 
        paidOutAmt <- actualPaidOut
        let (bondsPaid, remainAmt) = paySequentially d paidOutAmt L.bndDueInt (L.payInt d) [] bndsList
        let accPaidOut = min availAccBal paidOutAmt
      
        let dealAfterAcc = t {accounts = Map.adjust (A.draw accPaidOut d (PayInt bnds)) an accMap
                             ,bonds = Map.fromList (zip bnds bondsPaid) <> bndMap}

        let supportPaidOut = paidOutAmt - accPaidOut
        return $ updateSupport d mSupport supportPaidOut dealAfterAcc


performAction d t@TestDeal{bonds=bndMap,accounts=accMap} (W.PayIntOverInt mLimit an bnds mSupport)
   = let 
       availAccBal = A.accBalance (accMap Map.! an)
       bndsList = (Map.!) bndMap <$> bnds
       dueAmts = L.bndDueIntOverInt <$> bndsList
       totalDue = sum dueAmts
       actualPaidOut = calcAvailAfterLimit t d (accMap Map.! an) mSupport totalDue mLimit
     in
       do
         paidOutAmt <- actualPaidOut
         let (bondsPaid, remainAmt) = payProRata d paidOutAmt L.bndDueIntOverInt (L.payInt d) bndsList
         let accPaidOut = min availAccBal paidOutAmt
       
         let dealAfterAcc = t {accounts = Map.adjust (A.draw accPaidOut d (PayInt bnds)) an accMap
                              ,bonds = Map.fromList (zip bnds bondsPaid) <> bndMap}

         let supportPaidOut = paidOutAmt - accPaidOut
         return $ updateSupport d mSupport supportPaidOut dealAfterAcc

performAction d t@TestDeal{bonds=bndMap,accounts=accMap} (W.PayInt mLimit an bnds mSupport)
  = let 
     availAccBal = A.accBalance (accMap Map.! an)
     bndsList = (Map.!) bndMap <$> bnds
     dueAmts = L.bndDueInt <$> bndsList
     totalDue = sum dueAmts
     actualPaidOut = calcAvailAfterLimit t d (accMap Map.! an) mSupport totalDue mLimit
   in
     do
       paidOutAmt <- actualPaidOut
       let (bondsPaid, remainAmt) = payProRata d paidOutAmt L.bndDueInt (L.payInt d) bndsList
       let accPaidOut = (min availAccBal paidOutAmt)
     
       let dealAfterAcc = t {accounts = Map.adjust (A.draw accPaidOut d (PayInt bnds)) an accMap
                            ,bonds = Map.fromList (zip bnds bondsPaid) <> bndMap}

       let supportPaidOut = paidOutAmt - accPaidOut
       return $ updateSupport d mSupport supportPaidOut dealAfterAcc

performAction d t (W.AccrueAndPayInt mLimit an bnds mSupport) =
  do
    dealWithBondDue <- performAction d t (W.CalcBondInt bnds Nothing Nothing)
    performAction d dealWithBondDue (W.PayInt mLimit an bnds mSupport)

performAction d t (W.CalcAndPayFee mLimit ans fees mSupport) =
  do
    dealWithFeeDue <- performAction d t (W.CalcFee fees)
    performAction d dealWithFeeDue (W.PayFee mLimit ans fees mSupport)

performAction d t@TestDeal{bonds=bndMap,accounts=accMap} (W.PayIntResidual mLimit an bndName) =
  let 
    availBal = A.accBalance $ accMap Map.! an
  in
    do 
      let limitAmt = applyLimit t d availBal availBal mLimit
      return $ t {accounts = Map.adjust (A.draw limitAmt d (PayYield bndName)) an accMap
                 , bonds = Map.adjust (L.payYield d limitAmt) bndName bndMap}

performAction d t@TestDeal{fees=feeMap,accounts=accMap} (W.PayFeeResidual mlimit an feeName) =
  let
    availBal = A.accBalance $ accMap Map.! an
    paidOutAmt = applyLimit t d availBal availBal mlimit
    accMapAfterPay = Map.adjust (A.draw paidOutAmt d (PayFeeYield feeName)) an accMap
    feeMapAfterPay = Map.adjust (F.payResidualFee d paidOutAmt) feeName feeMap
  in 
    Right $ t {accounts = accMapAfterPay, fees = feeMapAfterPay}

performAction d t@TestDeal{bonds=bndMap,accounts=accMap} 
                (W.PayPrinBySeq mLimit an bnds mSupport) 
  = let 
     availAccBal = A.accBalance (accMap Map.! an)
     bndsList = (Map.!) bndMap <$> bnds
     bndsToPay = filter (not . L.isPaidOff) bndsList
     bndsToPayNames = L.bndName <$> bndsToPay
     bndsWithDue = calcDuePrin t d <$> bndsToPay
     bndsDueAmts = L.bndDuePrin <$> bndsWithDue
     totalDue = sum bndsDueAmts
     actualPaidOut = calcAvailAfterLimit t d (accMap Map.! an) mSupport totalDue mLimit
   in
     do
       paidOutAmt <- actualPaidOut
       let (bondsPaid, remainAmt) = paySequentially d paidOutAmt L.bndDuePrin (L.payPrin d) [] bndsToPay
       let accPaidOut = min availAccBal paidOutAmt
     
       let dealAfterAcc = t {accounts = Map.adjust (A.draw accPaidOut d (PayPrin bndsToPayNames)) an accMap
                            ,bonds = Map.fromList (zip bndsToPayNames bondsPaid) <> bndMap}

       let supportPaidOut = paidOutAmt - accPaidOut
       return $ updateSupport d mSupport supportPaidOut dealAfterAcc

performAction d t@TestDeal{bonds=bndMap,accounts=accMap} 
                (W.PayPrinGroup mLimit an bndGrpName by mSupport) 
  = let 
     availAccBal = A.accBalance (accMap Map.! an)
     L.BondGroup bndsMap = bndMap Map.! bndGrpName
     bndsToPay = Map.filter (not . L.isPaidOff) bndsMap
     bndsToPayNames = L.bndName <$> Map.elems bndsToPay
     bndsWithDueMap = Map.map (calcDuePrin t d) bndsToPay
     bndsDueAmtsMap = Map.map (\x -> (x, L.bndDuePrin x)) bndsWithDueMap
     totalDue = sum $ snd <$> Map.elems bndsDueAmtsMap -- `debug` (">date"++show d++" due amt"++show bndsDueAmtsMap)
     actualPaidOut = calcAvailAfterLimit t d (accMap Map.! an) mSupport totalDue mLimit
   in
     do
       paidOutAmt <- actualPaidOut

       let payOutPlan = allocAmtToBonds by paidOutAmt (Map.elems bndsDueAmtsMap) -- `debug` (">date"++ show payAmount)
       let payOutPlanWithBondName = [ (L.bndName bnd,amt) | (bnd,amt) <- payOutPlan] -- `debug` (">date"++show d++"payOutPlan"++ show payOutPlan)

       let bndMapAfterPay = foldr 
                              (\(bndName, _amt) acc -> Map.adjust (L.payPrin d _amt) bndName acc)
                              bndsMap
                              payOutPlanWithBondName -- `debug` (">date"++show d++"payoutPlan"++ show payOutPlanWithBondName)
       let accPaidOut = min availAccBal paidOutAmt
     
       let dealAfterAcc = t {accounts = Map.adjust (A.draw accPaidOut d (PayGroupPrin bndsToPayNames)) an accMap
                            ,bonds = Map.insert bndGrpName (L.BondGroup  bndMapAfterPay) bndMap}

       let supportPaidOut = paidOutAmt - accPaidOut
       return $ updateSupport d mSupport supportPaidOut dealAfterAcc


performAction d t@TestDeal{bonds=bndMap} (W.AccrueAndPayIntGroup mLimit an bndName by mSupport)
  = do 
      dAfterAcc <- performAction d t (W.AccrueIntGroup [bndName])-- `debug` ("Acc due int grp"++ show (getDueInt (bndMap Map.! bndName)))
      performAction d dAfterAcc (W.PayIntGroup mLimit an bndName by mSupport)


performAction d t@TestDeal{bonds=bndMap} (W.AccrueIntGroup bndNames)
  = do 
      let bondGrp = Map.filterWithKey (\k _ -> S.member k (S.fromList bndNames)) bndMap
      bondGrpAccrued <- mapM (calcDueInt t d Nothing Nothing) bondGrp
      return t {bonds = bondGrpAccrued <> bndMap}


performAction d t@TestDeal{bonds=bndMap,accounts=accMap} (W.PayIntGroup mLimit an bndGrpName by mSupport)
  = let 
     availAccBal = A.accBalance (accMap Map.! an)
     L.BondGroup bndsMap = bndMap Map.! bndGrpName
     bndsToPay = Map.filter (not . L.isPaidOff) bndsMap
     bndsToPayNames = L.bndName <$> Map.elems bndsToPay
   in
     do
       bndsWithDueMap <- mapM (calcDueInt t d Nothing Nothing) bndsToPay
       let bndsDueAmtsMap = Map.map (\x -> (x, L.totalDueInt x)) bndsWithDueMap
       let totalDue = sum $ snd <$> Map.elems bndsDueAmtsMap -- `debug` (">date"++show d++" due amt"++show bndsDueAmtsMap)
       let actualPaidOut = calcAvailAfterLimit t d (accMap Map.! an) mSupport totalDue mLimit
       paidOutAmt <- actualPaidOut

       let payOutPlan = allocAmtToBonds by paidOutAmt (Map.elems bndsDueAmtsMap) -- `debug` (">date"++ show payAmount)
       let payOutPlanWithBondName = [ (L.bndName bnd,amt) | (bnd,amt) <- payOutPlan] -- `debug` (">date"++show d++"payOutPlan"++ show payOutPlan)

       let bndMapAfterPay = foldr 
                              (\(bndName, _amt) acc -> Map.adjust (L.payInt d _amt) bndName acc)
                              bndsMap
                              payOutPlanWithBondName -- `debug` (">date"++show d++"payoutPlan"++ show payOutPlanWithBondName)
       let accPaidOut = (min availAccBal paidOutAmt)
     
       let dealAfterAcc = t {accounts = Map.adjust (A.draw accPaidOut d (PayGroupInt bndsToPayNames)) an accMap
                            ,bonds = Map.insert bndGrpName (L.BondGroup  bndMapAfterPay) bndMap}

       let supportPaidOut = paidOutAmt - accPaidOut
       return $ updateSupport d mSupport supportPaidOut dealAfterAcc


performAction d t@TestDeal{bonds=bndMap,accounts=accMap} (W.PayPrinWithDue an bnds Nothing) 
  = Right $ t {accounts = accMapAfterPay, bonds = bndMapUpdated}
    where
      acc = accMap Map.! an
      availBal = A.accBalance acc
      bndsToPay = getActiveBonds t bnds
      bndsToPayNames = L.bndName <$> bndsToPay
      bndsDueAmts = L.bndDuePrin <$> bndsToPay
      actualPaidOut = min availBal $ sum bndsDueAmts

      (bndsPaid, remainAmt) = payProRata d actualPaidOut L.bndDuePrin (L.payPrin d) bndsToPay
      
      bndMapUpdated = (Map.fromList $ zip bndsToPayNames bndsPaid) <> bndMap
      accMapAfterPay = Map.adjust (A.draw actualPaidOut d (PayPrin bnds)) an accMap


performAction d t@TestDeal{bonds=bndMap,accounts=accMap} (W.PayPrin mLimit an bnds mSupport)
  = let 
     availAccBal = A.accBalance (accMap Map.! an)
     bndsToPay = getActiveBonds t bnds
     
     bndsWithDue = calcDuePrin t d <$> bndsToPay
     bndsDueAmts = L.bndDuePrin <$> bndsWithDue
     
     bndsToPayNames = L.bndName <$> bndsWithDue
     totalDue = sum bndsDueAmts
     actualPaidOut = calcAvailAfterLimit t d (accMap Map.! an) mSupport totalDue mLimit
   in
     do
       paidOutAmt <- actualPaidOut
       let (bondsPaid, remainAmt) = payProRata d paidOutAmt L.bndDuePrin (L.payPrin d) bndsWithDue
       let accPaidOut = min availAccBal paidOutAmt
     
       let dealAfterAcc = t {accounts = Map.adjust (A.draw accPaidOut d (PayPrin bndsToPayNames)) an accMap
                            ,bonds = Map.fromList (zip bndsToPayNames bondsPaid) <> bndMap}

       let supportPaidOut = paidOutAmt - accPaidOut
       return $ updateSupport d mSupport supportPaidOut dealAfterAcc

-- ^ pay principal without any limit
performAction d t@TestDeal{accounts=accMap, bonds=bndMap} (W.PayPrinResidual an bnds) = 
  Right $ t {accounts = accMapAfterPay, bonds = bndMapUpdated} -- `debug` ("Bond Prin Pay Result"++show(bndMapUpdated))
  where
    acc = accMap Map.! an

    bndsToPay = getActiveBonds t bnds
    bndsToPayNames = L.bndName <$> bndsToPay
    availBal = A.accBalance acc
    bndsDueAmts = map L.getCurBalance bndsToPay

    actualPaidOut = min availBal $ sum bndsDueAmts -- `debug` ("bonds totoal due ->"++show(bndsDueAmts))
    bndsAmountToBePaid = zip bndsToPay (prorataFactors bndsDueAmts actualPaidOut)
    bndsPaid = map (\(l,amt) -> L.payPrin d amt l) bndsAmountToBePaid  -- `debug` ("pay bonds "++show bnds ++"pay prin->>>To"++show(prorataFactors bndsDueAmts availBal))

    bndMapUpdated =  (Map.fromList $ zip bndsToPayNames bndsPaid) <> bndMap
    accMapAfterPay = Map.adjust (A.draw actualPaidOut d (PayPrin bnds)) an accMap

performAction d t@TestDeal{accounts=accMap, bonds=bndMap} (W.FundWith mlimit an bnd) = 
  do
    fundAmt_ <- case mlimit of 
                 Just (DS ds) -> queryCompound t d (patchDateToStats d ds)
                 Just (DueCapAmt amt) -> Right $ toRational amt
                 _ -> Left $ "Not valid limit for funding with bond"++ show bnd
    let fundAmt = fromRational fundAmt_
    let accMapAfterFund = Map.adjust (A.deposit fundAmt d (FundWith bnd fundAmt)) an accMap
    newBnd <- calcDueInt t d Nothing Nothing $ bndMap Map.! bnd
    let bndFunded = L.fundWith d fundAmt newBnd
    return $ t {accounts = accMapAfterFund, bonds= Map.fromList [(bnd,bndFunded)] <> bndMap } 

-- ^ write off bonds and book 
performAction d t@TestDeal{bonds = bndMap, ledgers = Just ledgerM } 
              (W.WriteOffAndBook mLimit bnd (dr,lName))
  = let 
      bndToWriteOff = bndMap Map.! bnd
      bndBal = L.bndBalance bndToWriteOff
      writeAmt = applyLimit t d bndBal bndBal mLimit
      newLedgerM = Map.adjust (LD.entryLogByDr dr writeAmt d (WriteOff bnd writeAmt)) lName ledgerM
    in
      do 
        newBnd <- calcDueInt t d Nothing Nothing bndToWriteOff
        let bndWritedOff = L.writeOff d writeAmt newBnd
        return $ t {bonds = Map.fromList [(bnd,bndWritedOff)] <> bndMap, ledgers = Just newLedgerM}

performAction d t@TestDeal{bonds=bndMap} (W.WriteOff mlimit bnd)
  = do 
      writeAmt <- case mlimit of
                    Just (DS ds) -> queryCompound t d (patchDateToStats d ds)
                    Just (DueCapAmt amt) -> Right $ toRational amt
                    Nothing -> Right $ toRational . L.bndBalance $ bndMap Map.! bnd
                    x -> Left $ "not supported type to determine the amount to write off"++ show x

      let writeAmtCapped = min (fromRational writeAmt) $ L.bndBalance $ bndMap Map.! bnd
      newBnd <- calcDueInt t d Nothing Nothing $ bndMap Map.! bnd
      let bndWritedOff = L.writeOff d writeAmtCapped newBnd
      return $ t {bonds = Map.fromList [(bnd,bndWritedOff)] <> bndMap}

performAction d t@TestDeal{bonds=bndMap, ledgers = Just ledgerM} 
              (W.WriteOffBySeqAndBook mLimit bnds (dr,lName))
  = do
      bndsToWriteOff <- mapM (calcDueInt t d Nothing Nothing . (bndMap Map.!)) bnds
      let totalBondBal = sum $ L.bndBalance <$> bndsToWriteOff
      let writeAmt = applyLimit t d totalBondBal totalBondBal mLimit

      let (bndWrited, _) = paySequentially d writeAmt L.bndBalance (L.writeOff d) [] bndsToWriteOff 
      let bndMapUpdated = lstToMapByFn L.bndName bndWrited
      let newLedgerM = Map.adjust (LD.entryLogByDr dr writeAmt d (TxnDirection dr)) lName ledgerM
      return t {bonds = bndMapUpdated <> bndMap, ledgers = Just newLedgerM}


performAction d t@TestDeal{bonds=bndMap } (W.WriteOffBySeq mlimit bnds)
  = do 
      bondsToWriteOff <- mapM (calcDueInt t d Nothing Nothing . (bndMap Map.!)) bnds
      let totalBondBal = sum $ L.bndBalance <$> bondsToWriteOff
      let writeAmt = case mlimit of
                       Just (DS ds) -> queryDeal t (patchDateToStats d ds)
                       Just (DueCapAmt amt) -> amt
                       Nothing -> totalBondBal
                       x -> error $ "not supported type to determine the amount to write off"++ show x

      let writeAmtCapped = min writeAmt totalBondBal
      let (bndWrited, _) = paySequentially d writeAmtCapped L.bndBalance (L.writeOff d) [] bondsToWriteOff 
      let bndMapUpdated = lstToMapByFn L.bndName bndWrited
      return t {bonds = bndMapUpdated <> bndMap }

performAction d t@TestDeal{fees=feeMap} (W.CalcFee fns) 
  = do
      newFeeMap <- mapM (calcDueFee t d) $ getFeeByName t (Just fns)
      return t {fees = newFeeMap <> feeMap }

performAction d t@TestDeal{bonds=bndMap} (W.CalcBondInt bns mBalDs mRateDs) 
  = do 
      newBondMap <- mapM (calcDueInt t d mBalDs mRateDs) $ getBondsByName t (Just bns)
      return t {bonds = newBondMap <> bndMap}

-- ^ set due prin mannually
performAction d t@TestDeal{bonds=bndMap} (W.CalcBondPrin2 mLimit bnds) 
  = Right $ t {bonds = newBndMap} -- `debug` ("New map after calc due"++ show (Map.mapWithKey (\k v -> (k, L.bndDuePrin v)) newBndMap))
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

performAction d t@TestDeal{bonds=bndMap, accounts = accMap} (W.CalcBondPrin mLimit accName bnds mSupport) 
  = do 
      availBal <- calcAvailFund t d (accMap Map.! accName) mSupport
      let limitCap = applyLimit t d availBal (sum bndsDueAmts) mLimit
      let payAmount = min limitCap availBal 
      let bndsAmountToBePaid = zip bndsToPayNames $ prorataFactors bndsDueAmts payAmount  -- (bond, amt-allocated)
      let newBndMap = foldr 
                        (\(bn,amt) acc -> Map.adjust (\b -> b {L.bndDuePrin = amt})  bn acc) 
                        bndMap 
                        bndsAmountToBePaid -- `debug` ("Calc Bond Prin"++ show bndsAmountToBePaid)
      return $ t {bonds = newBndMap}
    where 
      accBal = A.accBalance $ accMap Map.! accName
      bndsToPay = filter (not . L.isPaidOff) $ map (bndMap Map.!) bnds
      bndsToPayNames = L.bndName <$> bndsToPay
      bndsDueAmts = L.bndDuePrin . calcDuePrin t d <$> bndsToPay
      
      
-- ^ draw cash and deposit to account
performAction d t@TestDeal{accounts=accs, liqProvider = Just _liqProvider} (W.LiqSupport limit pName CE.LiqToAcc an)
  =  
    do 
      _transferAmt <- case limit of 
                        Nothing -> Right 0 -- `debug` ("limit on nothing"++ show limit)
                        Just (DS ds) -> queryCompound t d (patchDateToStats d ds) -- `debug` ("hit with ds"++ show ds)
                        _ -> Left $ "Failed on <limit> passed from action : liqSupport, only formula is supported but got "++ show limit -- `debug` ("limit on last"++ show limit)
      let transferAmt = fromRational $ max 0 $
                          case CE.liqCredit $ _liqProvider Map.! pName of 
                            Nothing -> _transferAmt -- `debug` ("not loc"++ show newLiqMapUpdated)
                            Just _availBal -> min _transferAmt (toRational _availBal)  -- `debug` ("transfer amt"++ show _transferAmt ++ "loc"++ show _availBal)
      
      return t { accounts = Map.adjust (A.deposit transferAmt d (LiquidationSupport pName)) an accs
               , liqProvider = Just $ Map.adjust (CE.draw transferAmt d) pName _liqProvider
               }

performAction d t@TestDeal{fees=feeMap,liqProvider = Just _liqProvider} (W.LiqSupport limit pName CE.LiqToFee fn)
  = Right $ t { fees = newFeeMap, liqProvider = Just newLiqMap }
  where 
      _transferAmt = case limit of 
                      Nothing -> 0 
                      Just (DS (CurrentDueFee [fn])) -> queryDeal t (CurrentDueFee [fn])
                      _ -> 0

      transferAmt = case CE.liqCredit $  _liqProvider Map.! pName of 
                       Nothing -> _transferAmt
                       Just _availBal -> min _transferAmt _availBal

      newFeeMap = Map.adjust (F.payFee d transferAmt) fn feeMap
      newLiqMap = Map.adjust (CE.draw transferAmt d ) pName _liqProvider 

performAction d t@TestDeal{bonds=bndMap,liqProvider = Just _liqProvider} (W.LiqSupport limit pName CE.LiqToBondInt bn)
  = Right $ t { bonds = newBondMap, liqProvider = Just newLiqMap }
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


-- ^ payout due interest / due fee / oustanding balance to liq provider
performAction d t@TestDeal{accounts=accs,liqProvider = Just _liqProvider} (W.LiqRepay limit rpt an pName)
  = Right $ t { accounts = newAccMap, liqProvider = Just newLiqMap }
  where 
      liqDueAmts CE.LiqBal = [ CE.liqBalance $ _liqProvider Map.! pName]
      liqDueAmts CE.LiqInt =  [ CE.liqDueInt $ _liqProvider Map.! pName ]
      liqDueAmts CE.LiqPremium = [ CE.liqDuePremium $ _liqProvider Map.! pName]
      liqDueAmts (CE.LiqRepayTypes lrts) = concat $ liqDueAmts <$> lrts

      overDrawnBalance = maybe 0 negate (CE.liqCredit $ _liqProvider Map.! pName)
      
      dueBreakdown 
        | overDrawnBalance > 0 = overDrawnBalance:liqDueAmts rpt
        | otherwise = liqDueAmts rpt

      liqTotalDues = sum dueBreakdown
      
      cap = min liqTotalDues $ A.accBalance $ accs Map.! an


      transferAmt = case limit of 
                      Just (DS ds) -> min cap $ queryDeal t (patchDateToStats d ds) -- `debug` ("Cap acc"++ show cap)
                      Nothing -> cap
                      _ -> error $ "Not implement the limit"++ show limit++"For Repay to liqProvider"
      
      paidOutsToLiq = paySeqLiabilitiesAmt transferAmt dueBreakdown

      rptsToPair = case rpt of 
                     CE.LiqRepayTypes lrts -> lrts
                     x  -> [x]

      paidOutWithType
        | overDrawnBalance > 0 = zip (CE.LiqOD:rptsToPair) paidOutsToLiq 
        | otherwise = zip rptsToPair paidOutsToLiq -- `debug` ("rpts To pair"++ show rptsToPair)


      newAccMap = Map.adjust (A.draw transferAmt d (LiquidationSupport pName)) an accs -- `debug` ("repay liq amt"++ show transferAmt)
      newLiqMap = foldl
                    (\acc (_rpt,_amt) -> Map.adjust (CE.repay _amt d _rpt ) pName acc)
                    _liqProvider
                    paidOutWithType -- `debug` ("paid out"++ show paidOutWithType)

-- ^ pay yield to liq provider
performAction d t@TestDeal{accounts=accs,liqProvider = Just _liqProvider} (W.LiqYield limit an pName)
  =
    let cap = A.accBalance $ accs Map.! an in
      do 
        transferAmt <- case limit of 
                        Nothing -> Right (toRational cap)
                        Just (DS ds) -> (min (toRational cap)) <$> (queryCompound t d (patchDateToStats d ds)) 
                        _ -> Left $ "Not implement the limit"++ show limit++"For Pay Yield to liqProvider"
      
        let newAccMap = Map.adjust (A.draw (fromRational transferAmt) d (LiquidationSupport pName)) an accs
        let newLiqMap = Map.adjust (CE.repay (fromRational transferAmt) d CE.LiqResidual) pName _liqProvider 
        return t { accounts = newAccMap, liqProvider = Just newLiqMap }

performAction d t@TestDeal{liqProvider = Just _liqProvider} (W.LiqAccrue liqNames)
  = Right $ t {liqProvider = Just updatedLiqProvider}
    where 
      updatedLiqProvider = mapWithinMap ((updateLiqProvider t d) . (CE.accrueLiqProvider d)) liqNames _liqProvider


-- TODO fix query
performAction d t@TestDeal{rateSwap = Just rtSwap } (W.SwapAccrue sName)
  = Right $ t { rateSwap = Just newRtSwap } 
    where 
        refBal = case HE.rsNotional (rtSwap Map.! sName) of 
                   (HE.Fixed b) -> b
                   (HE.Base ds) -> queryDeal t (patchDateToStats d ds)
                   (HE.Schedule ts) -> fromRational $ getValByDate ts Inc d
                   
        newRtSwap = Map.adjust 
                      (HE.accrueIRS d)
                      sName
                      (Map.adjust (set HE.rsRefBalLens refBal) sName rtSwap)

performAction d t@TestDeal{rateSwap = Just rtSwap, accounts = accsMap } (W.SwapReceive accName sName)
  = Right $ t { rateSwap = Just newRtSwap, accounts = newAccMap }
    where 
        receiveAmt = max 0 $ HE.rsNetCash $ rtSwap Map.! sName
        newRtSwap = Map.adjust (HE.receiveIRS d) sName rtSwap -- `debug` ("REceiv AMT"++ show receiveAmt)
        newAccMap = Map.adjust (A.deposit receiveAmt d SwapInSettle) accName accsMap

performAction d t@TestDeal{rateCap = Just rcM, accounts = accsMap } (W.CollectRateCap accName sName)
  = Right $ t { rateCap = Just newRcSwap, accounts = newAccMap }
    where 
        receiveAmt = max 0 $ HE.rcNetCash $ rcM Map.! sName
        newRcSwap = Map.adjust (HE.receiveRC d) sName rcM -- `debug` ("REceiv AMT"++ show receiveAmt)
        newAccMap = Map.adjust (A.deposit receiveAmt d SwapInSettle) accName accsMap


performAction d t@TestDeal{rateSwap = Just rtSwap, accounts = accsMap } (W.SwapPay accName sName)
  = Right $ t { rateSwap = Just newRtSwap, accounts = newAccMap }
    where 
        payoutAmt = negate $ HE.rsNetCash $ rtSwap Map.! sName
        availBal = A.accBalance $ accsMap Map.! accName
        amtToPay = min payoutAmt availBal
        newRtSwap = Map.adjust (HE.payoutIRS d amtToPay) sName rtSwap
        newAccMap = Map.adjust (A.draw amtToPay d SwapOutSettle) accName accsMap

performAction d t@TestDeal{rateSwap = Just rtSwap, accounts = accsMap } (W.SwapSettle accName sName)
  = do
      t2 <- performAction d t (W.SwapReceive accName sName)
      performAction d t2 (W.SwapPay accName sName)

performAction d t@TestDeal{ triggers = Just trgM } (W.RunTrigger loc tNames)
  = do 
      tList <- newTrgList
      return $
          let 
            newTrgMap = Map.fromList $ zip tNames tList
          in 
            t { triggers = Just (Map.insert loc newTrgMap trgM) }
    where 
      triggerM = trgM Map.! loc
      triggerList = (triggerM Map.!) <$> tNames
      newTrgList = mapM 
                    (testTrigger t d)
                    triggerList

performAction d t (W.Placeholder mComment) = Right t 

performAction d t action =  error $ "failed to match action>>"++show action++">>Deal"++show (name t)
