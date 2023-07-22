{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Deal.DealAction (performActionWrap,performAction,calcDueFee
                       ,testTrigger,RunContext(..),updateLiqProvider
                       ,calcDueInt,calcDueFee,projAssetUnion,priceAssetUnion
                       ,accrueLiqProvider) 
  where

import qualified Accounts as A
import qualified Ledger as LD
import qualified Asset as P
import qualified Expense as F
import qualified Liability as L
import qualified CreditEnhancement as CE
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
debug = flip trace

calcDayToPoolDate :: (TestDeal a) -> Date -> Date 
calcDayToPoolDate t calcDay 
  = CF.mflowDate $ last pFlows -- `debug` ("calDayToPoolDate"++show calcDay ++">>>>>"++show pFlows)
    where 
      pFlows = getPoolFlows t Nothing (Just calcDay) EI  -- II here is not being used


getPoolFlows :: TestDeal a -> Maybe Date -> Maybe Date -> RangeType -> [CF.TsRow]
getPoolFlows t sd ed rt =
  case (sd,ed) of
    (Nothing,Nothing) ->  _trs
    (Nothing,Just _ed) -> case rt of 
                             EI -> filter (\x -> CF.getDate x <= _ed) _trs
    (Just _sd,Nothing) -> CF.getTxnAfter _projCf _sd   -- >= d
    (Just _sd,Just _ed) -> case rt of 
                             IE -> filter (\x -> (CF.getDate x >= _sd) && (CF.getDate x < _ed)) _trs
                             EI -> filter (\x -> (CF.getDate x > _sd) && (CF.getDate x <= _ed)) _trs
  where
    _projCf = fromMaybe (CF.CashFlowFrame []) (P.futureCf (pool t))
    _trs =  CF.getTsCashFlowFrame _projCf


testTrigger :: P.Asset a => TestDeal a -> Date -> Trigger -> Bool 
testTrigger t d trigger@Trigger{ trgStatus=st,trgCurable=cure,trgCondition=cond } 
  | not cure && st = True 
  | otherwise = testPre d t cond -- `debug` ("Testing Pre"++show d++">>>"++ show cond++">>result"++ show (testPre d t cond))


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
            Just _futureCf ->
                let 
                  poolInflow = CF.getEarlierTsCashFlowFrame _futureCf d -- `debug` ("liq:"++show _futureCf++"D"++ show d)
                  earlierTxns = CF.getTxnAsOf _futureCf d
                  currentDefaulBal = sum $ map (\x -> (CF.mflowDefault x) - (CF.mflowRecovery x) - (CF.mflowLoss x)) earlierTxns
                in 
                  case poolInflow of 
                    Nothing -> 0  -- `debug` ("No pool Inflow")
                    Just _ts ->   -- TODO need to check if missing last row
                        (mulBR (CF.mflowBalance _ts) currentFactor) + (mulBR currentDefaulBal defaultFactor) 
                        -- `debug` ("LIQ:"++show poolInflow)

      PV discountRate recoveryPct ->
          case P.futureCf pool of
            Nothing -> 0 
            Just _futureCf ->
                let 
                  futureTxns = CF.getTxnAfter _futureCf d
                  earlierTxns = CF.getTxnAsOf _futureCf d
                  pvCf = sum $ map (\x -> pv2  discountRate  d (CF.getDate x) (CF.tsTotalCash x)) futureTxns 
                  currentDefaulBal = sum $ map (\x -> (CF.mflowDefault x) - (CF.mflowRecovery x) - (CF.mflowLoss x)) earlierTxns
                in 
                  pvCf + mulBI currentDefaulBal recoveryPct

liquidatePool :: PricingMethod -> T.Day -> String -> TestDeal a -> TestDeal a
liquidatePool lq d accName t =
  t {accounts = Map.adjust updateFn accName accs} -- `debug` ("Accs->"++show(accs))
  where
     proceeds = calcLiquidationAmount lq (pool t) d
     updateFn = A.deposit proceeds d (LiquidationProceeds)
     accs = accounts t



calcDueFee :: P.Asset a => (TestDeal a) -> Date -> F.Fee -> F.Fee
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
                                OriginalPoolBalance -> (mulBR (P.getIssuanceField (pool t) P.IssuanceBalance) (yearCountFraction DC_ACT_365F accrueStart calcDay),collectionEndDay)
                                OriginalBondBalance -> (mulBR (queryDeal t OriginalBondBalance) (yearCountFraction DC_ACT_365F accrueStart calcDay),calcDay)
                                CurrentBondBalance -> (Map.foldr (\v a-> a + L.weightAverageBalance accrueStart calcDay v ) 0.0 (bonds t),calcDay)
                                CurrentBondBalanceOf bns -> (Map.foldr (\v a-> a + L.weightAverageBalance accrueStart calcDay v ) 0.0 (getBondByName t (Just bns)),calcDay)
        r = toRational $ queryDealRate t _r 
        newDue = mulBR baseBal r

calcDueFee t calcDay f@(F.Fee fn (F.PctFee (PoolCollectionIncome it) r ) fs fd fdDay fa lpd _)
  = f { F.feeDue = newDueFee, F.feeDueDate = Just calcDay } -- `debug` ("BAL"++show baseBal++"New Fee Due"++ show newDueFee)
    where 
      baseBal = queryDeal t (PoolCollectionHistory it lastBegDay calcDay)  
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
      ,F.feeType = F.FeeFlow futureDue} -- `debug` ("New fee due"++show newFeeDue)
    where
      (currentNewDue,futureDue) = splitTsByDate ts calcDay
      cumulativeDue = sumValTs currentNewDue
      newFeeDue =  cumulativeDue + fd  -- `debug` ("cumulativeDue"++ show cumulativeDue)

calcDueFee t calcDay f@(F.Fee fn (F.RecurFee p amt)  fs fd Nothing fa _ _)
  = f{ F.feeDue = amt * (fromIntegral (periodGaps - 1)), F.feeDueDate = Just calcDay } -- `debug` ("New fee"++show(f))
  where
    periodGaps = length $ projDatesByPattern p fs calcDay  -- `debug` ("###"++show (projDatesByPattern p fs calcDay))

calcDueFee t calcDay f@(F.Fee fn (F.RecurFee p amt)  fs fd (Just _fdDay) fa _ _)
  | _fdDay == calcDay = f
  | periodGap == 0 = f
  | otherwise = f { F.feeDue = (fd+(amt*(fromIntegral (periodGap - 1)))) , F.feeDueDate = Just calcDay } -- `debug` ("Gap->"++show(fromIntegral periodGap))
  where
    periodGap =  length $ projDatesByPattern p _fdDay calcDay

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


updateLiqProvider :: P.Asset a => TestDeal a -> Date -> CE.LiqFacility -> CE.LiqFacility
updateLiqProvider t d liq@(CE.LiqFacility _ liqType (Just curBal) curCredit _ _ _ mRate mPRate stmt) -- refresh available balance
  = liq { CE.liqBalance = newBalance }
    where 
      newBalance = case liqType of 
                     CE.ReplenishSupport _ b -> Just (max b curBal)
                     _ -> Just curBal
updateLiqProvider _ _ liq = liq

accrueLiqProvider :: P.Asset a => TestDeal a -> Date -> CE.LiqFacility -> CE.LiqFacility
accrueLiqProvider t d liq@(CE.LiqFacility _ _ mCurBal curCredit sd di dp mRate mPRate Nothing)
  = accrueLiqProvider t d $ liq{CE.liqStmt = Just defaultStmt} 
   where 
       defaultStmt = Statement [SupportTxn sd mCurBal 0 curCredit di dp Empty]

accrueLiqProvider t d liq@(CE.LiqFacility _ _ mCurBal curCredit sd dueInt dueFee mRate mPRate stmt)
  = liq { CE.liqCredit = newBal
         ,CE.liqStmt = Just newStmt
         ,CE.liqDueInt = newDueInt
         ,CE.liqDuePremium = newDueFee
         ,CE.liqPremium = newPRate
         ,CE.liqRate = newRate} -- `debug` ("Accure liq"++ show liq)
    where 
      accureInt = case mRate of 
                    Nothing -> 0
                    Just (CE.FixRate _ r mLastAccDate) -> 
                      let 
                        lastAccDate = fromMaybe sd mLastAccDate
                        bals = weightAvgBalanceByDates [lastAccDate,d] $ getTxns stmt
                      in 
                        sum $ ((flip mulBR) r) <$> bals
      accureFee = case mPRate of
                    Nothing -> 0 
                    Just (CE.FixRate _ r mLastAccDate) -> 
                      let 
                        lastAccDate = fromMaybe sd mLastAccDate
                        (_,_unAccTxns) = splitByDate (getTxns stmt) lastAccDate EqToLeftKeepOne
                        accBals = getUnusedBal <$> _unAccTxns 
                        _ds = lastAccDate : tail (getDate <$> _unAccTxns)
                        _avgBal = calcWeigthBalanceByDates accBals (_ds++[d])
                      in 
                        mulBR _avgBal r  
                        
      getUnusedBal (SupportTxn _ b _ _ _ _ _ ) = fromMaybe 0 b 
      
      newDueFee = Just $ accureFee + fromMaybe 0 dueFee 
      newDueInt = Just $ accureInt + fromMaybe 0 dueInt
      newBal = curCredit + accureInt + accureFee
      newStmt = appendStmt stmt $ SupportTxn d 
                                             mCurBal 
                                             (accureInt + accureFee) 
                                             newBal 
                                             newDueInt 
                                             newDueFee 
                                             (LiquidationSupportInt accureInt accureFee)
      
      newRate = case mRate of 
                  Nothing -> Nothing
                  Just (CE.FixRate _x _y _) -> Just $ CE.FixRate _x _y (Just d)
      newPRate = case mPRate of 
                  Nothing -> Nothing
                  Just (CE.FixRate _x _y _) -> Just $ CE.FixRate _x _y (Just d)


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
                       L.Floater _ _ _ _dc _ _ -> _dc 
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
    isZbond (L.Bond _ _ _ _ _ _ _ _ _ _ _ _) = False
    
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

splitAssetUnion :: [Rate] -> ACM.AssetUnion -> [ACM.AssetUnion]
splitAssetUnion rs (ACM.MO m) = [ ACM.MO a | a <- P.splitWith m rs]
splitAssetUnion rs (ACM.LO m) = [ ACM.LO a | a <- P.splitWith m rs]
splitAssetUnion rs (ACM.IL m) = [ ACM.IL a | a <- P.splitWith m rs]
splitAssetUnion rs (ACM.LS m) = [ ACM.LS a | a <- P.splitWith m rs]

buyRevolvingPool :: Date -> [Rate] -> RevolvingPool -> ([ACM.AssetUnion],RevolvingPool)
buyRevolvingPool _ rs rp@(StaticAsset assets) 
  = let 
      splitedAssets = (splitAssetUnion rs) <$> assets
      assetBought = head <$> splitedAssets
      assetRemains = last <$> splitedAssets 
    in 
      (assetBought ,StaticAsset assetRemains)

buyRevolvingPool _ rs rp@(ConstantAsset assets)
  = let 
      splitedAssets = (splitAssetUnion rs) <$> assets
      assetBought = head <$> splitedAssets
    in 
      (assetBought ,rp)

buyRevolvingPool d rs rp@(AssetCurve aus)
  = let
      assets = lookupAssetAvailable rp d 
      splitedAssets = (splitAssetUnion rs) <$> assets
      assetBought = head <$> splitedAssets
    in 
      (assetBought, rp)


projAssetUnion :: ACM.AssetUnion -> Date -> [AP.AssumptionBuilder] -> CF.CashFlowFrame
projAssetUnion (ACM.MO ast) d assumps = CF.cfInsertHead (CF.MortgageFlow d (P.getCurrentBal ast) 0 0 0 0 0 0 0 Nothing) $ P.projCashflow ast d assumps
projAssetUnion (ACM.LO ast) d assumps = CF.cfInsertHead (CF.LoanFlow d (P.getCurrentBal ast) 0 0 0 0 0 0 0) $ P.projCashflow ast d assumps
projAssetUnion (ACM.IL ast) d assumps = CF.cfInsertHead (CF.LoanFlow d (P.getCurrentBal ast) 0 0 0 0 0 0 0) $ P.projCashflow ast d assumps
projAssetUnion (ACM.LS ast) d assumps = CF.cfInsertHead (CF.LeaseFlow d (P.getCurrentBal ast) 0 ) $ P.projCashflow ast d assumps

data RunContext a = RunContext{
                  runPoolFlow:: CF.CashFlowFrame
                  ,revolvingAssump:: Maybe (RevolvingPool ,[AP.AssumptionBuilder])
                   }

updateOriginDate2 :: Date -> ACM.AssetUnion -> ACM.AssetUnion
updateOriginDate2 d (ACM.LO m) = ACM.LO $ (updateOriginDate m) (P.calcAlignDate m d)
updateOriginDate2 d (ACM.MO m) = ACM.MO $ (updateOriginDate m) (P.calcAlignDate m d)
updateOriginDate2 d (ACM.IL m) = ACM.IL $ (updateOriginDate m) (P.calcAlignDate m d)
updateOriginDate2 d (ACM.LS m) = ACM.LS $ (updateOriginDate m) (P.calcAlignDate m d)
 

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
      assets = (updateOriginDate2 d) <$> _assets 
                
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

performAction d t@TestDeal{accounts=accMap} (W.Transfer an1 an2) =
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
                     ,(queryTxnAmt (ledgerM Map.! ledgerName) (TxnDirection Debit)))
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

performAction d t@TestDeal{accounts=accMap, ledgers = Just ledgerM} (W.TransferBy (ClearPDL ln) an1 an2) =
  t {accounts = accMapAfterDeposit, ledgers = Just newLedgerM}  -- `debug` ("ABCD "++show(d))
  where
    sourceAcc = accMap Map.! an1
    targetAcc = accMap Map.! an2 -- `debug` ("Target>>"++an2)
    targetAmt = queryDeal t (LedgerBalance [ln]) -- assuming (debit -> positvie)
    transferAmt = min (A.accBalance sourceAcc) targetAmt -- `debug` ("Clear PDL"++show d++ show targetAmt)
 
    accMapAfterDraw = Map.adjust (A.draw transferAmt d (TransferBy an1 an2 (ClearPDL ln))) an1 accMap -- `debug` (">>PDL >>Ledger bal"++show d ++ show targetAmt)
    accMapAfterDeposit = Map.adjust (A.deposit transferAmt d (TransferBy an1 an2 (ClearPDL ln))) an2 accMapAfterDraw

    newLedgerM = Map.adjust 
                   (LD.entryLog (negate transferAmt) d (TxnDirection Credit))
                   ln 
                   ledgerM

performAction d t@TestDeal{accounts=accMap} (W.TransferBy limit an1 an2) =
  t {accounts = accMapAfterDeposit}  -- `debug` ("ABCD "++show(d))
  where
    sourceAcc = accMap Map.! an1
    targetAcc = accMap Map.! an2 -- `debug` ("Target>>"++an2)
    formulaAmount = case limit of 
                      DuePct r -> r * A.accBalance sourceAcc
                      DueCapAmt a -> min a (A.accBalance sourceAcc)
                      DS ds -> queryDeal t (patchDateToStats d ds)
                      Formula ABCD -> max 
                                          ((queryDeal t CumulativePoolDefaultedBalance) + 
                                             (negate (queryTxnAmt targetAcc (Transfer an2 an1))) +
                                             (negate (queryTxnAmt sourceAcc (Transfer an1 an2))))
                                        0
    transferAmt = min (max formulaAmount 0) (A.accBalance sourceAcc) -- `debug` ("Formula amount"++show formulaAmount)

    accMapAfterDraw = Map.adjust (A.draw transferAmt d (TransferBy an1 an2 limit)) an1 accMap
    accMapAfterDeposit = Map.adjust (A.deposit transferAmt d (TransferBy an1 an2 limit)) an2 accMapAfterDraw

performAction d t@TestDeal{accounts=accMap} (W.TransferReserve meetAcc sa ta)=
    t {accounts = accMapAfterTransfer }
  where
    sourceAcc = accMap Map.! sa 
    targetAcc = accMap Map.! ta
    sourceAccBal = A.accBalance sourceAcc
    targetAccBal = A.accBalance targetAcc 
    transferAmt = 
        case meetAcc of 
             W.Source -> max (sourceAccBal - (calcTargetAmount t d sourceAcc) ) 0
             W.Target ->
                 let 
                   targetBal = calcTargetAmount t d targetAcc
                   transferAmtTarget = max (targetBal - targetAccBal) 0 -- `debug` ("Target balance ->> "++show(targetBal))
                 in 
                   min transferAmtTarget sourceAccBal

    accMapAfterTransfer
      = case transferAmt of
          0 -> accMap
          amt ->  Map.adjust (A.draw amt d (Transfer sa ta)) sa  $ 
                  Map.adjust (A.deposit amt d (Transfer sa ta)) ta $ accMap

performAction d t@TestDeal{fees=feeMap} (W.PayFee ans fns) =
  t {accounts = accMapUpdated, fees = feeMapUpdated}
  where
    accMap = getAccountByName t (Just ans)

    feesToPay = map (feeMap Map.!) fns
    feeDueAmts = map F.feeDue feesToPay  

    accNList = Map.toList accMap -- `debug` ("Show Fee with Due "++show(feeDueAmts))
    availBalLst = [ (n,A.accBalance x) | (n,x) <- accNList ]
    availAccBals = map snd availBalLst
    availAccNames = map fst availBalLst
    accList = map (accMap Map.!) ans

    availBal = sum availAccBals

    actualPaidOut = min availBal $ sum feeDueAmts -- `debug` ("Fee Due Amounts"++show(feeDueAmts))
    feesAmountToBePaid = zip feesToPay  $ prorataFactors feeDueAmts availBal
    feesPaid = map (\(f,amt) -> F.payFee d amt f) feesAmountToBePaid

    feeMapUpdated = Map.union (Map.fromList $ zip fns feesPaid) feeMap

    accsAfterPay = A.supportPay accList d actualPaidOut (SeqPayFee fns ,SeqPayFee fns)  
    accMapUpdated = Map.union (Map.fromList (zip ans accsAfterPay)) (accounts t)


performAction d t@TestDeal{fees=feeMap} (W.PayFeeBy limit ans fns) =
  t {accounts = accMapUpdated, fees = feeMapUpdated}
  where
    accMap = getAccountByName t (Just ans)
    feesToPay = map (feeMap Map.!) fns
    feeDueAmts = case limit of
                  (DuePct pct) -> map (\x -> pct * (F.feeDue x) ) feesToPay
                  (DueCapAmt amt) -> map (\x -> (min (F.feeDue x) amt)) feesToPay

    accNList = Map.toList accMap
    availBalLst = [ (n,A.accBalance x) | (n,x) <- accNList]
    availAccBals = map snd availBalLst
    availAccNames = map fst availBalLst
    accList = map (accMap Map.!) ans

    availBal = sum availAccBals

    actualPaidOut = min availBal $ sum feeDueAmts
    feesAmountToBePaid = zip feesToPay  $ prorataFactors feeDueAmts availBal
    feesPaid = map (\(f,amt) -> F.payFee d amt f) feesAmountToBePaid

    feeMapUpdated = Map.union (Map.fromList $ zip fns feesPaid) feeMap

    accsAfterPay = A.supportPay accList d actualPaidOut (SeqPayFee fns,SeqPayFee fns)  
    accMapUpdated = Map.union (Map.fromList (zip ans accsAfterPay)) (accounts t)

performAction d t@TestDeal{bonds=bndMap,accounts=accMap} (W.PayInt an bnds) =
  t {accounts = accMapAfterPay, bonds = bndMapUpdated}
  where
    acc = accMap Map.! an
    availBal = A.accBalance acc
    bndsToPay = map (bndMap Map.!) bnds

    bndsDueAmts = map L.bndDueInt bndsToPay
    bndsNames = map L.bndName bndsToPay

    actualPaidOut = min availBal $ sum bndsDueAmts -- `debug` ("due mats"++ show bndsDueAmts ++">>"++ show availBal)
    bndsAmountToBePaid = zip bndsToPay $ prorataFactors bndsDueAmts availBal -- `debug` ("prorata"++ show (prorataFactors bndsDueAmts availBal) )

    bndsPaid = map (\(l,amt) -> L.payInt d amt l) bndsAmountToBePaid

    bndMapUpdated = Map.union (Map.fromList $ zip bndsNames bndsPaid) bndMap
    comment = PayInt bnds
    accMapAfterPay = Map.adjust 
                       (A.draw actualPaidOut d comment)
                       an
                       accMap

performAction d t (W.AccrueAndPayInt an bnds) =
  let 
    dealWithBondDue = performAction d t (W.CalcBondInt bnds)
  in 
    performAction d dealWithBondDue (W.PayInt an bnds)


performAction d t (W.PayTillYield an bnds) = performAction d t (W.AccrueAndPayInt an bnds)

performAction d t@TestDeal{bonds=bndMap,accounts=accMap} (W.PayResidual Nothing an bndName) =
  t {accounts = accMapAfterPay, bonds = bndMapAfterPay}
  where
    availBal = A.accBalance $ accMap Map.! an
    accMapAfterPay = Map.adjust (A.draw availBal d (PayYield bndName)) an accMap
    bndMapAfterPay = Map.adjust (L.payYield d availBal) bndName bndMap

performAction d t@TestDeal{fees=feeMap,accounts=accMap} (W.PayFeeResidual limit an feeName) =
  t {accounts = accMapAfterPay, fees = feeMapAfterPay}
  where
    availBal = A.accBalance $ accMap Map.! an
    paidOutAmt = case limit of
                   Just (DuePct pct) ->  pct * availBal
                   Just (DueCapAmt cap) ->  min cap availBal
                   Nothing -> availBal
    accMapAfterPay = Map.adjust (A.draw paidOutAmt d (PayFeeYield feeName)) an accMap
    feeMapAfterPay = Map.adjust (F.payResidualFee d paidOutAmt) feeName feeMap

-- ^ pay bond till its balance as pct of total balance
performAction d t@TestDeal{bonds=bndMap,accounts=accMap} (W.PayPrinBy (RemainBalPct pct) an bndName)=  --Need to replace with formula
  t {accounts = accMapAfterPay, bonds = bndMapAfterPay}
  where
    availBal = A.accBalance $ accMap Map.! an
    targetBnd = bndMap Map.! bndName
    targetBndBal = L.bndBalance targetBnd

    otherBndBal = queryDeal t CurrentBondBalance - targetBndBal

    _pct = fromRational pct
    dueAmount = (1/(1-_pct)) * (targetBndBal * (1-_pct) - (_pct * otherBndBal))
    actAmount = min availBal $ max dueAmount 0

    accMapAfterPay = Map.adjust
                        (A.draw actAmount d (PayPrin [bndName])) 
                        an
                        accMap
    bndMapAfterPay = Map.adjust (L.payPrin d actAmount) bndName bndMap

performAction d t@TestDeal{bonds=bndMap,accounts=accMap} (W.PayPrinBy (DS ds) an bndName)=  --Need to replace with formula
  t {accounts = accMapAfterPay, bonds = bndMapAfterPay}
  where
    availBal = A.accBalance $ accMap Map.! an
    targetBndBal = L.bndBalance $ bndMap Map.! bndName

    patchedDs = patchDateToStats d ds 
    payAmount = min targetBndBal $ min availBal (queryDeal t patchedDs) -- `debug` ("Query with "++show (patchedDs))

    accMapAfterPay = Map.adjust
                        (A.draw payAmount d (TxnComments [PayPrin [bndName],UsingDS ds])) 
                        an
                        accMap  -- `debug` ("payOutAmt"++show (queryDeal t patchedDs))
    bndMapAfterPay = Map.adjust 
                       (L.payPrin d payAmount) 
                       bndName $
                       Map.adjust (calcDuePrin t d) bndName bndMap -- `debug` ("Actual PayAmount"++show payAmount)

performAction d t@TestDeal{bonds=bndMap,accounts=accMap} (W.PayPrin an bnds) =
  t {accounts = accMapAfterPay, bonds = bndMapUpdated} -- `debug` ("Bond Prin Pay Result"++show(bndMapUpdated))
  where
    acc = accMap Map.! an

    bndsToPay = filter (\x -> L.bndBalance x > 0) $ map (bndMap Map.!) bnds
    availBal = A.accBalance acc
    -- TODO  add filter lockout bonds here
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
    -- TODO  add filter lockout bonds here
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

performAction d t@TestDeal{accounts=accs,liqProvider = Just _liqProvider} (W.LiqSupport limit pName an)
  = t { accounts = newAccMap, liqProvider = Just newLiqMap } -- `debug` ("Using LImit"++ show limit)
  where 
      _transferAmt = case limit of 
                      Nothing -> 0 -- `debug` ("limit on nothing"++ show limit)
                      Just (DS (ReserveAccGap [an])) -> queryDeal t (ReserveAccGapAt d [an]) -- `debug` ("Query Gap"++ show (queryDeal t (ReserveAccGapAt d [an])))
                      Just (DS ds) -> queryDeal t (patchDateToStats d ds) -- `debug` ("hit with ds"++ show ds)
                      _ -> error "Failed on formula passed" -- `debug` ("limit on last"++ show limit)
      transferAmt = case CE.liqBalance $  _liqProvider Map.! pName of 
                       Nothing -> _transferAmt
                       Just _availBal -> min _transferAmt _availBal  -- `debug` ("transfer amt"++ show _transferAmt )
      newAccMap = Map.adjust (A.deposit transferAmt d (LiquidationSupport pName)) an accs
      newLiqMap = Map.adjust (CE.draw transferAmt d ) pName _liqProvider 

performAction d t@TestDeal{fees=feeMap,liqProvider = Just _liqProvider} (W.LiqPayFee limit pName fn)
  = t { fees = newFeeMap, liqProvider = Just newLiqMap }
  where 
      _transferAmt = case limit of 
                      Nothing -> 0 
                      Just (DS (CurrentDueFee [fn]))
                        -> queryDeal t (CurrentDueFee [fn])
                      _ -> 0
      transferAmt = case CE.liqBalance $  _liqProvider Map.! pName of 
                       Nothing -> _transferAmt
                       Just _availBal -> min _transferAmt _availBal
     -- transferAmt = min _transferAmt $ CE.liqBalance $  _liqProvider Map.! pName
      newFeeMap = Map.adjust (F.payFee d transferAmt) fn feeMap
      newLiqMap = Map.adjust (CE.draw transferAmt d ) pName _liqProvider 

performAction d t@TestDeal{bonds=bndMap,liqProvider = Just _liqProvider} (W.LiqPayBond limit pName bn)
  = t { bonds = newBondMap, liqProvider = Just newLiqMap }
  where 
      _transferAmt = case limit of 
                      Nothing -> 0 
                      Just (DS (CurrentDueBondInt [bn]))
                        -> queryDeal t (CurrentDueBondInt [bn])
                      _ -> 0
      transferAmt = case CE.liqBalance $  _liqProvider Map.! pName of 
                       Nothing -> _transferAmt
                       Just _availBal -> min _transferAmt _availBal
      --transferAmt = min _transferAmt $ CE.liqBalance $  _liqProvider Map.! pName
      newBondMap = Map.adjust (L.payInt d transferAmt ) bn bndMap
      newLiqMap = Map.adjust (CE.draw transferAmt d ) pName _liqProvider 

performAction d t@TestDeal{accounts=accs,liqProvider = Just _liqProvider} (W.LiqRepay limit rpt an pName)
  = t { accounts = newAccMap, liqProvider = Just newLiqMap }
  where 
      liqDue = CE.liqCredit $ _liqProvider Map.! pName
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
      updatedLiqProvider = Map.adjust (accrueLiqProvider t d ) n _liqProvider

performAction d t@TestDeal{rateSwap = Just rtSwap } (W.SwapAccrue sName)
  = t { rateSwap = Just newRtSwap }
    where 
        newRtSwap = Map.adjust (CE.accrueIRS d) sName rtSwap

performAction d t@TestDeal{rateSwap = Just rtSwap, accounts = accsMap } (W.SwapReceive accName sName)
  = t { rateSwap = Just newRtSwap, accounts = newAccMap }
    where 
        receiveAmt = CE.rsNetCash $ rtSwap Map.! sName
        newRtSwap = Map.adjust (CE.receiveIRS d) sName rtSwap
        newAccMap = Map.adjust (A.deposit receiveAmt d SwapInSettle) accName accsMap

performAction d t@TestDeal{rateSwap = Just rtSwap, accounts = accsMap } (W.SwapPay accName sName)
  = t { rateSwap = Just newRtSwap, accounts = newAccMap }
    where 
        payoutAmt = negate $ CE.rsNetCash $ rtSwap Map.! sName
        availBal = A.accBalance $ accsMap Map.! accName
        amtToPay = min payoutAmt availBal 
        newRtSwap = Map.adjust (CE.payoutIRS d amtToPay) sName rtSwap
        newAccMap = Map.adjust (A.draw amtToPay d SwapOutSettle) accName accsMap

performAction d t@TestDeal{ triggers = Just trgM } (W.RunTrigger loc idx)
  = t { triggers = Just (Map.insert loc newTrgLst trgM) }
    where 
      trg = (trgM Map.! loc) !! idx
      newTrg = updateTrigger t d trg
      newTrgLst = replace (trgM Map.! loc) idx newTrg
