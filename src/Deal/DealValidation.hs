{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Deal.DealValidation (validateRun,validatePreRun,validateReq)
  where 

import Deal.DealBase
import Types
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import Data.ByteString (intercalate, foldl1')

import qualified Waterfall as W
import qualified CreditEnhancement as CE
import qualified Liability as L
import qualified Accounts as A
import qualified Expense as F
import qualified Asset as P
import qualified Assumptions as AP
import qualified InterestRate as IR


import Data.Maybe
import qualified Assumptions as A
import Asset (getIssuanceField)

isPreClosing :: TestDeal a -> Bool
isPreClosing t@TestDeal{ status = PreClosing _ } = True
isPreClosing _ = False

validateAction :: [W.Action] -> [ResultComponent] -> Set.Set String -> Set.Set String -> Set.Set String-> Set.Set String-> Set.Set String -> Set.Set String -> Set.Set String -> [ResultComponent]
validateAction [] rs _ _ _ _ _ _ _ = rs
validateAction ((W.Transfer _ acc1 acc2 _):as) rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys 
  | Set.notMember acc1 accKeys || Set.notMember acc2 accKeys 
    = validateAction as (rs ++ [ErrorMsg (acc1 ++","++acc2++" not in "++ show accKeys)]) accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys
  | otherwise = validateAction as rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys

validateAction ((W.CalcFee fees):as) rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys 
  | not (Set.isSubsetOf (Set.fromList fees) feeKeys)
    = validateAction as (rs ++ [ErrorMsg (show fees ++ " not in "++ show feeKeys)]) accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys
  | otherwise = validateAction as rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys

validateAction ((W.PayFee _ accName fees _):as) rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys
  | not (Set.isSubsetOf (Set.fromList fees) feeKeys) || Set.notMember accName accKeys
    = validateAction as (rs ++ [ErrorMsg (show fees ++ " not in "++ show feeKeys++" Or "++ show accName ++" not in "++show accKeys)]) accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys
  | otherwise = validateAction as rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys

validateAction ((W.CalcAndPayFee _ accName fees _):as) rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys
  | not (Set.isSubsetOf (Set.fromList fees) feeKeys) || Set.notMember accName accKeys
    = validateAction as (rs ++ [ErrorMsg (show fees ++ " not in "++ show feeKeys++" Or "++ accName ++" not in "++ show accKeys)]) accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys
  | otherwise = validateAction as rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys

validateAction ((W.PayFeeResidual _ accName feeName):as) rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys
  | Set.notMember feeName feeKeys || Set.notMember accName accKeys
    = validateAction as (rs ++ [ErrorMsg (feeName ++ " not in "++ show feeKeys++" Or "++accName++ " not in "++show accKeys)]) accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys
  | otherwise = validateAction as rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys

validateAction ((W.CalcBondInt bnds Nothing Nothing):as) rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys
  | not (Set.isSubsetOf (Set.fromList bnds) bndKeys)
    = validateAction as (rs ++ [ErrorMsg (show bnds ++ " not in "++ show bndKeys)]) accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys
  | otherwise = validateAction as rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys

validateAction ((W.PayInt _ accName bnds _):as) rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys
  | not (Set.isSubsetOf (Set.fromList bnds) bndKeys) || Set.notMember accName accKeys
    = validateAction as (rs ++ [ErrorMsg (show bnds ++ " not in "++ show bndKeys++" Or "++accName++" not in "++show accKeys)]) accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys
  | otherwise = validateAction as rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys

validateAction ((W.AccrueAndPayInt _ accName bnds _):as) rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys
  | not (Set.isSubsetOf (Set.fromList bnds) bndKeys) || Set.notMember accName accKeys
    = validateAction as (rs ++ [ErrorMsg (show bnds ++ " not in "++ show bndKeys++" Or "++accName++" not in "++show accKeys)]) accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys
  | otherwise = validateAction as rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys

validateAction ((W.PayIntResidual _ accName bndName):as) rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys
  | Set.notMember bndName bndKeys || Set.notMember accName accKeys
    = validateAction as (rs ++ [ErrorMsg (bndName ++ " not in "++ show bndKeys++" Or "++accName++" not in "++show accKeys)]) accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys
  | otherwise = validateAction as rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys

validateAction ((W.PayPrin _ accName bnds _):as) rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys  ledgerKeys
  | not (Set.isSubsetOf (Set.fromList bnds) bndKeys) || Set.notMember accName accKeys
    = validateAction as (rs ++ [ErrorMsg (show bnds++ " not in "++ show bndKeys++" Or "++accName++" not in "++show accKeys)]) accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys
  | otherwise = validateAction as rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys

validateAction ((W.PayPrinResidual accName bnds):as) rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys
  | not (Set.isSubsetOf (Set.fromList bnds) bndKeys) || Set.notMember accName accKeys
    = validateAction as (rs ++ [ErrorMsg (show bnds++ " not in "++ show bndKeys++" Or "++accName++" not in "++show accKeys)]) accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys
  | otherwise = validateAction as rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys

validateAction ((W.BuyAsset _ _ accName _):as) rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys
  | Set.notMember accName accKeys = validateAction as (rs ++ [ErrorMsg (accName++" not in "++show accKeys)]) accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys
  | otherwise = validateAction as rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys

validateAction ((W.LiquidatePool _ accName):as) rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys
  | Set.notMember accName accKeys = validateAction as (rs ++ [ErrorMsg (accName++" not in "++show accKeys)]) accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys
  | otherwise = validateAction as rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys

validateAction ((W.LiqSupport _ liqName _ accName):as) rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys
  | Set.notMember accName accKeys || Set.notMember liqName liqProviderKeys 
    = validateAction as (rs ++ [ErrorMsg (accName++" not in "++show accKeys++" Or "++liqName ++" not in "++ show liqProviderKeys)]) accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys
  | otherwise = validateAction as rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys

validateAction ((W.LiqRepay _ _ accName liqName):as) rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys
  | Set.notMember accName accKeys || Set.notMember liqName liqProviderKeys 
    = validateAction as (rs ++ [ErrorMsg (accName++" not in "++show accKeys++" Or "++liqName ++" not in "++ show liqProviderKeys)]) accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys
  | otherwise = validateAction as rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys

validateAction ((W.LiqYield _ accName liqName):as) rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys
  | Set.notMember accName accKeys || Set.notMember liqName liqProviderKeys 
    = validateAction as (rs ++ [ErrorMsg (accName++" not in "++show accKeys++" Or "++liqName ++" not in "++ show liqProviderKeys)]) accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys
  | otherwise = validateAction as rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys

validateAction ((W.LiqAccrue liqName):as) rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys
  | Set.notMember liqName liqProviderKeys 
    = validateAction as (rs ++ [ErrorMsg (liqName ++" not in "++ show liqProviderKeys)]) accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys
  | otherwise = validateAction as rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys

validateAction ((W.LiqAccrue liqName):as) rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys
  | Set.notMember liqName liqProviderKeys 
    = validateAction as (rs ++ [ErrorMsg (liqName ++" not in "++ show liqProviderKeys)]) accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys
  | otherwise = validateAction as rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys

validateAction ((W.SwapAccrue rsName):as) rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys
  | Set.notMember rsName rateSwapKeys
    = validateAction as (rs ++ [ErrorMsg (rsName ++" not in "++ show rateSwapKeys)]) accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys
  | otherwise = validateAction as rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys

validateAction ((W.SwapReceive accName rsName):as) rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys
  | Set.notMember rsName rateSwapKeys || Set.notMember accName accKeys
    = validateAction as (rs ++ [ErrorMsg (rsName ++" not in "++ show rateSwapKeys ++ " Or "++ accName ++ " not in "++ show accKeys)]) accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys
  | otherwise = validateAction as rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys

validateAction ((W.SwapPay accName rsName):as) rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys
  | Set.notMember rsName rateSwapKeys || Set.notMember accName accKeys
    = validateAction as (rs ++ [ErrorMsg (rsName ++" not in "++ show rateSwapKeys ++ " Or "++ accName ++ " not in "++ show accKeys)]) accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys
  | otherwise = validateAction as rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys

validateAction ((W.SwapSettle accName rsName):as) rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys
  | Set.notMember rsName rateSwapKeys || Set.notMember accName accKeys
    = validateAction as (rs ++ [ErrorMsg (rsName ++" not in "++ show rateSwapKeys ++ " Or "++ accName ++ " not in "++ show accKeys)]) accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys
  | otherwise = validateAction as rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys

validateAction ((W.CollectRateCap accName rcName):as) rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys
  | Set.notMember rcName rcKeys || Set.notMember accName accKeys
    = validateAction as (rs ++ [ErrorMsg (rcName ++" not in "++ show rcKeys ++ " Or "++ accName ++ " not in "++ show accKeys)]) accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys
  | otherwise = validateAction as rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys

validateAction ((W.ActionWithPre p subActionList):as) rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys
  = validateAction (subActionList++as) rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys

validateAction ((W.ActionWithPre2 p subActionList1 subActionList2):as) rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys
  = validateAction (subActionList1++subActionList2++as) rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys

validateAction (action:as) rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys
  = validateAction as rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys

extractRequiredRates :: (P.Asset a,IR.UseRate a) => TestDeal a -> Set.Set Index
extractRequiredRates t@TestDeal{accounts = accM 
                               ,fees = feeM 
                               ,bonds = bondM 
                               ,liqProvider = mliqProviderM 
                               ,rateSwap = mrsM 
                               ,rateCap = mRcM
                               ,pool = pool}
  = Set.fromList $ assetIndex ++ accIndex ++ bondIndex ++ liqProviderIndex ++ rsIndex ++ rcIndex
  -- = Set.fromList $ accIndex ++ bondIndex ++ liqProviderIndex ++ rsIndex
    where 
      assetIndex = catMaybes $ IR.getIndex <$> getAllAssetList t
      
      accIndex = catMaybes $ IR.getIndex <$> Map.elems accM 
      bondIndex = concat $ catMaybes $ IR.getIndexes <$> Map.elems bondM 
      liqProviderIndex = case mliqProviderM of 
                           Just liqProviderM -> concat $ catMaybes $ IR.getIndexes <$> Map.elems liqProviderM
                           Nothing -> [] 
      rsIndex = case mrsM of 
                  Just rsM -> concat $ catMaybes $ IR.getIndexes <$> Map.elems rsM
                  Nothing -> []
      rcIndex = case mRcM of 
                  Just rcM -> concat $ catMaybes $ IR.getIndexes <$> Map.elems rcM
                  Nothing -> []
        
      -- note fee is not tested
-- TODO Need to fix 
validateAggRule :: [W.CollectionRule] -> [ResultComponent]
validateAggRule rules = 
    [] -- [ ErrorMsg ("Pool source "++show ps++" has a weight of "++show r)   | (ps,r) <- Map.toList oustandingPs ]
  where 
    countWeight (W.Collect Nothing ps _) =  Map.fromList [(ps,1.0)]
    countWeight (W.CollectByPct _ ps lst) = Map.fromList [(ps, sum (fst <$> lst))]
    sumMap = foldl1 (Map.unionWith (+)) $ countWeight <$> rules 
    oustandingPs = Map.filter (> 1.0) sumMap


validateReq :: (IR.UseRate a,P.Asset a) => TestDeal a -> AP.NonPerfAssumption -> (Bool,[ResultComponent])
validateReq t assump@A.NonPerfAssumption{A.interest = intM} 
  = let 
      ratesRequired = extractRequiredRates t
      ratesSupplied = case intM of 
                        Nothing -> Set.empty
                        Just intLst -> Set.fromList $ [ idx | RateFlat idx _ <- intLst ] ++ [ idx | RateCurve idx _ <- intLst ]
      missingIndex = Set.difference ratesRequired ratesSupplied
      missingIndexError = if null missingIndex then 
                            []
                          else
                            [ErrorMsg ("Failed to find index "++show missingIndex++"in assumption rates"++ show ratesSupplied)]

      (dealWarnings,dealErrors) = validatePreRun t 
      finalErrors = missingIndexError ++ dealErrors                          
      finalWarnings = dealWarnings
    in 
      (null finalErrors,finalErrors++finalWarnings)

validatePreRun :: P.Asset a => TestDeal a -> ([ResultComponent],[ResultComponent])
validatePreRun t@TestDeal{waterfall=waterfallM
                      ,accounts =accM 
                      ,fees = feeM 
                      ,bonds = bondM 
                      ,collects = aggRule 
                      ,liqProvider = liqProviderM 
                      ,rateSwap = rsM 
                      ,rateCap = rcM 
                      ,triggers = triggerM
                      ,ledgers = ledgerM
                      ,pool = pool 
                      ,dates = dates
                      ,status = status} 
  = let 
      accKeys = Map.keysSet accM
      bndKeys = Map.keysSet bondM 
      feeKeys = Map.keysSet feeM
      waterfallKeys = Map.keysSet waterfallM
      liqProviderKeys = maybe Set.empty Map.keysSet liqProviderM
      rateSwapKeys = maybe Set.empty Map.keysSet rsM
      rateCapKeys = maybe Set.empty Map.keysSet rcM
      ledgerKeys = maybe Set.empty Map.keysSet ledgerM
      triggerKeys = maybe Set.empty Map.keysSet triggerM
      -- date check

      -- issuance balance check 
      issuanceBalCheck CurrentDates {} = let 
                                           stats = Map.elems $ getIssuanceStats t Nothing
                                           lookupResult = Map.lookup IssuanceBalance <$> stats
                                         in
                                           if all isNothing lookupResult then
                                             [ErrorMsg "Issuance balance not found for a Ongoing Deal"]
                                           else
                                             []
      issuanceBalCheck _ = []

      -- collection rule check
      aggRuleResult = validateAggRule aggRule
      -- TODO : collectCash shouldn't overlap with others

      -- waterfall key not exists test error
      errors = concat $ (\x -> validateAction x [] accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys rateCapKeys ledgerKeys) <$> Map.elems waterfallM 

      -- waterfall action coverage check 

      -- run result scan

      allErrors = errors ++ issuanceBalCheck dates ++ aggRuleResult
      -- check issuance balance 
      
      w1 = if (not (isPreClosing t)) && (length (Map.elems (getIssuanceStats t Nothing))) == 0 then
             [WarningMsg "Deal passes PreClosing status, but not cumulative defaults/delinq at cutoff date?"]
           else 
             []
      warnings = w1
    in 
      (warnings,allErrors) -- Valiation Pass

validateRun :: TestDeal a -> [ResultComponent]
validateRun t@TestDeal{waterfall=waterfallM
                      ,accounts =accM 
                      ,fees = feeM 
                      ,bonds = bondM 
                      ,collects = aggRule 
                      ,liqProvider = liqProviderM 
                      ,rateSwap = rsM 
                      ,triggers = triggerM
                      ,ledgers = ledgerM} 
  = let 
      -- oustanding liability
      --- bond
      bondWarnings = [ WarningMsg ("Bond "++bn++ " is not paid off")  | bn <- Map.elems (Map.map L.bndName $ Map.filter (not . isPaidOff)  bondM) ]
      --- fee
      feeWarnings = [ WarningMsg ("Fee "++fn++ " is not paid off")  | fn <- Map.elems (Map.map F.feeName $ Map.filter (not . isPaidOff) feeM) ]
      --- liquidity provider 
      liqWarnings = case liqProviderM of 
                      Nothing -> []
                      Just liqM -> [ WarningMsg ("LiquidityProvider "++bn++ " is not paid off")  | bn <- Map.elems (Map.map CE.liqName $ Map.filter (not . isPaidOff)  liqM) ]
      --- rate swap
      rsWarnings = case rsM of 
                     Nothing -> []
                     Just rsM -> []   -- TODO [ WarningMsg ("LiquidityProvider "++bn++ " is not paid off")  | bn <- Map.elems (Map.map CE.liqName $ Map.filter (not . isPaidOff)  rsM) ]

      -- oustanding assets
      --- account
      accWarnings = [ WarningMsg ("Account "++an++ " has cash to be distributed")  | an <- Map.elems (Map.map A.accName $ Map.filter (\x -> A.accBalance x > 0) accM)]
      --- uncollected pool cash

      -- run result scan
    in 
      bondWarnings ++ feeWarnings ++ accWarnings ++ liqWarnings ++ rsWarnings
