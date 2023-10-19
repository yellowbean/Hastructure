{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Deal.DealValidation (validateRun,validatePreRun)
  where 

import Deal.DealBase
import Types
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.ByteString (intercalate)
import qualified Types as Set

import qualified Waterfall as W
import qualified Liability as L
import qualified Accounts as A
import qualified Expense as F

validateAction :: [W.Action] -> [ResultComponent] -> Set.Set String -> Set.Set String -> Set.Set String-> Set.Set String-> Set.Set String -> Set.Set String -> [ResultComponent]
validateAction [] rs _ _ _ _ _ _ = rs
validateAction ((W.Transfer _ acc1 acc2 _):as) rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys 
  | Set.notMember acc1 accKeys || Set.notMember acc2 accKeys 
    = validateAction as (rs ++ [ErrorMsg (acc1 ++","++acc2++" not in "++ show accKeys)]) accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys
  | otherwise = validateAction as rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys

validateAction ((W.CalcFee fees):as) rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys 
  | not (Set.isSubsetOf (Set.fromList fees) feeKeys)
    = validateAction as (rs ++ [ErrorMsg (show fees ++ " not in "++ show feeKeys)]) accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys
  | otherwise = validateAction as rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys

validateAction ((W.PayFee _ accName fees _):as) rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys
  | not (Set.isSubsetOf (Set.fromList fees) feeKeys) || Set.notMember accName accKeys
    = validateAction as (rs ++ [ErrorMsg (show fees ++ " not in "++ show feeKeys++" Or "++ show accName ++" not in "++show accKeys)]) accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys
  | otherwise = validateAction as rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys

validateAction ((W.CalcAndPayFee _ accName fees _):as) rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys
  | not (Set.isSubsetOf (Set.fromList fees) feeKeys) || Set.notMember accName accKeys
    = validateAction as (rs ++ [ErrorMsg (show fees ++ " not in "++ show feeKeys++" Or "++ accName ++" not in "++ show accKeys)]) accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys
  | otherwise = validateAction as rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys

validateAction ((W.PayFeeResidual _ accName feeName):as) rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys
  | Set.notMember feeName feeKeys || Set.notMember accName accKeys
    = validateAction as (rs ++ [ErrorMsg (feeName ++ " not in "++ show feeKeys++" Or "++accName++ " not in "++show accKeys)]) accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys
  | otherwise = validateAction as rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys

validateAction ((W.CalcBondInt bnds):as) rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys
  | not (Set.isSubsetOf (Set.fromList bnds) bndKeys)
    = validateAction as (rs ++ [ErrorMsg (show bnds ++ " not in "++ show bndKeys)]) accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys
  | otherwise = validateAction as rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys

validateAction ((W.PayInt _ accName bnds _):as) rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys
  | not (Set.isSubsetOf (Set.fromList bnds) bndKeys) || Set.notMember accName accKeys
    = validateAction as (rs ++ [ErrorMsg (show bnds ++ " not in "++ show bndKeys++" Or "++accName++" not in "++show accKeys)]) accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys
  | otherwise = validateAction as rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys

validateAction ((W.AccrueAndPayInt _ accName bnds _):as) rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys
  | not (Set.isSubsetOf (Set.fromList bnds) bndKeys) || Set.notMember accName accKeys
    = validateAction as (rs ++ [ErrorMsg (show bnds ++ " not in "++ show bndKeys++" Or "++accName++" not in "++show accKeys)]) accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys
  | otherwise = validateAction as rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys

validateAction ((W.PayIntResidual _ accName bndName):as) rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys
  | Set.notMember bndName bndKeys || Set.notMember accName accKeys
    = validateAction as (rs ++ [ErrorMsg (bndName ++ " not in "++ show bndKeys++" Or "++accName++" not in "++show accKeys)]) accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys
  | otherwise = validateAction as rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys

validateAction ((W.PayPrin _ accName bnds _):as) rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys
  | not (Set.isSubsetOf (Set.fromList bnds) bndKeys) || Set.notMember accName accKeys
    = validateAction as (rs ++ [ErrorMsg (show bnds++ " not in "++ show bndKeys++" Or "++accName++" not in "++show accKeys)]) accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys
  | otherwise = validateAction as rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys

validateAction ((W.PayPrinResidual accName bnds):as) rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys
  | not (Set.isSubsetOf (Set.fromList bnds) bndKeys) || Set.notMember accName accKeys
    = validateAction as (rs ++ [ErrorMsg (show bnds++ " not in "++ show bndKeys++" Or "++accName++" not in "++show accKeys)]) accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys
  | otherwise = validateAction as rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys

validateAction ((W.BuyAsset _ _ accName):as) rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys
  | Set.notMember accName accKeys = validateAction as (rs ++ [ErrorMsg (accName++" not in "++show accKeys)]) accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys
  | otherwise = validateAction as rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys

validateAction ((W.LiquidatePool _ accName):as) rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys
  | Set.notMember accName accKeys = validateAction as (rs ++ [ErrorMsg (accName++" not in "++show accKeys)]) accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys
  | otherwise = validateAction as rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys

validateAction ((W.LiqSupport _ liqName _ accName):as) rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys
  | Set.notMember accName accKeys || Set.notMember liqName liqProviderKeys 
    = validateAction as (rs ++ [ErrorMsg (accName++" not in "++show accKeys++" Or "++liqName ++" not in "++ show liqProviderKeys)]) accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys
  | otherwise = validateAction as rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys

validateAction ((W.LiqRepay _ _ accName liqName):as) rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys
  | Set.notMember accName accKeys || Set.notMember liqName liqProviderKeys 
    = validateAction as (rs ++ [ErrorMsg (accName++" not in "++show accKeys++" Or "++liqName ++" not in "++ show liqProviderKeys)]) accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys
  | otherwise = validateAction as rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys

validateAction ((W.LiqYield _ accName liqName):as) rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys
  | Set.notMember accName accKeys || Set.notMember liqName liqProviderKeys 
    = validateAction as (rs ++ [ErrorMsg (accName++" not in "++show accKeys++" Or "++liqName ++" not in "++ show liqProviderKeys)]) accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys
  | otherwise = validateAction as rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys

validateAction ((W.LiqAccrue liqName):as) rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys
  | Set.notMember liqName liqProviderKeys 
    = validateAction as (rs ++ [ErrorMsg (liqName ++" not in "++ show liqProviderKeys)]) accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys
  | otherwise = validateAction as rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys

validateAction ((W.LiqAccrue liqName):as) rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys
  | Set.notMember liqName liqProviderKeys 
    = validateAction as (rs ++ [ErrorMsg (liqName ++" not in "++ show liqProviderKeys)]) accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys
  | otherwise = validateAction as rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys

validateAction ((W.SwapAccrue rsName):as) rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys
  | Set.notMember rsName rateSwapKeys
    = validateAction as (rs ++ [ErrorMsg (rsName ++" not in "++ show rateSwapKeys)]) accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys
  | otherwise = validateAction as rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys

validateAction ((W.SwapReceive accName rsName):as) rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys
  | Set.notMember rsName rateSwapKeys || Set.notMember accName accKeys
    = validateAction as (rs ++ [ErrorMsg (rsName ++" not in "++ show rateSwapKeys ++ " Or "++ accName ++ " not in "++ show accKeys)]) accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys
  | otherwise = validateAction as rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys

validateAction ((W.SwapPay accName rsName):as) rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys
  | Set.notMember rsName rateSwapKeys || Set.notMember accName accKeys
    = validateAction as (rs ++ [ErrorMsg (rsName ++" not in "++ show rateSwapKeys ++ " Or "++ accName ++ " not in "++ show accKeys)]) accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys
  | otherwise = validateAction as rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys

validateAction ((W.SwapSettle accName rsName):as) rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys
  | Set.notMember rsName rateSwapKeys || Set.notMember accName accKeys
    = validateAction as (rs ++ [ErrorMsg (rsName ++" not in "++ show rateSwapKeys ++ " Or "++ accName ++ " not in "++ show accKeys)]) accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys
  | otherwise = validateAction as rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys

validateAction ((W.ActionWithPre p subActionList):as) rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys
  = validateAction (subActionList++as) rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys

validateAction ((W.ActionWithPre2 p subActionList1 subActionList2):as) rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys
  = validateAction (subActionList1++subActionList2++as) rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys

validateAction (action:as) rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys
  = validateAction as rs accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys

validatePreRun :: TestDeal a -> (Bool,[ResultComponent])
validatePreRun t@TestDeal{waterfall=waterfallM
                      ,accounts =accM 
                      ,fees = feeM 
                      ,bonds = bondM 
                      ,collects = aggRule 
                      ,liqProvider = liqProviderM 
                      ,rateSwap = rsM 
                      ,triggers = triggerM
                      ,ledgers = ledgerM} 
  = let 
      errors = []
      warnings = []
      flag = True
      -- date check

      accKeys = Map.keysSet accM
      bndKeys = Map.keysSet bondM 
      feeKeys = Map.keysSet feeM
      waterfallKeys = Map.keysSet waterfallM
      liqProviderKeys = maybe Set.empty Map.keysSet liqProviderM
      rateSwapKeys = maybe Set.empty Map.keysSet rsM
      ledgerKeys = maybe Set.empty Map.keysSet ledgerM
      triggerKeys = maybe Set.empty Map.keysSet triggerM

      -- waterfall key not exists test error
      errors2 = concat $ (\x -> validateAction x [] accKeys bndKeys feeKeys liqProviderKeys rateSwapKeys ledgerKeys) <$> Map.elems waterfallM 

      -- waterfall action coverage check 

      -- run result scan
    in 
      if null errors2 then 
        (True, warnings) -- Valiation Pass
      else 
        (False, errors2 ++ warnings) -- Validation Failed

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
      
      --- rate swap
      -- oustanding assets
      --- account
      accWarnings = [ WarningMsg ("Account "++an++ " has cash to be distributed")  | an <- Map.elems (Map.map A.accName $ Map.filter (\x -> A.accBalance x > 0) accM)]
      --- uncollected pool cash

      -- run result scan
    in 
      bondWarnings ++ feeWarnings ++ accWarnings