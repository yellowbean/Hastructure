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

import qualified Waterfall as W
import qualified CreditEnhancement as CE
import qualified Liability as L
import qualified Accounts as A
import qualified Expense as F
import qualified Asset as P
import qualified Assumptions as AP
import qualified InterestRate as IR

import Control.Lens hiding (element)
import Control.Lens.TH

import Data.Maybe
import qualified Assumptions as A


import Debug.Trace
debug = flip trace

isPreClosing :: TestDeal a -> Bool
isPreClosing t@TestDeal{ status = PreClosing _ } = True
isPreClosing _ = False

validateAction :: [W.Action] -> [ResultComponent] -> Set.Set String -> Set.Set String -> Set.Set String -> Set.Set String-> Set.Set String-> Set.Set String -> Set.Set String -> Set.Set String -> Set.Set String -> Set.Set PoolId -> [ResultComponent]
validateAction [] rs _ _ _ _ _ _ _ _ _ _ = rs
validateAction ((W.Transfer _ acc1 acc2 _):as) rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | Set.notMember acc1 accKeys || Set.notMember acc2 accKeys 
    = validateAction as (rs ++ [ErrorMsg (acc1 ++","++acc2++" not in "++ show accKeys)]) accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | otherwise = validateAction as rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys

validateAction ((W.CalcFee fees):as) rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | not (Set.isSubsetOf (Set.fromList fees) feeKeys)
    = validateAction as (rs ++ [ErrorMsg (show fees ++ " not in "++ show feeKeys)]) accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | otherwise = validateAction as rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys

validateAction ((W.PayFee _ accName fees _):as) rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | not (Set.isSubsetOf (Set.fromList fees) feeKeys) || Set.notMember accName accKeys
    = validateAction as (rs ++ [ErrorMsg (show fees ++ " not in "++ show feeKeys++" Or "++ show accName ++" not in "++show accKeys)]) accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys  rPoolKeys poolKeys
  | otherwise = validateAction as rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys  rPoolKeys poolKeys

validateAction ((W.CalcAndPayFee _ accName fees _):as) rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | not (Set.isSubsetOf (Set.fromList fees) feeKeys) || Set.notMember accName accKeys
    = validateAction as (rs ++ [ErrorMsg (show fees ++ " not in "++ show feeKeys++" Or "++ accName ++" not in "++ show accKeys)]) accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | otherwise = validateAction as rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys

validateAction ((W.PayFeeResidual _ accName feeName):as) rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | Set.notMember feeName feeKeys || Set.notMember accName accKeys
    = validateAction as (rs ++ [ErrorMsg (feeName ++ " not in "++ show feeKeys++" Or "++accName++ " not in "++show accKeys)]) accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | otherwise = validateAction as rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys

validateAction ((W.CalcBondInt bnds _ _):as) rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | not (Set.isSubsetOf (Set.fromList bnds) bndKeys) = validateAction as (rs ++ [ErrorMsg (show bnds ++ " not in "++ show bndKeys)]) accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | otherwise = validateAction as rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys

validateAction ((W.PayInt _ accName bnds _):as) rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | not (Set.isSubsetOf (Set.fromList bnds) bndKeys) || Set.notMember accName accKeys
    = validateAction as (rs ++ [ErrorMsg (show bnds ++ " not in "++ show bndKeys++" Or "++accName++" not in "++show accKeys)]) accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | otherwise = validateAction as rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys

validateAction ((W.PayIntBySeq _ accName bndNames _):as) rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | not (Set.isSubsetOf (Set.fromList bndNames) bndKeys) || Set.notMember accName accKeys
    = validateAction as (rs ++ [ErrorMsg (show bndNames ++ " not in "++ show bndKeys++" Or "++accName++" not in "++show accKeys)]) accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | otherwise = validateAction as rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys

validateAction ((W.AccrueAndPayIntBySeq _ accName bndNames _):as) rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | not (Set.isSubsetOf (Set.fromList bndNames) bndKeys) || Set.notMember accName accKeys
    = validateAction as (rs ++ [ErrorMsg (show bndNames ++ " not in "++ show bndKeys++" Or "++accName++" not in "++show accKeys)]) accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | otherwise = validateAction as rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys

validateAction ((W.PayIntOverIntBySeq _ accName bnds _):as) rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | not (Set.isSubsetOf (Set.fromList bnds) bndKeys) || Set.notMember accName accKeys
    = validateAction as (rs ++ [ErrorMsg (show bnds ++ " not in "++ show bndKeys++" Or "++accName++" not in "++show accKeys)]) accKeys bndKeys bgNames  feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | otherwise = validateAction as rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys

validateAction ((W.PayIntOverInt _ accName bnds _):as) rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | not (Set.isSubsetOf (Set.fromList bnds) bndKeys) || Set.notMember accName accKeys
    = validateAction as (rs ++ [ErrorMsg (show bnds ++ " not in "++ show bndKeys++" Or "++accName++" not in "++show accKeys)]) accKeys bndKeys bgNames  feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | otherwise = validateAction as rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys

validateAction ((W.AccrueAndPayInt _ accName bnds _):as) rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | not (Set.isSubsetOf (Set.fromList bnds) bndKeys) || Set.notMember accName accKeys
    = validateAction as (rs ++ [ErrorMsg (show bnds ++ " not in "++ show bndKeys++" Or "++accName++" not in "++show accKeys)]) accKeys bndKeys bgNames  feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | otherwise = validateAction as rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys

validateAction ((W.PayIntResidual _ accName bndName):as) rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | Set.notMember bndName bndKeys || Set.notMember accName accKeys
    = validateAction as (rs ++ [ErrorMsg (bndName ++ " not in "++ show bndKeys++" Or "++accName++" not in "++show accKeys)]) accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | otherwise = validateAction as rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys

validateAction ((W.PayPrin _ accName bnds _):as) rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys  ledgerKeys rPoolKeys poolKeys
  | not (Set.isSubsetOf (Set.fromList bnds) bndKeys) || Set.notMember accName accKeys
    = validateAction as (rs ++ [ErrorMsg (show bnds++ " not in "++ show bndKeys++" Or "++accName++" not in "++show accKeys)]) accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | otherwise = validateAction as rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys

validateAction ((W.PayPrinResidual accName bnds):as) rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys  ledgerKeys rPoolKeys poolKeys
  | not (Set.isSubsetOf (Set.fromList bnds) bndKeys) || Set.notMember accName accKeys
    = validateAction as (rs ++ [ErrorMsg (show bnds++ " not in "++ show bndKeys++" Or "++accName++" not in "++show accKeys)]) accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | otherwise = validateAction as rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys

validateAction ((W.PayPrinWithDue accName bnds _):as) rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys  ledgerKeys rPoolKeys poolKeys
  | not (Set.isSubsetOf (Set.fromList bnds) bndKeys) || Set.notMember accName accKeys
    = validateAction as (rs ++ [ErrorMsg (show bnds++ " not in "++ show bndKeys++" Or "++accName++" not in "++show accKeys)]) accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | otherwise = validateAction as rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys

validateAction ((W.PayPrinBySeq _ accName bnds _):as) rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys  ledgerKeys rPoolKeys poolKeys
  | not (Set.isSubsetOf (Set.fromList bnds) bndKeys) || Set.notMember accName accKeys
    = validateAction as (rs ++ [ErrorMsg (show bnds++ " not in "++ show bndKeys++" Or "++accName++" not in "++show accKeys)]) accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | otherwise = validateAction as rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys

validateAction ((W.PayPrinGroup _ accName bg _ _):as) rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys  ledgerKeys rPoolKeys poolKeys
  | Set.notMember bg bgNames || Set.notMember accName accKeys
    = validateAction as (rs ++ [ErrorMsg (show bg++ " not in "++ show bgNames ++" Or "++accName++" not in "++show accKeys)]) accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | otherwise = validateAction as rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys

validateAction ((W.AccrueAndPayIntGroup _ accName bg _ _):as) rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys  ledgerKeys rPoolKeys poolKeys
  | Set.notMember bg bgNames || Set.notMember accName accKeys
    = validateAction as (rs ++ [ErrorMsg (show bg++ " not in "++ show bgNames ++" Or "++accName++" not in "++show accKeys)]) accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | otherwise = validateAction as rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys

validateAction ((W.PayIntGroup _ accName bg _ _):as) rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys  ledgerKeys rPoolKeys poolKeys
  | Set.notMember bg bgNames || Set.notMember accName accKeys
    = validateAction as (rs ++ [ErrorMsg (show bg++ " not in "++ show bgNames ++" Or "++accName++" not in "++show accKeys)]) accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | otherwise = validateAction as rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys

validateAction ((W.AccrueIntGroup bgs ):as) rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys  ledgerKeys rPoolKeys poolKeys
  | not (Set.isSubsetOf (Set.fromList bgs) bgNames) = validateAction as (rs ++ [ErrorMsg (show bgs++ " not in "++ show bgNames)]) accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | otherwise = validateAction as rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys

validateAction ((W.PayPrinResidual accName bnds):as) rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | not (Set.isSubsetOf (Set.fromList bnds) bndKeys) || Set.notMember accName accKeys
    = validateAction as (rs ++ [ErrorMsg (show bnds++ " not in "++ show bndKeys++" Or "++accName++" not in "++show accKeys)]) accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | otherwise = validateAction as rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys

validateAction ((W.BuyAsset _ _ accName _):as) rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | Set.notMember accName accKeys = validateAction as (rs ++ [ErrorMsg (accName++" not in "++show accKeys)]) accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | otherwise = validateAction as rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys

validateAction ((W.BuyAssetFrom _ _ accName mRPoolName mPid):as) rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | Set.notMember accName accKeys = validateAction as (rs ++ [ErrorMsg (accName++" not in "++show accKeys)]) accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | Set.notMember (fromMaybe PoolConsol mPid) poolKeys = validateAction as (rs ++ [ErrorMsg (show mPid++" not in "++show poolKeys)]) accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | otherwise = validateAction as rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys

validateAction ((W.PayPrinBySeq _ accName bnds _):as) rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys  ledgerKeys rPoolKeys poolKeys
  | not (Set.isSubsetOf (Set.fromList bnds) bndKeys) || Set.notMember accName accKeys
    = validateAction as (rs ++ [ErrorMsg (show bnds++ " not in "++ show bndKeys++" Or "++accName++" not in "++show accKeys)]) accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | otherwise = validateAction as rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys

validateAction ((W.PayPrinGroup _ accName bg _ _):as) rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys  ledgerKeys rPoolKeys poolKeys
  | Set.notMember bg bgNames || Set.notMember accName accKeys
    = validateAction as (rs ++ [ErrorMsg (show bg++ " not in "++ show bgNames ++" Or "++accName++" not in "++show accKeys)]) accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | otherwise = validateAction as rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys

validateAction ((W.AccrueAndPayIntGroup _ accName bg _ _):as) rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys  ledgerKeys rPoolKeys poolKeys
  | Set.notMember bg bgNames || Set.notMember accName accKeys
    = validateAction as (rs ++ [ErrorMsg (show bg++ " not in "++ show bgNames ++" Or "++accName++" not in "++show accKeys)]) accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | otherwise = validateAction as rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys

validateAction ((W.PayIntGroup _ accName bg _ _):as) rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys  ledgerKeys rPoolKeys poolKeys
  | Set.notMember bg bgNames || Set.notMember accName accKeys
    = validateAction as (rs ++ [ErrorMsg (show bg++ " not in "++ show bgNames ++" Or "++accName++" not in "++show accKeys)]) accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | otherwise = validateAction as rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys

validateAction ((W.AccrueIntGroup bgs ):as) rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys  ledgerKeys rPoolKeys poolKeys
  | not (Set.isSubsetOf (Set.fromList bgs) bgNames) = validateAction as (rs ++ [ErrorMsg (show bgs++ " not in "++ show bgNames)]) accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | otherwise = validateAction as rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys

validateAction ((W.PayPrinResidual accName bnds):as) rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | not (Set.isSubsetOf (Set.fromList bnds) bndKeys) || Set.notMember accName accKeys
    = validateAction as (rs ++ [ErrorMsg (show bnds++ " not in "++ show bndKeys++" Or "++accName++" not in "++show accKeys)]) accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | otherwise = validateAction as rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys

validateAction ((W.BuyAsset _ _ accName _):as) rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | Set.notMember accName accKeys = validateAction as (rs ++ [ErrorMsg (accName++" not in "++show accKeys)]) accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | otherwise = validateAction as rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys

validateAction ((W.BuyAssetFrom _ _ accName mRPoolName mPid):as) rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | Set.notMember accName accKeys = validateAction as (rs ++ [ErrorMsg (accName++" not in "++show accKeys)]) accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | Set.notMember (fromMaybe PoolConsol mPid) poolKeys = validateAction as (rs ++ [ErrorMsg (show mPid++" not in "++show poolKeys)]) accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | otherwise = validateAction as rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys

validateAction ((W.LiquidatePool _ accName mPids):as) rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | Set.notMember accName accKeys = validateAction as (rs ++ [ErrorMsg (accName++" not in "++show accKeys)]) accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | isJust mPids && not (Set.isSubsetOf (Set.fromList (fromMaybe [] mPids)) poolKeys) = validateAction as (rs ++ [ErrorMsg (show mPids++" not in "++show poolKeys)]) accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | otherwise = validateAction as rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys

validateAction ((W.LiqSupport _ liqName CE.LiqToAcc [accName]):as) rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | Set.notMember accName accKeys || Set.notMember liqName liqProviderKeys 
    = validateAction as (rs ++ [ErrorMsg (show accName++" not in "++show accKeys++" Or "++liqName ++" not in "++ show liqProviderKeys)]) accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | otherwise = validateAction as rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys

validateAction ((W.LiqSupport _ liqName CE.LiqToFee feeNames):as) rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | not (Set.isSubsetOf (Set.fromList feeNames) feeKeys) || Set.notMember liqName liqProviderKeys 
    = validateAction as (rs ++ [ErrorMsg (show feeNames++" not in "++show feeKeys++" Or "++liqName ++" not in "++ show liqProviderKeys)]) accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | otherwise = validateAction as rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys

validateAction ((W.LiqSupport _ liqName CE.LiqToBondInt bndNames):as) rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | not (Set.isSubsetOf (Set.fromList bndNames) bndKeys) || Set.notMember liqName liqProviderKeys 
    = validateAction as (rs ++ [ErrorMsg (show bndNames++" not in "++show bndKeys++" Or "++liqName ++" not in "++ show liqProviderKeys)]) accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | otherwise = validateAction as rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys

validateAction ((W.LiqRepay _ _ accName liqName):as) rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | Set.notMember accName accKeys || Set.notMember liqName liqProviderKeys 
    = validateAction as (rs ++ [ErrorMsg (accName++" not in "++show accKeys++" Or "++liqName ++" not in "++ show liqProviderKeys)]) accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | otherwise = validateAction as rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys

validateAction ((W.LiqYield _ accName liqName):as) rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | Set.notMember accName accKeys || Set.notMember liqName liqProviderKeys 
    = validateAction as (rs ++ [ErrorMsg (accName++" not in "++show accKeys++" Or "++liqName ++" not in "++ show liqProviderKeys)]) accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | otherwise = validateAction as rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys

validateAction ((W.LiqAccrue liqNames):as) rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | not (Set.isSubsetOf (Set.fromList liqNames) liqProviderKeys)
    = validateAction as (rs ++ [ErrorMsg (show liqNames ++" not in "++ show liqProviderKeys)]) accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | otherwise = validateAction as rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys

validateAction ((W.LiqAccrue liqNames):as) rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | not (Set.isSubsetOf (Set.fromList liqNames) liqProviderKeys) 
    = validateAction as (rs ++ [ErrorMsg (show liqNames ++" not in "++ show liqProviderKeys)]) accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | otherwise = validateAction as rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys

validateAction ((W.SwapAccrue rsName):as) rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | Set.notMember rsName rateSwapKeys
    = validateAction as (rs ++ [ErrorMsg (rsName ++" not in "++ show rateSwapKeys)]) accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | otherwise = validateAction as rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys

validateAction ((W.SwapReceive accName rsName):as) rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | Set.notMember rsName rateSwapKeys || Set.notMember accName accKeys
    = validateAction as (rs ++ [ErrorMsg (rsName ++" not in "++ show rateSwapKeys ++ " Or "++ accName ++ " not in "++ show accKeys)]) accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | otherwise = validateAction as rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys

validateAction ((W.SwapPay accName rsName):as) rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | Set.notMember rsName rateSwapKeys || Set.notMember accName accKeys
    = validateAction as (rs ++ [ErrorMsg (rsName ++" not in "++ show rateSwapKeys ++ " Or "++ accName ++ " not in "++ show accKeys)]) accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | otherwise = validateAction as rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys

validateAction ((W.SwapSettle accName rsName):as) rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | Set.notMember rsName rateSwapKeys || Set.notMember accName accKeys
    = validateAction as (rs ++ [ErrorMsg (rsName ++" not in "++ show rateSwapKeys ++ " Or "++ accName ++ " not in "++ show accKeys)]) accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | otherwise = validateAction as rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys

validateAction ((W.FundWith _ accName bName):as) rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | Set.notMember bName bndKeys || Set.notMember accName accKeys
    = validateAction as (rs ++ [ErrorMsg (bName ++" not in "++ show bndKeys ++ " Or "++ accName ++ " not in "++ show accKeys)]) accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | otherwise = validateAction as rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys

validateAction ((W.WriteOff _ bName):as) rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | Set.notMember bName bndKeys = validateAction as (rs ++ [ErrorMsg (bName ++" not in "++ show bndKeys )]) accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | otherwise = validateAction as rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys

validateAction ((W.CollectRateCap accName rcName):as) rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | Set.notMember rcName rcKeys || Set.notMember accName accKeys
    = validateAction as (rs ++ [ErrorMsg (rcName ++" not in "++ show rcKeys ++ " Or "++ accName ++ " not in "++ show accKeys)]) accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  | otherwise = validateAction as rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys

validateAction ((W.ActionWithPre p subActionList):as) rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  = validateAction (subActionList++as) rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys

validateAction ((W.ActionWithPre2 p subActionList1 subActionList2):as) rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  = validateAction (subActionList1++subActionList2++as) rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys

validateAction (action:as) rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys
  = validateAction as rs accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rcKeys ledgerKeys rPoolKeys poolKeys

extractRequiredRates :: (P.Asset a,IR.UseRate a) => TestDeal a -> Set.Set Types.Index
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
validateAggRule :: [W.CollectionRule] -> [PoolId] -> [ResultComponent]
validateAggRule rules validPids =
    [ ErrorMsg ("Pool source "++show ps++" has a weight of "++show r)   | ((pid,ps),r) <- Map.toList oustandingPs ] ++
    [ ErrorMsg ("Pool Id not found"++show ospid++" in "++ show validPids) | ospid <- osPid ]
  where 
    countWeight (W.Collect (Just pids) ps _) =  Map.fromList [((pid,ps),1.0) | pid <- pids]
    countWeight (W.Collect Nothing ps _) =  Map.fromList [((PoolConsol,ps),1.0)]
    countWeight (W.CollectByPct (Just pids) ps lst) = Map.fromList [((pid,ps), pct) | pid <- pids, pct <- fst <$> lst]
    countWeight (W.CollectByPct Nothing ps lst) = Map.fromList [((PoolConsol, ps),pct)| pct <- fst <$> lst]
    
    sumMap = foldl1 (Map.unionWith (+)) $ countWeight <$> rules  
    oustandingPs = Map.filter (> 1.0) sumMap

    getPids (W.Collect (Just pids) _ _) = pids  
    getPids (W.Collect Nothing ps _) = [PoolConsol]
    getPids (W.CollectByPct (Just pids) _ _) = pids
    getPids (W.CollectByPct Nothing _ _ ) = [PoolConsol]
    osPid = Set.elems $ Set.difference (Set.fromList (concat (getPids <$> rules))) (Set.fromList validPids)


validateFee :: F.Fee -> [ResultComponent]
-- validateFee (F.Fee fn (F.AnnualRateFee (CurrentBondBalanceOf _) _) _ _ _ _ _ _) = [] 
-- validateFee (F.Fee fn (F.AnnualRateFee (OriginalBondBalanceOf _) _) _ _ _ _ _ _) = [] 
-- validateFee (F.Fee fn (F.AnnualRateFee (CurrentPoolBalance _) _) _ _ _ _ _ _) = [] 
-- validateFee (F.Fee fn (F.AnnualRateFee (OriginalPoolBalance _) _) _ _ _ _ _ _) = [] 
-- validateFee (F.Fee fn (F.AnnualRateFee CurrentBondBalance _) _ _ _ _ _ _) = [] 
-- validateFee (F.Fee fn (F.AnnualRateFee OriginalBondBalance _) _ _ _ _ _ _) = [] 
-- validateFee (F.Fee fn (F.AnnualRateFee ds _) _ _ _ _ _ _ )
--   = [ErrorMsg ("Fee Name "++fn++" has an unsupported base "++show ds)]
validateFee _ = []

--- get required pool id and required revolving pool name
extractRequiredRevolvingPool :: P.Asset a => TestDeal a -> (Set.Set PoolId, Set.Set String)
extractRequiredRevolvingPool t@TestDeal{waterfall = waterfallM} = 
  let 
    poolIds = Set.fromList $ getPoolIds t
    extract accPoolIds accRpoolNames [] = (accPoolIds,accRpoolNames)
    extract accPoolIds accRpoolNames ((W.BuyAsset _ _ _ mPoolId):as) = 
      extract (Set.insert (fromMaybe PoolConsol mPoolId) accPoolIds) accRpoolNames as
    extract accPoolIds accRpoolNames ((W.BuyAssetFrom _ _ _ rPoolName mPoolId):as) = 
      extract (Set.insert (fromMaybe PoolConsol mPoolId) accPoolIds)
              (Set.insert (fromMaybe "Consol" rPoolName) accRpoolNames)
              as
    extract accPoolIds accRpoolNames ((W.ActionWithPre _ subActions):as) = 
      let 
        (subAccPoolIds,subAccRPoolNames) = extract accPoolIds accRpoolNames subActions
      in 
        extract (accPoolIds <> subAccPoolIds) (accRpoolNames <> subAccRPoolNames) as
    extract accPoolIds accRpoolNames ((W.ActionWithPre2 _ subActionsA subActionsB):as) = 
      let 
        (subAccPoolIdsA,subAccRPoolNamesA) = extract accPoolIds accRpoolNames subActionsA
        (subAccPoolIdsB,subAccRPoolNamesB) = extract subAccPoolIdsA subAccRPoolNamesA subActionsB
      in 
        extract subAccPoolIdsB subAccRPoolNamesB as
    extract accPoolIds accRpoolNames (_:as) = extract accPoolIds accRpoolNames as
    requiredByWaterfall = Map.elems $ Map.map (extract (Set.fromList []) (Set.fromList [])) waterfallM
  in 
    (Set.unions $ fst <$> requiredByWaterfall, Set.unions $ snd <$> requiredByWaterfall)


validateReq :: (IR.UseRate a,P.Asset a) => TestDeal a -> AP.NonPerfAssumption -> (Bool,[ResultComponent])
validateReq t@TestDeal{accounts = accMap, fees = feeMap} 
            assump@A.NonPerfAssumption{A.interest = intM, A.issueBondSchedule = mIssuePlan, A.revolving = mRevolvingAssump} 
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

      bgNamesInDeal = Map.keysSet $ view dealBondGroups t
      -- fee validation 
      feeErrors = concatMap validateFee $ Map.elems feeMap
      -- issue plan validation
      issuePlanError = case mIssuePlan of 
                        Nothing -> []
                        Just issueBndEventlist
                          -> let 
                              bgNamesInAssump = Set.fromList $ [ bgName | TsPoint d (A.IssueBondEvent _ bgName _ bnd _ _) <- issueBndEventlist ]
                              bgNameErrors = [ ErrorMsg ("issueBond:Missing Bond Group Name in Deal:"++ missingBgName ) | missingBgName <- Set.elems (Set.difference bgNamesInAssump bgNamesInDeal)]

                              newBndNames = Set.fromList $ [ L.bndName bnd | TsPoint d (A.IssueBondEvent _ _ _ bnd _ _) <- issueBndEventlist ]
                              existingBndNames = Set.fromList $ L.bndName <$> viewDealAllBonds t
                              bndNameErrors = [ ErrorMsg ("issueBond:Existing Bond Name in Deal:"++ existsBndName ) | existsBndName <- Set.elems (Set.intersection newBndNames existingBndNames)]

                              acNamesInAssump = Set.fromList $ [ acName | TsPoint d (A.IssueBondEvent _ _ acName _ _ _) <- issueBndEventlist ]
                              existingAccNames = Map.keysSet accMap
                              accNameErrors = [ ErrorMsg ("issueBond:Missing Account Name in Deal:"++ missingAccName ) | missingAccName <- Set.elems (Set.difference acNamesInAssump existingAccNames)]
                              
                              bndNamesInAssump = [ L.bndName bnd | TsPoint d (A.IssueBondEvent _ bgName _ bnd _ _) <- issueBndEventlist ]
                              bndUniqNames = Set.fromList bndNamesInAssump
                              dupNamesErrors = [ ErrorMsg("Duplicate Bond Names in Funding Plan") | length bndUniqNames /= length bndNamesInAssump]
                             in 
                              bgNameErrors ++ accNameErrors ++ bndNameErrors ++ dupNamesErrors

      -- revolving buy validation
      revolvingBuyError = let 
                            (requiredPoolIds, requiredRPoolNames) =  extractRequiredRevolvingPool t
                            a = 1 
                          in 
                            case mRevolvingAssump of 
                              Nothing -> []
                              Just (A.AvailableAssets _ _ ) -> [ ErrorMsg ("BuyAsset: Missing Pool Id in assumption" ++ show x)  | x <- Set.toList (requiredPoolIds Set.\\ Set.fromList (getPoolIds t))]
                              Just (A.AvailableAssetsBy rMap ) -> [ ErrorMsg ("BuyAsset: Missing Revolving Pool in assumption" ++ show x)  | x <- Set.toList (requiredRPoolNames Set.\\ Set.fromList (Map.keys rMap))] -- `debug` ("requiredRPoolNames 0> "++ show requiredRPoolNames)


      (dealWarnings,dealErrors) = validatePreRun t 
      finalErrors = missingIndexError ++ dealErrors ++ issuePlanError ++ feeErrors ++ revolvingBuyError
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
      bgNames = Map.keysSet $ view dealBondGroups t
      feeKeys = Map.keysSet feeM
      waterfallKeys = Map.keysSet waterfallM
      liqProviderKeys = maybe Set.empty Map.keysSet liqProviderM
      rateSwapKeys = maybe Set.empty Map.keysSet rsM
      rateCapKeys = maybe Set.empty Map.keysSet rcM
      ledgerKeys = maybe Set.empty Map.keysSet ledgerM
      triggerKeys = maybe Set.empty Map.keysSet triggerM
      poolKeys = Set.fromList $ getPoolIds t
      rPoolKeys = Set.fromList [] -- $ maybe Set.empty (Set.fromList . Map.keys) pool
      poolIds = getPoolIds t 
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

      -- val on deal status and deal dates
      statusCheck (PreClosing _) PreClosingDates {} = [] 
      statusCheck (PreClosing _) _ = [ErrorMsg "Deal is in PreClosing status but it is not using preClosing dates"]
      statusCheck _ _ = []


      -- collection rule check
      aggRuleResult = if isResec t then 
                        []
                      else
                        validateAggRule aggRule poolIds 
      -- TODO : collectCash shouldn't overlap with others

      -- waterfall key not exists test error
      errors = (\x -> validateAction x [] accKeys bndKeys bgNames feeKeys liqProviderKeys rateSwapKeys rateCapKeys ledgerKeys rPoolKeys poolKeys) <$> Map.elems waterfallM 

      -- waterfall action coverage check 

      -- run result scan

      allErrors = (concat errors) ++ issuanceBalCheck dates ++ aggRuleResult ++ statusCheck status dates 
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
      bndList = viewDealAllBonds t
      -- oustanding liability
      --- bond
      bondWarnings = [ WarningMsg ("Bond "++bn++ " is not paid off")  | bn <- L.bndName <$> filter (not . isPaidOff) bndList ]
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
