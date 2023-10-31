{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module AssetClass.MixedAsset
  (projAssetUnion,projAssetUnionList,projectCashflow)
  where

import qualified Data.Time as T
import qualified Cashflow as CF -- (Cashflow,Amount,Interests,Principals)
import qualified Assumptions as A
import qualified AssetClass.AssetBase as ACM
import InterestRate
import qualified Asset as P
import Lib
import Util
import DateUtil
import Types
import qualified Data.Map as Map
import Data.List
import Data.Maybe
import Data.Aeson hiding (json)
import Language.Haskell.TH
import Data.Aeson.TH
import Data.Aeson.Types
import GHC.Generics

import AssetClass.AssetBase
import AssetClass.Mortgage
import AssetClass.Lease
import AssetClass.Loan
import AssetClass.Installment
import AssetClass.AssetCashflow

import Debug.Trace
import Assumptions (AssetDefaultAssumption(DefaultCDR))
import qualified Asset as Ast



instance P.Asset MixedAsset where

  getCurrentBal ma = 0

  getOriginBal ma = 0

  getOriginRate ma = 0

  calcCashflow pl asOfDay mRates 
    = CF.CashFlowFrame []
  
projAssetUnion :: ACM.AssetUnion -> Date -> A.AssetPerf -> Maybe [RateAssumption] -> (CF.CashFlowFrame, Map.Map CutoffFields Balance)
projAssetUnion (ACM.MO ast) d assumps mRates = P.projCashflow ast d assumps mRates
projAssetUnion (ACM.LO ast) d assumps mRates = P.projCashflow ast d assumps mRates
projAssetUnion (ACM.IL ast) d assumps mRates = P.projCashflow ast d assumps mRates
projAssetUnion (ACM.LS ast) d assumps mRates = P.projCashflow ast d assumps mRates

projAssetUnionList :: [ACM.AssetUnion] -> Date -> A.ApplyAssumptionType -> Maybe [RateAssumption] -> (CF.CashFlowFrame, Map.Map CutoffFields Balance)
projAssetUnionList assets d (A.PoolLevel assetPerf) mRate =
  let 
    results = [ projAssetUnion asset d assetPerf mRate | asset <- assets ]
    cfs = fst <$> results
    bals = snd <$> results
  in 
    (foldl1 CF.mergePoolCf cfs, Map.unionsWith (+) bals)

projAssetUnionList assets d _ mRate = error " not implemented on asset level assumption for revolving pool"


projectCashflow :: MixedAsset -> Date -> Map.Map String A.ApplyAssumptionType -> Maybe [RateAssumption] -> Map.Map String (CF.CashFlowFrame, Map.Map CutoffFields Balance)
projectCashflow (MixedPool assetMap) asOfDate mAssump mRate 
  = let 
      mWithCf = Map.mapWithKey
                  (\k astList -> projAssetUnionList 
                                   astList 
                                   asOfDate
                                   (case Map.lookup k mAssump of 
                                      Just assump -> assump
                                      Nothing -> error ("Failed to read sub assump:"++k))
                                   mRate)
                  assetMap
    in 
      mWithCf