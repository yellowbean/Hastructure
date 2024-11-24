{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module AssetClass.FixedAsset
  ()
  where

import qualified Data.Time as T
import Data.Ratio

import Data.Aeson hiding (json)
import Language.Haskell.TH
import Data.Maybe
import Data.List
import Data.Aeson.TH
import qualified Data.Map as Map
import Data.Aeson.Types
import GHC.Generics

import qualified Assumptions as A
import Types hiding (startDate)
import Lib
import Util
import DateUtil
import qualified Cashflow as CF

import AssetClass.AssetBase


import Debug.Trace
import AssetClass.AssetCashflow
import qualified Asset as Ast
import Asset (Asset(projCashflow))
import Assumptions (AssetDelinqPerfAssumption(DummyDelinqAssump))
debug = flip trace

calcAmortAmt ::FixedAsset -> [Balance]
calcAmortAmt fa@(FixedAsset fai@FixedAssetInfo{originBalance=ob, accRule=ar, originTerm=ot
                                               ,residualBalance=rb ,capacity=cap} rt)
  = case ar of
      StraightLine -> replicate ot $ divideBI (ob-rb) ot
      DecliningBalance -> 
        let 
          r = 1 - (realToFrac (divideBB rb ob)) ** ((1.0/(fromIntegral ot))::Double)
          remainBals = scanl (\remainBal x -> remainBal - mulBR remainBal x) ob $ replicate ot (toRational r)
        in 
          [ x-y |  (x,y) <- zip (init remainBals) (tail remainBals) ] `debug` ("remain bals"++ show remainBals)
      _ -> error ("Not implemented for depreciation rule"++show ar)
 
calcAmortBals ::FixedAsset -> [Balance]
calcAmortBals fa@(FixedAsset fai@FixedAssetInfo{originBalance=ob, accRule=ar, originTerm=ot
                                               ,residualBalance=rb ,capacity=cap} rt)
  = scanl (-) ob $ calcAmortAmt fa
 

instance Ast.Asset FixedAsset where 

  calcCashflow fa@(FixedAsset {}) asOfDay _ = 
     fst <$> projCashflow fa asOfDay (A.FixedAssetAssump (mkTs []) (mkTs []), A.DummyDelinqAssump, A.DummyDefaultAssump) Nothing

  getCurrentBal  fa@(FixedAsset fai@FixedAssetInfo{originBalance=ob, accRule=ar, originTerm=ot
                                                 ,residualBalance=rb ,capacity=cap} rt) 
    = calcAmortBals fa!!(ot-rt)

  resetToOrig fa@(FixedAsset fai@FixedAssetInfo{originBalance=ob, accRule=ar, originTerm=ot
                                                 ,residualBalance=rb ,capacity=cap} rt) 
    = FixedAsset fai ot
  
  getPaymentDates 
    (FixedAsset fo@FixedAssetInfo{startDate=sd ,period=p,originTerm=ot} rt)
    extra
    = genDates sd p (ot+extra)

  projCashflow fa@(FixedAsset fai@FixedAssetInfo{originBalance=ob, accRule=ar, originTerm=ot
                                                 ,residualBalance=rb ,capacity=cap} rt) 
               asOfDay
               (A.FixedAssetAssump uCurve pCurve,_,_)
               _
    = let 
        pdates = lastN rt $ Ast.getPaymentDates fa 0
        cfLength = length pdates 
        amortizedBals = lastN rt $ calcAmortAmt fa
        scheduleBals = lastN rt $ calcAmortBals fa
        capacityCaps = case cap of
                        FixedCapacity b -> replicate rt b
                        CapacityByTerm tbl -> lastN rt $ concat [ replicate i b | (i,b)  <- tbl ]

        cumuDep = sum $ take (ot-rt) (calcAmortAmt fa)
        utilsVec = getValByDates uCurve Inc pdates
        units = [ mulBR c u | (u,c) <- zip utilsVec capacityCaps]
        prices = getValByDates pCurve Inc pdates
        cash = [ mulBR u p | (p,u) <- zip prices units]
        cumuDepreciation = tail $ scanl (+) cumuDep amortizedBals 
        
        txns = zipWith6 CF.FixedFlow pdates scheduleBals amortizedBals cumuDepreciation units cash
        futureTxns = cutBy Inc Future asOfDay txns
        begBal = CF.buildBegBal futureTxns
      in 
        Right $ (CF.CashFlowFrame (begBal,asOfDay,Nothing) $ futureTxns, Map.empty)
  
