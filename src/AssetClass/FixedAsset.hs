{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

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
debug = flip trace


instance Ast.Asset FixedAsset where 
  getCurrentBal (FixedAsset _  bal rt ) = bal
  
  getPaymentDates 
    (FixedAsset fo@FixedAssetInfo{startDate=sd ,period=p,originTerm=ot} bal rt)
    extra
    = genDates sd p (ot+extra)

  projCashflow fa@(FixedAsset fai@FixedAssetInfo{originBalance=ob, accRule=ar, originTerm=ot,residualBalance=rb
                                                 ,capacity=cap} bal rt) 
               asOfDay
               (A.FixedAssetAssump uCurve pCurve,_,_)
               _
    = let 
        pdates = Ast.getPaymentDates fa rt
        cfLength = length pdates
        amortizedBals = case ar of 
                          StraightLine -> replicate rt $ divideBI (ob-rb) ot
                          -- DecliningBalance -> 
                          --   let 
                          --     r = 0.2
                          --   in 
                          --     scanl (\x -> x - (mulBR x r)) (ob-rb)
                          _ -> replicate rt 0
        scheduleBals = tail $ scanl (-) ob amortizedBals
        capacityCaps = case cap of
                        FixedCapacity b -> replicate rt b
                        CapacityByTerm tbl -> lastN rt $ concat [ replicate i b | (i,b)  <- tbl ]

        cumuDep = ob - bal
        utilsVec = getValByDates uCurve Inc pdates
        units = [ mulBR c u | (u,c) <- zip utilsVec capacityCaps]
        prices = getValByDates pCurve Inc pdates
        cash = [ mulBR u p | (p,u) <- zip prices units]
        cumuDepreciation = tail $ scanl (+) cumuDep amortizedBals 
        
        txns = zipWith6 CF.FixedFlow pdates scheduleBals amortizedBals cumuDepreciation units cash
      in 
        (CF.CashFlowFrame $ cutBy Inc Future asOfDay txns, Map.empty)

  