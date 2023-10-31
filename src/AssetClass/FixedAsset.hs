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
import Types 
import Lib
import Util
import DateUtil
import qualified Cashflow as CF

import AssetClass.AssetBase


import Debug.Trace
import AssetClass.AssetCashflow
import qualified Asset as Ast
debug = flip trace


instance Asset FixedAsset where 
  getCurrentBal (FixedAsset _ _ _ _ bal rt ) = bal
  
  getPaymentDates 
    (FixedAsset fo@FixedAsestInfo{originBalance=ob,startDate=sd,period=p,originTerm=ot} cap mE mI bal rt)
    extra
    = genDates sd p (ot+extra)

  projCashflow fa@(FixedAsset fo cap mE mI bal rt) 
               asOfDay
               (FixedAssetAssump uCurve pCurve)
               Nothing
  = let 
      pdates = getPaymentDates fa rt
      amortizedBals = case (accRule fo) of 
                        Straight -> replicate rt $ divideBI ob ot
                        _ -> replicate rt 0
      scheduleBals = scanl ob (-) amortizedBals
      capacityCaps = case cap of
                       FixedCapcity b -> replicate rt b
                       CapcityByTerm -> []
      cumuDep = ob - bal
      utilsVec = getValByDates uCurve pdates
      units = [ mulBR c u | (u,c) <- zip utilsVec capacityCaps]
      prices = getValByDates pCurve pdates
      cash = [ p * u | (p,u) <- zip prices units]
      cumuDepreciation = scanl (+) cumuDep amortizedBals 
      txns = zipWith8 CF.FixedFlow pdates scheduleBals amortizedBals cumuDepreciation units cash Nothing Nothing
    in 
      (CF.CashFlowFrame $ cutBy asOfDay Inc Future txns,Map.Empty)