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
  getCurrentBal (FixedAsset _ _ _ _ bal ) = bal
  
  projCashflow (FixedAsset fo _ _ _ _ ) 
               asOfDay
               (FixedAssetAssump uCurve pCurve)
               Nothing
  = CF.CashFlowFrame []