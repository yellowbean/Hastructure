{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module AssetClass.AssetCashflow
  ()
  where

import qualified Data.Time as T
import qualified Cashflow as CF 
import qualified Assumptions as A
import Asset as Ast
import Types
import Lib
import Util
import DateUtil
import InterestRate as IR

import qualified Data.Map as Map
import Data.List
import Data.Ratio
import Data.Maybe
import GHC.Generics
import Data.Aeson hiding (json)
import Language.Haskell.TH
import Data.Aeson.TH
import Data.Aeson.Types

import AssetClass.AssetBase

import Debug.Trace
import Assumptions (AssetPerfAssumption(MortgageAssump))
import GHC.Float.RealFracMethods (truncateFloatInteger)
debug = flip trace


-- patchRecovery :: A.RecoveryAssumption -> [CF.TsRow] -> [CF.TsRow]
-- patchRecovery (A.Reocvery (rr,lag)) trs
--   = let 
--       recoveries = [ mulBR (CF.mflowDefault x) rr | x <- trs ]
--       recoveriesLagged = (replicate lag 0) ++ recoveries
--     in 
--       