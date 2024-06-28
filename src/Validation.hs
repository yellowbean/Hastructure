{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Validation ()
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



-- valAssetRunReq :: (IR.UseRate a,P.Asset a) => TestDeal a -> AP.NonPerfAssumption -> (Bool,[ResultComponent])
-- valAssetRunReq t@TestDeal{accounts = accMap} assump@A.NonPerfAssumption{A.interest = intM, A.issueBondSchedule = mIssuePlan} 
--   = let 