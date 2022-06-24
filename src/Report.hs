{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Report() where

import Language.Haskell.TH
import Data.Aeson.TH

import Lib 
import Data.Maybe
import Data.Time as T
import qualified Data.Map as M
import qualified Liability as L
import qualified Expense as F
import qualified Asset as P
import qualified Accounts as A
import qualified Cashflow as CF
import qualified Deal as D



--buildSummary :: D.TestDeal -> CF.CashFlowFrame
--buildSummary t = 
--
--    where
--        ads = []
--        bndFlow = []
--        accFlow = []
--        feeFlow = []
--        poolFlow = []


-- $(deriveJSON defaultOptions ''Pr)
