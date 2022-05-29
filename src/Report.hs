{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Report where

import Language.Haskell.TH
import Data.Aeson.TH

import qualified Liability as L
import qualified Expense as F
import qualified Asset as P
import qualified Cashflow as CF
import qualified Deal as D

-- data r =
-- getReports :: TestDeal ->
data ProjectFlow = ProjectFlow {
   projDealName :: String
}


$(deriveJSON defaultOptions ''ProjectFlow)
