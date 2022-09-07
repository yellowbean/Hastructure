--{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE TemplateHaskell #-}

module Asset.Loan(Loan)
  where

import qualified Data.Time as T
import qualified Cashflow as CF -- (Cashflow,Amount,Interests,Principals)
import qualified Assumptions as A
import qualified Asset as Ast
import qualified Lib as L
import qualified Data.Map as Map
import Data.List
import Data.Aeson hiding (json)
import Language.Haskell.TH
import Data.Aeson.TH
import Data.Aeson.Types

import Debug.Trace
debug = flip trace

data LoanType = Bullet
              | IntBy


data Loan = Loan Ast.OriginalInfo LoanType Balance Rate RemainTerms Status

-- instance Asset Loan where
