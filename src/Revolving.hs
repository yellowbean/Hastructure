{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Revolving
  (LiquidationMethod(..))
  where

import GHC.Generics
import Language.Haskell.TH
import Data.Aeson hiding (json)
import qualified Data.Text as T
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Hashable
import Data.Fixed
import Types
import Asset

data LiquidationMethod = BalanceFactor Rate Rate -- by performing & default balace
                       | BalanceFactor2 Rate Rate Rate -- by performing/delinq/default factor
                       | DefaultedBalance Rate  -- only liquidate defaulted balance
                       | PV IRate IRate -- discount factor, recovery on default
                       | Custom Rate -- custom amount
                       deriving (Show)



$(deriveJSON defaultOptions ''LiquidationMethod)
