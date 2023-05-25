{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Revolving
  (LiquidationMethod(..),AssetForSale(..),AssetForSale(..)
  ,AssetAvailable(..))
  where

import GHC.Generics
import Language.Haskell.TH
import Data.Aeson hiding (json)
import qualified Data.Text as T
import qualified Cashflow as CF
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Hashable
import Data.Fixed
import Types

import AssetClass.AssetBase


data LiquidationMethod = BalanceFactor Rate Rate -- by performing & default balace
                       | BalanceFactor2 Rate Rate Rate -- by performing/delinq/default factor
                       | DefaultedBalance Rate  -- only liquidate defaulted balance
                       | PV IRate IRate -- discount factor, recovery on default
                       | Custom Rate -- custom amount
                       deriving (Show,Generic)


data AssetAvailable a = ConstantAsset [a]
                    | RevolvingAsset (TsPoint [a])
                    deriving (Show,Generic)

data AssetForSale = AFS (AssetAvailable Installment)
                  | StaticAssetFlow CF.CashFlowFrame
                  deriving (Show, Generic)

$(deriveJSON defaultOptions ''LiquidationMethod)
$(deriveJSON defaultOptions ''AssetForSale)
$(deriveJSON defaultOptions ''AssetAvailable)
