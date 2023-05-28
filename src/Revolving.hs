{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Revolving
  (
  --,AssetForSale(..)
  RevolvingPool(..))
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





data RevolvingPool = ConstantAsset [AssetUnion]
                   | StaticAsset [AssetUnion]
                   | AssetCurve [TsPoint AssetUnion]
                   deriving (Show,Generic)

-- data AssetForSale = AssetAvailableInstallment (AssetAvailable Installment)
--                   | AssetAvailableMortgage (AssetAvailable Mortgage)
--                   | AssetAvailableLease (AssetAvailable Lease)
--                   | AssetAvailableLoan (AssetAvailable Loan)
--                   deriving (Show, Generic)

-- $(deriveJSON defaultOptions ''AssetForSale)
$(deriveJSON defaultOptions ''RevolvingPool)
