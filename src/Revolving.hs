{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Revolving
  (AssetPricingMethod(..))
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

data AssetPricingMethod = BalanceFactor Rate
                        | PVFactor Rate
                        deriving(Show)


$(deriveJSON defaultOptions ''AssetPricingMethod)
