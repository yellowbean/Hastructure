{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Call(CallOption(..),AssetLiquidationMethod(..))
 where

import qualified Data.Time as T

import Data.Aeson hiding (json)
import Language.Haskell.TH
import Data.Aeson.TH
import Data.Aeson.Types

data AssetLiquidationMethod = BalanceFactor Float Float -- by performing & default
                            | BalanceFactor2 Float Float Float -- by performing/delinq/default factor
                            | PV Float Float -- discount factor, recovery on default
                            | Custom Float -- custom amount
                            deriving (Show)

data CallOption = PoolBalance Float
                | BondBalance Float
                | PoolFactor Float
                | BondFactor Float
                | OnDate T.Day
                | AfterDate T.Day
                | And [CallOption] 
                | Or [CallOption] 
                | PoolPv Float  -- Call when PV of pool fall below
                deriving (Show)




$(deriveJSON defaultOptions ''CallOption)
$(deriveJSON defaultOptions ''AssetLiquidationMethod)
