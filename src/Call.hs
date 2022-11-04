{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Call(CallOption(..),LiquidationMethod(..))
 where

import qualified Data.Time as T
import Lib(Balance,Rate,Date,IRate)
import Data.Aeson hiding (json)
import Language.Haskell.TH
import Data.Aeson.TH
import Data.Aeson.Types

data LiquidationMethod = BalanceFactor Rate Rate -- by performing & default
                       | BalanceFactor2 Rate Rate Rate -- by performing/delinq/default factor
                       | PV IRate IRate -- discount factor, recovery on default
                       | Custom Rate -- custom amount
                       deriving (Show)

data CallOption = PoolBalance Balance
                | BondBalance Balance
                | PoolFactor Rate
                | BondFactor Rate
                | OnDate Date
                | AfterDate Date
                | And [CallOption] 
                | Or [CallOption] 
                | PoolPv Balance  -- Call when PV of pool fall below
                deriving (Show)


$(deriveJSON defaultOptions ''CallOption)
$(deriveJSON defaultOptions ''LiquidationMethod)
