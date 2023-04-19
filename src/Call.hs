{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Call(CallOption(..))
 where

import qualified Data.Time as T
import Lib
import Types
import Data.Aeson hiding (json)
import Language.Haskell.TH
import Data.Aeson.TH
import Data.Aeson.Types
import GHC.Generics

data CallOption = PoolBalance Balance
                | BondBalance Balance
                | PoolFactor Rate
                | BondFactor Rate
                | OnDate Date
                | AfterDate Date
                | And [CallOption] 
                | Or [CallOption] 
                | PoolPv Balance  -- Call when PV of pool fall below
                deriving (Show,Generic)

$(deriveJSON defaultOptions ''CallOption)
