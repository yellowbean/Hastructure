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

data CallOption = PoolBalance Balance    -- ^ triggered when pool perform balance below threshold
                | BondBalance Balance    -- ^ triggered when bond current balance below threshold
                | PoolFactor Rate        -- ^ triggered when pool factor (pool perform balance/origin balance)
                | BondFactor Rate        -- ^ triggered when bond factor (total bonds current balance / origin balance)
                | OnDate Date            -- ^ triggered at date
                | AfterDate Date         -- ^ triggered when after date
                | And [CallOption]       -- ^  triggered when all options were satisfied
                | Or [CallOption]        -- ^ triggered when any option is satisfied
                | PoolPv Balance         -- ^ Call when PV of pool fall below
                | Pre Pre                -- ^ triggered when pool perform balance below threshold
                deriving (Show,Generic)

$(deriveJSON defaultOptions ''CallOption)
