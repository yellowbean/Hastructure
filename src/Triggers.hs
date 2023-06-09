{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Triggers(
    Trigger(..),TriggerEffect(..)
)
 where

import qualified Data.Text as T
import Text.Read (readMaybe)
import Lib
import Types
import Accounts (ReserveAmount)
import Data.Aeson hiding (json)
import Language.Haskell.TH
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Fixed
import Data.Maybe
import GHC.Generics

data TriggerEffect = DealStatusTo DealStatus
                   | DoAccrueFee FeeNames
                   | AddTrigger Trigger 
                   | ChangeReserveBalance String ReserveAmount
                   | TriggerEffects [TriggerEffect]
                   deriving (Show, Eq, Generic)
 
data Trigger = Trigger {
            trgCondition :: Pre
            ,trgEffects :: TriggerEffect
            ,trgStatus :: Bool
            ,trgCurable :: Bool
            } deriving (Show, Eq, Generic)

$(deriveJSON defaultOptions ''Trigger)
$(deriveJSON defaultOptions ''TriggerEffect)
