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
import qualified Liability as L


data TriggerEffect = DealStatusTo DealStatus                    -- ^ change deal status
                   | DoAccrueFee FeeNames                       -- ^ accure fee
                   | AddTrigger Trigger                         -- ^ add a new trigger
                   | ChangeReserveBalance String ReserveAmount  -- ^ update reserve target balance  
                   | IssueBonds [L.Bond] AccountName            -- ^ issue new bonds and deposit proceeds to account
                   | BuyAsset AccountName PricingMethod         -- ^ buy asset from the assumption using funds from account
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
