{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Triggers(
    Trigger(..),TriggerEffect(..),TriggerName,setTriggered
)
 where

import qualified Data.Text as T
import Text.Read (readMaybe)
import Lib ( Pre, DealStatus )
import Types
import Accounts (ReserveAmount)
import Data.Aeson ( defaultOptions )
import Language.Haskell.TH
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Fixed
import Data.Maybe
import Data.Map
import GHC.Generics
import qualified Liability as L

type TriggerName = String

setTriggered :: Trigger -> Trigger
setTriggered trg@Trigger{ trgStatus = False } = trg { trgStatus = True }
setTriggered trg@Trigger{ trgStatus = True } = error ("The trigger is already triggered"++ show trg)


data TriggerEffect = DealStatusTo DealStatus                    -- ^ change deal status
                   | DoAccrueFee FeeNames                       -- ^ accure fee
                   | AddTrigger Trigger                         -- ^ add a new trigger
                   | ChangeReserveBalance String ReserveAmount  -- ^ update reserve target balance  
                   | IssueBonds [L.Bond] AccountName            -- ^ issue new bonds and deposit proceeds to account
                   | BuyAsset AccountName PricingMethod         -- ^ buy asset from the assumption using funds from account
                   | TriggerEffects [TriggerEffect]
                   deriving (Show, Eq, Generic)
 
data Trigger = Trigger {
            trgCondition :: Pre                       -- ^ condition to trigger 
            ,trgEffects :: TriggerEffect              -- ^ what happen if it was triggered
            ,trgStatus :: Bool                        -- ^ if it is triggered or not 
            ,trgCurable :: Bool                       -- ^ if it is curable trigger
            } deriving (Show, Eq, Generic)

$(deriveJSON defaultOptions ''Trigger)
$(deriveJSON defaultOptions ''TriggerEffect)
