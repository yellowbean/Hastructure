{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Triggers(
    Trigger(..),TriggerEffect(..),TriggerName,trgStatusLens
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
import Control.Lens
import qualified Liability as L

type TriggerName = String


data TriggerEffect = DealStatusTo DealStatus                    -- ^ change deal status
                   | DoAccrueFee FeeNames                       -- ^ accure fee
                   | AddTrigger Trigger                         -- ^ add a new trigger
                   | ChangeReserveBalance String ReserveAmount  -- ^ update reserve target balance  
                   | IssueBonds [L.Bond] AccountName            -- ^ issue new bonds and deposit proceeds to account
                   | BuyAsset AccountName PricingMethod         -- ^ buy asset from the assumption using funds from account
                   | TriggerEffects [TriggerEffect]             -- ^ a combination of effects above
                   | DoNothing                                  -- ^ do nothing
                   deriving (Show, Eq, Generic,Ord)
 
data Trigger = Trigger {
            trgCondition :: Pre                       -- ^ condition to trigger 
            ,trgEffects :: TriggerEffect              -- ^ what happen if it was triggered
            ,trgStatus :: Bool                        -- ^ if it is triggered or not 
            ,trgCurable :: Bool                       -- ^ if it is curable trigger
            } deriving (Show, Eq, Generic,Ord)

makeLensesFor [("trgStatus","trgStatusLens") 
                ,("trgEffects","trgEffectsLens") 
                ,("trgCondition","trgConditionLens") 
                ,("trgCurable","trgCurableLens")] ''Trigger

$(concat <$> traverse (deriveJSON defaultOptions) [''TriggerEffect, ''Trigger])