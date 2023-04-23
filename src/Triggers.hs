{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Triggers(
    Trigger(..),TriggerEffect(..)
)
 where

-- import qualified Data.Time as T
import qualified Data.Text as T
import Text.Read (readMaybe)
import Lib
import Types
import Data.Aeson hiding (json)
import Language.Haskell.TH
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Fixed
import Data.Maybe
import GHC.Generics

-- data Trigger = ThresholdBal Threshold DealStats Balance
--              | ThresholdBalCurve Threshold DealStats Ts
--              | ThresholdRate Threshold DealStats Micro
--              | ThresholdRateCurve Threshold DealStats Ts
--              | AfterDate Date
--              | AfterOnDate Date
--              | OnDates [Dates]
--              | PassMaturityDate BondName  -- a bond remain oustanding after mature date
--              | AllTrigger [Trigger]
--              | AnyTrigger [Trigger]
--              | Always  Bool
--              deriving (Show,Eq,Ord,Read,Generic)

data TriggerEffect = DealStatusTo DealStatus
                   | DoAccrueFee FeeNames
                   | AddTrigger Trigger 
                   | TriggerEffects [TriggerEffect]
                   deriving (Show, Eq, Generic)
 
data Trigger = Trigger {
            trgCondition :: Pre
            ,trgEffects :: TriggerEffect
            ,trgStatus :: Bool
            ,trgCurable :: Bool
            } deriving (Show, Eq, Generic)

-- instance ToJSONKey Trigger where
--   toJSONKey = toJSONKeyText (T.pack . show)
-- instance FromJSONKey Trigger where
--   fromJSONKey = FromJSONKeyTextParser $ \t -> case readMaybe (T.unpack t) of
--     Just k -> pure k
--     Nothing -> fail ("Invalid key: " ++ show t)

  

$(deriveJSON defaultOptions ''Trigger)
$(deriveJSON defaultOptions ''TriggerEffect)
