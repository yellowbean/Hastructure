{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Types
  (DayCount(..),DateType(..))
  where

import qualified Data.Text as T
import GHC.Generics
import Language.Haskell.TH

import Text.Read (readMaybe)

import Data.Aeson hiding (json)
import Data.Aeson.TH
import Data.Aeson.Types

--30/360 - calculates the daily interest using a 360-day year and then multiplies that by 30 (standardized month).
--30/365 - calculates the daily interest using a 365-day year and then multiplies that by 30 (standardized month).
--actual/360 - calculates the daily interest using a 360-day year and then multiplies that by the actual number of days in each time period.
--actual/365 - calculates the daily interest using a 365-day year and then multiplies that by the actual number of days in each time period.
--actual/actual - calculates the daily interest using the actual number of days in the year and then multiplies that by the actual number of days in each time period.

-- http://www.deltaquants.com/day-count-conventions

data DayCount = DC_30E_360  -- ISMA European 30S/360 Special German Eurobond Basis
              | DC_30Ep_360 -- 30E+/360
              | DC_ACT_360 -- Actual/360 , French
              | DC_ACT_365
              | DC_ACT_365A -- Actual/365 Actual 
              | DC_ACT_365L -- Actual/365 Leap Year
              | DC_NL_365 -- Actual/365 No leap year
              | DC_ACT_365F -- Actual /365 Fixed, English
              | DC_ACT_ACT -- Actual/Actual ISDA 
              | DC_30_360_ISDA --  IDSA
              | DC_30_360_German --  Gernman
              | DC_30_360_US --  30/360 US Municipal , Bond basis
              deriving (Show)

data DateType = ClosingDate
              | CutoffDate
              | FirstPayDate
              deriving (Show,Ord,Eq,Generic,Read)

instance ToJSONKey DateType where
  toJSONKey = toJSONKeyText (T.pack . show)

instance FromJSONKey DateType where
  fromJSONKey = FromJSONKeyTextParser $ \t -> case readMaybe (T.unpack t) of
    Just k -> pure k
    Nothing -> fail ("Invalid key: " ++ show t)


--data DealComponent = CompLiability 
--                   | CompPool 
--                   | CompAsset 
--                   | CompCall 


$(deriveJSON defaultOptions ''DayCount)
$(deriveJSON defaultOptions ''DateType)
