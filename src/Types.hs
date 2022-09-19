{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Types
  where

import Language.Haskell.TH
import Data.Aeson.TH

--30/360 - calculates the daily interest using a 360-day year and then multiplies that by 30 (standardized month).
--30/365 - calculates the daily interest using a 365-day year and then multiplies that by 30 (standardized month).
--actual/360 - calculates the daily interest using a 360-day year and then multiplies that by the actual number of days in each time period.
--actual/365 - calculates the daily interest using a 365-day year and then multiplies that by the actual number of days in each time period.
--actual/actual - calculates the daily interest using the actual number of days in the year and then multiplies that by the actual number of days in each time period.


data DayCount = DC_30_360
              | DC_30_365
              | DC_ACT_360
              | DC_ACT_365
              | DC_ACT_ACT


$(deriveJSON defaultOptions ''DayCount)
