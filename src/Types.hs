{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Types
  (DayCount(..),DateType(..),OverrideType(..)
  ,ActionOnDate(..),DealStatus(..),DatePattern(..)
  ,BondName,BondNames,FeeName,FeeNames,AccName,AccNames)
  where

import qualified Data.Text as T
import qualified Data.Time as Time
import GHC.Generics
import Language.Haskell.TH

import Text.Read (readMaybe)

import Data.Aeson hiding (json)
import Data.Aeson.TH
import Data.Aeson.Types


type BondName = String
type BondNames = [String]
type FeeName = String
type FeeNames = [String]
type AccName = String
type AccNames = [String]

type Date = Time.Day
type Dates = [Time.Day]


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
              | RevolvingEndDate
              | StatedMaturityDate
              deriving (Show,Ord,Eq,Generic,Read)
$(deriveJSON defaultOptions ''DateType)

data ActionOnDate = RunWaterfall Date String
                   |PoolCollection Date String
                   |EarnAccInt Date String
                   deriving (Show,Generic,Read)
$(deriveJSON defaultOptions ''ActionOnDate)

actionDate :: ActionOnDate -> Date
actionDate (RunWaterfall d _) = d
actionDate (PoolCollection d _) = d
actionDate (EarnAccInt d _) = d

instance Ord ActionOnDate where
  compare a1 a2 = compare (actionDate a1) (actionDate a2)
  --compare (PoolCollection d1 _) (PoolCollection d2 _) = compare d1 d2
  --compare (RunWaterfall d1 _) (RunWaterfall d2 _) = compare d1 d2
  --compare (PoolCollection d1 _) (RunWaterfall d2 _) = compare d1 d2
  --compare (RunWaterfall d1 _) (PoolCollection d2 _) = compare d1 d2

instance Eq ActionOnDate where
  a1 == a2 = (actionDate a1) == (actionDate a2)
  --(PoolCollection d1 _) == (PoolCollection d2 _) = d1 == d2
  --(RunWaterfall d1 _) == (RunWaterfall d2 _) = d1 == d2
  --(PoolCollection d1 _) == (RunWaterfall d2 _) = d1 == d2
  --(RunWaterfall d1 _) == (PoolCollection d2 _) = d1 == d2

opts :: JSONKeyOptions
opts = defaultJSONKeyOptions -- { keyModifier = toLower }

instance ToJSONKey DateType where
  -- toJSONKey = toJSONKeyText (T.pack . show)
  toJSONKey = genericToJSONKey opts

instance FromJSONKey DateType where
  fromJSONKey = genericFromJSONKey opts
  --fromJSONKey = FromJSONKeyTextParser $ \t -> case readMaybe (T.unpack t) of
  --  Just k -> pure k
  --  Nothing -> fail ("Invalid key: " ++ show t)

data OverrideType = CustomActionOnDates [ActionOnDate]
                    deriving (Show)

data DealStatus = EventOfAccelerate (Maybe Date)
                | EventOfDefault (Maybe Date)
                | Current
                | Revolving
                | Ended
                deriving (Show)

data DatePattern = MonthEnd
                 | QuarterEnd
                 | YearEnd 
                 | MonthFirst
                 | QuarterFirst
                 | YearFirst
                 | MonthDayOfYear Int Int  -- T.MonthOfYear T.DayOfMonth
                 | DayOfMonth Int -- T.DayOfMonth 
                 | DayOfWeek Int -- T.DayOfWeek
                 deriving (Show)


$(deriveJSON defaultOptions ''DealStatus)
$(deriveJSON defaultOptions ''DayCount)
$(deriveJSON defaultOptions ''OverrideType)
$(deriveJSON defaultOptions ''DatePattern)
