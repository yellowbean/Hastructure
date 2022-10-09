{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Types
  (DayCount(..),DateType(..),OverrideType(..)
  ,ActionOnDate(..),DealStatus(..),DatePattern(..)
  ,BondName,BondNames,FeeName,FeeNames,AccName,AccNames,AccountName
  ,Pre(..),DealStats(..),Ts(..),TsPoint(..)
  ,actionDate,actionDates,DateDesp(..),Period(..)
  ,WhenTrigger(..),Trigger(..),Threshold(..),TriggerEffect(..))
  where

import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Data.Map as Map
import GHC.Generics
import Language.Haskell.TH

import Text.Read (readMaybe)

import Data.Aeson hiding (json)
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Fixed


type BondName = String
type BondNames = [String]
type FeeName = String
type FeeNames = [String]
type AccName = String
type AccountName = String
type AccNames = [String]
type Balance = Centi
type IRate = Micro

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
              | RevolvingDate
              | StatedMaturityDate
              deriving (Show,Ord,Eq,Generic,Read)

data Period = Daily 
              | Weekly 
              | Monthly 
              | Quarterly 
              | SemiAnnually 
              | Annually
              deriving (Show,Eq)

data DateDesp = FixInterval (Map.Map DateType Date) Period Period 
              | CustomDates Date [ActionOnDate] Date [ActionOnDate]
              | PatternInterval (Map.Map DateType (Date, DatePattern, Date))
              deriving (Show,Eq)

data ActionOnDate = RunWaterfall Date String
                   |PoolCollection Date String
                   |EarnAccInt Date AccName -- sweep bank account interest
                   deriving (Show,Generic,Read)

actionDate :: ActionOnDate -> Date
actionDate (RunWaterfall d _) = d
actionDate (PoolCollection d _) = d
actionDate (EarnAccInt d _) = d

actionDates :: [ActionOnDate] -> Dates 
actionDates = map actionDate

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
                | Amortizing
                | Revolving
                | Ended
                deriving (Show,Ord,Eq,Read)

--instance ToJSONKey DealStatus where
--  -- toJSONKey = toJSONKeyText (T.pack . show)
--  toJSONKey = genericToJSONKey opts
--
--instance FromJSONKey DealStatus where
--  fromJSONKey = genericFromJSONKey opts
 
data DatePattern = MonthEnd
                 | QuarterEnd
                 | YearEnd 
                 | MonthFirst
                 | QuarterFirst
                 | YearFirst
                 | MonthDayOfYear Int Int  -- T.MonthOfYear T.DayOfMonth
                 | DayOfMonth Int -- T.DayOfMonth 
                 -- | DayOfWeek Int -- T.DayOfWeek
                 deriving (Show,Eq)


data DealStats =  CurrentBondBalance
              | CurrentPoolBalance
              | CurrentPoolBegBalance
              | CurrentPoolDefaultedBalance
              | OriginalBondBalance
              | OriginalPoolBalance
              | BondFactor
              | PoolFactor
              | PoolCollectionInt  -- a redirect map to `CurrentPoolCollectionInt T.Day`
              | AllAccBalance
              | CumulativeDefaultBalance Date
              | FutureCurrentPoolBalance Date
              | FutureCurrentPoolBegBalance Date
              | FutureCurrentPoolDefaultBalance Date
              | FutureCurrentBondBalance Date
              | FutureCurrentBondFactor Date
              | FutureCurrentPoolFactor Date
              | FutureOriginalPoolBalance
              | CurrentBondBalanceOf [String]
              | Factor DealStats Float
              | BondIntPaidAt Date String
              | BondsIntPaidAt Date [String]
              | FeePaidAt Date String
              | FeesPaidAt Date [String]
              | CurrentDueBondInt [String]
              | CurrentDueFee [String]
              | Max DealStats DealStats
              | Min DealStats DealStats
              | LastBondIntPaid [String]
              | LastFeePaid [String]
              | BondBalanceHistory Date Date
              | PoolCollectionIntHistory Date Date
              | Sum [DealStats]
              deriving (Show,Eq,Ord,Read)

data Pre = And Pre Pre
         | Or Pre Pre
         | IfZero DealStats
         | IfGT DealStats Centi
         | IfGET DealStats Centi
         | IfLT DealStats Centi
         | IfLET DealStats Centi
         | IfDealStatus DealStatus
         deriving (Show)

data TsPoint a = TsPoint Date a
                deriving (Show,Eq,Read)

data Ts = FloatCurve [TsPoint Rational]
         |BoolCurve [TsPoint Bool]
         |BalanceCurve [TsPoint Balance]
         |AmountCurve [TsPoint Balance]
         |IRateCurve [TsPoint IRate]
         |FactorCurveClosed [TsPoint Rational] Date
         deriving (Show,Eq,Ord,Read)

instance Ord a => Ord (TsPoint a) where
  compare (TsPoint d1 tv1) (TsPoint d2 tv2)
    = compare d1 d2

data Curve = DRationalCurve [TsPoint Rational]
           | DBoolCurve [TsPoint Bool]
           | DBalanceCurve [TsPoint Balance]
           | DIRateCurve [TsPoint IRate]


data WhenTrigger = EndCollection
                 | EndCollectionWF
                 | BeginDistributionWF
                 | EndDistributionWF
                 deriving (Show,Eq,Ord,Read,Generic)

data Threshold = Below
               | EqBelow
               | Above
               | EqAbove
               deriving (Show,Eq,Ord,Read,Generic)

instance ToJSONKey Threshold where
  toJSONKey = genericToJSONKey opts
instance FromJSONKey Threshold where
  fromJSONKey = genericFromJSONKey opts

data Trigger = Threshold Threshold DealStats Balance
             | ThresholdCurve Threshold DealStats Ts
             | AfterDate Date
             | AfterOnDate Date
             | OnDates [Dates]
             | AllTrigger [Trigger]
             | AnyTrigger [Trigger]
             deriving (Show,Eq,Ord,Read,Generic)

instance ToJSONKey Trigger where
  -- toJSONKey = genericToJSONKey opts
  -- toJSONKey = genericToJSONKey opts
  toJSONKey = toJSONKeyText (T.pack . show)
instance FromJSONKey Trigger where
  --fromJSONKey = genericFromJSONKey opts
  fromJSONKey = FromJSONKeyTextParser $ \t -> case readMaybe (T.unpack t) of
    Just k -> pure k
    Nothing -> fail ("Invalid key: " ++ show t)


data TriggerEffect = DealStatusTo DealStatus
                   | AddTrigger Trigger 
                   | TriggerEffects [TriggerEffect]
                   deriving (Show,Eq)


$(deriveJSON defaultOptions ''Pre)
$(deriveJSON defaultOptions ''DealStats)
$(deriveJSON defaultOptions ''DealStatus)
$(deriveJSON defaultOptions ''DayCount)
$(deriveJSON defaultOptions ''OverrideType)
$(deriveJSON defaultOptions ''DatePattern)
$(deriveJSON defaultOptions ''DateType)
$(deriveJSON defaultOptions ''ActionOnDate)
$(deriveJSON defaultOptions ''Ts)
$(deriveJSON defaultOptions ''TsPoint)
$(deriveJSON defaultOptions ''Threshold)
$(deriveJSON defaultOptions ''Trigger)
$(deriveJSON defaultOptions ''TriggerEffect)
$(deriveJSON defaultOptions ''WhenTrigger)
$(deriveJSON defaultOptions ''DateDesp)
$(deriveJSON defaultOptions ''Period)