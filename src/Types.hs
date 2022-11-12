{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Types
  (DayCount(..),DateType(..),OverrideType(..)
  ,ActionOnDate(..),DealStatus(..),DatePattern(..)
  ,BondName,BondNames,FeeName,FeeNames,AccName,AccNames,AccountName
  ,Pre(..),Ts(..),TsPoint(..),PoolSource(..)
  ,actionDate,actionDates,DateDesp(..),Period(..)
  ,WhenTrigger(..),Trigger(..),Threshold(..),TriggerEffect(..)
  ,RangeType(..),FormulaType(..),CustomDataType(..)
  ,Balance,DealStats(..)
  ,Date,Dates
  ,EndType,ResultComponent(..))
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
import Data.Ix


type BondName = String
type BondNames = [String]
type FeeName = String
type FeeNames = [String]
type AccName = String
type AccountName = String
type AccNames = [String]
type Balance = Centi
type IRate = Micro
type Rate = Rational 

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

data ActionOnDate = EarnAccInt Date AccName -- sweep bank account interest
                  | AccrueFee Date FeeName
                  | ResetLiqProvider Date String
                  | PoolCollection Date String
                  | RunWaterfall Date String
                  deriving (Show,Generic,Read)

actionDate :: ActionOnDate -> Date
actionDate (RunWaterfall d _) = d
actionDate (ResetLiqProvider d _) = d
actionDate (PoolCollection d _) = d
actionDate (EarnAccInt d _) = d
actionDate (AccrueFee d _) = d

actionDates :: [ActionOnDate] -> Dates 
actionDates = map actionDate

instance Ord ActionOnDate where
  compare a1 a2 = compare (actionDate a1) (actionDate a2)

instance Eq ActionOnDate where
  a1 == a2 = (actionDate a1) == (actionDate a2)

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

data DealStatus = DealAccelerated (Maybe Date)
                | DealDefaulted (Maybe Date)
                | Amortizing
                | Revolving
                | Ended
                deriving (Show,Ord,Eq,Read)

data CustomDataType = CustomConstant Rational 
                    | CustomCurve    Ts 
                    | CustomDS       DealStats
                    deriving (Show,Ord,Eq,Read)

data DatePattern = MonthEnd
                 | QuarterEnd
                 | YearEnd 
                 | MonthFirst
                 | QuarterFirst
                 | MidYear
                 | YearFirst
                 | MonthDayOfYear Int Int  -- T.MonthOfYear T.DayOfMonth
                 | DayOfMonth Int -- T.DayOfMonth 
                 | CustomDate [Dates]
                 -- | DayOfWeek Int -- T.DayOfWeek
                 deriving (Show,Eq)

data PoolSource = CollectedInterest
                | CollectedPrincipal
                | CollectedRecoveries
                | CollectedPrepayment
                | CollectedRental
                | CollectedFee
                deriving (Show,Ord,Read,Eq)


data DealStats =  CurrentBondBalance
               | CurrentPoolBalance
               | CurrentPoolBegBalance
               | CurrentPoolDefaultedBalance
               | CumulativePoolDefaultedBalance
               | CumulativePoolDefaultedRate
               | OriginalBondBalance
               | OriginalPoolBalance
               | BondFactor
               | PoolFactor
               | PoolCollectionInt  -- a redirect map to `CurrentPoolCollectionInt T.Day`
               | UseCustomData String
               | PoolCollectionIncome PoolSource
               | AllAccBalance
               | AccBalance [String]
               | ReserveAccGap [String] 
               | ReserveAccGapAt Date [String] 
               | FutureCurrentPoolBalance
               | FutureCurrentPoolBegBalance Date
               | FutureCurrentBondBalance Date
               | FutureCurrentBondFactor Date
               | FutureCurrentPoolFactor Date
               | FutureOriginalPoolBalance
               | CurrentBondBalanceOf [String]
               | BondIntPaidAt Date String
               | BondsIntPaidAt Date [String]
               | BondPrinPaidAt Date String
               | BondsPrinPaidAt Date [String]
               | BondBalanceGap String
               | BondBalanceGapAt Date String
               | FeePaidAt Date String
               | FeesPaidAt Date [String]
               | CurrentDueBondInt [String]
               | CurrentDueFee [String]
               | LastBondIntPaid [String]
               | LastBondPrinPaid [String]
               | LastFeePaid [String]
               | BondBalanceHistory Date Date
               | PoolCollectionHistory PoolSource Date Date
               | Factor DealStats Rational
               | Max DealStats DealStats
               | Min DealStats DealStats
               | Sum [DealStats]
               | Substract [DealStats]
               | Divide DealStats DealStats
               | Constant Rational
               | CustomData String Date
               deriving (Show,Eq,Ord,Read)

data Pre = And Pre Pre
         | Or Pre Pre
         | IfZero DealStats
         | IfGT DealStats Centi
         | IfGET DealStats Centi
         | IfLT DealStats Centi
         | IfLET DealStats Centi
         | IfDealStatus DealStatus
         | IfAfterDate Date
         | IfBeforeDate Date
         | IfAfterOnDate Date
         | IfBeforeOnDate Date
         deriving (Show)

data FormulaType = ABCD
                 | Other
                 deriving (Show,Eq)

data TsPoint a = TsPoint Date a
                deriving (Show,Eq,Read)

data Ts = FloatCurve [TsPoint Rational]
        | BoolCurve [TsPoint Bool]
        | BalanceCurve [TsPoint Balance]
        | RatioCurve [TsPoint Rational]
        | ThresholdCurve [TsPoint Rational]
        | IRateCurve [TsPoint IRate]
        | FactorCurveClosed [TsPoint Rational] Date
        | PricingCurve [TsPoint Rational] 
        deriving (Show,Eq,Ord,Read)

instance Ord a => Ord (TsPoint a) where
  compare (TsPoint d1 tv1) (TsPoint d2 tv2)
    = compare d1 d2

data WhenTrigger = EndCollection
                 | EndCollectionWF
                 | BeginDistributionWF
                 | EndDistributionWF
                 deriving (Show,Eq,Ord,Read,Generic)

instance ToJSONKey WhenTrigger where
  toJSONKey = toJSONKeyText (T.pack . show)

instance FromJSONKey WhenTrigger where
  fromJSONKey = FromJSONKeyTextParser $ \t -> case readMaybe (T.unpack t) of
    Just k -> pure k
    Nothing -> fail ("Invalid key: " ++ show t)


data RangeType = II | IE | EI | EE


data ResultComponent = CallAt Date
                  | DealStatusChangeTo Date DealStatus
                  -- | BondIntShortfall Date String Balance  -- when deal ends
                  | BondOutstanding String Balance Balance -- when deal ends
                  | BondOutstandingInt String Balance Balance -- when deal ends
                  deriving (Show)

--instance Eq TxnComponent where 
--    (Account s1) == (Account s2) = s1 ==  s2
--    (Bond s1) == (Bond s2) = s1 ==  s2
--    (Expense s1) == (Expense s2) = s1 == s2
--
--
--instance Ord TxnComponent where 
--    compare (Account s1) (Account s2) = compare s1 s2
--    compare (Bond s1) (Bond s2) = compare s1 s2
--    compare (Expense s1) (Expense s2) = compare s1 s2

data EndType = IN | EX
              deriving (Show)

data Threshold = Below
               | EqBelow
               | Above
               | EqAbove
               deriving (Show,Eq,Ord,Read,Generic)
      
instance ToJSONKey Threshold where
  toJSONKey = genericToJSONKey opts
instance FromJSONKey Threshold where
  fromJSONKey = genericFromJSONKey opts

data Trigger = ThresholdBal Threshold DealStats Balance
             | ThresholdBalCurve Threshold DealStats Ts
             | ThresholdRate Threshold DealStats Micro
             | ThresholdRateCurve Threshold DealStats Ts
             | AfterDate Date
             | AfterOnDate Date
             | OnDates [Dates]
             | PassMaturityDate BondName  -- a bond remain oustanding after mature date
             | AllTrigger [Trigger]
             | AnyTrigger [Trigger]
             | Always  Bool
             deriving (Show,Eq,Ord,Read,Generic)

instance ToJSONKey Trigger where
  toJSONKey = toJSONKeyText (T.pack . show)
instance FromJSONKey Trigger where
  fromJSONKey = FromJSONKeyTextParser $ \t -> case readMaybe (T.unpack t) of
    Just k -> pure k
    Nothing -> fail ("Invalid key: " ++ show t)


data TriggerEffect = DealStatusTo DealStatus
                   | DoAccrueFee FeeNames
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
$(deriveJSON defaultOptions ''PoolSource)
$(deriveJSON defaultOptions ''FormulaType)
$(deriveJSON defaultOptions ''CustomDataType)
$(deriveJSON defaultOptions ''ResultComponent)
