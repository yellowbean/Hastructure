{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Types
  (DayCount(..),DateType(..),OverrideType(..)
  ,ActionOnDate(..),DealStatus(..),DatePattern(..)
  ,BondName,BondNames,FeeName,FeeNames,AccName,AccNames,AccountName
  ,Pre(..),Ts(..),TsPoint(..),PoolSource(..)
  ,actionDates,DateDesp(..),Period(..)
  ,WhenTrigger(..),Trigger(..),Threshold(..),TriggerEffect(..)
  ,RangeType(..),CutoffType(..),FormulaType(..),CustomDataType(..)
  ,Balance,DealStats(..)
  ,Date,Dates,TimeSeries(..),IRate,Amount,Rate,StartDate,EndDate
  ,Spread,Floor,Cap,Interest,Principal,Cash,Default,Loss,Rental
  ,ResultComponent(..),SplitType(..)
  ,PrepaymentRate,DefaultRate,RecoveryRate,RemainTerms,Recovery,Prepayment
  ,Table(..),lookupTable,LookupType(..),epocDate,BorrowerNum)
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

import Data.List

type BondName = String
type BondNames = [String]
type FeeName = String
type FeeNames = [String]
type AccName = String
type AccountName = String
type AccNames = [String]
type Balance = Centi

type Date = Time.Day
type Dates = [Time.Day]
type Rate = Rational  -- general Rate like pool factor
type IRate = Micro    -- Interest Rate Type
type Spread = Micro
type Amount = Centi
type Comment = String
type StartDate = Time.Day
type EndDate = Time.Day
type LastIntPayDate = Time.Day
type Floor = Micro
type Principal = Centi
type Interest = Centi
type Default = Centi
type Loss = Centi
type Cash = Centi
type Recovery = Centi
type Prepayment = Centi
type Rental = Centi
type Cap = Micro

type PrepaymentRate = Rate
type DefaultRate = Rate
type RecoveryRate = Rate
type RemainTerms = Int


type BorrowerNum = Int

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

type DateVector = (Date, DatePattern)

data DateDesp = FixInterval (Map.Map DateType Date) Period Period 
                        --  cutoff pool       closing bond payment dates 
              | CustomDates Date [ActionOnDate] Date [ActionOnDate]
              | PatternInterval (Map.Map DateType (Date, DatePattern, Date))
              --cutoff closing mRevolving end-date dp1-pc dp2-bond-pay 
              | PreClosingDates Date Date (Maybe Date) Date DateVector DateVector
              --             cutoff mRevolving closing dp1-pool-pay dp2-bond-pay
              | CurrentDates (Date,Date) (Maybe Date) Date DateVector DateVector
              deriving (Show,Eq)

data ActionOnDate = EarnAccInt Date AccName -- sweep bank account interest
                  | ChangeDealStatusTo Date DealStatus
                  | AccrueFee Date FeeName
                  | ResetLiqProvider Date String
                  | PoolCollection Date String
                  | RunWaterfall Date String
                  | DealClosed Date 
                  deriving (Show,Generic,Read)

epocDate = Time.fromGregorian 1970 1 1


instance TimeSeries ActionOnDate where
    getDate (RunWaterfall d _) = d
    getDate (ResetLiqProvider d _) = d
    getDate (PoolCollection d _) = d
    getDate (EarnAccInt d _) = d
    getDate (AccrueFee d _) = d
    getDate (DealClosed d ) = d
    getDate (ChangeDealStatusTo d _ ) = d
    cmp ad1 ad2 = compare (getDate ad1) (getDate ad2)
    sameDate ad1 ad2 = getDate ad1 == getDate ad2
    getDates ads = map getDate ads

actionDates :: [ActionOnDate] -> Dates 
actionDates = map getDate

instance Ord ActionOnDate where
  compare a1 a2 = compare (getDate a1) (getDate a2)

instance Eq ActionOnDate where
  a1 == a2 = getDate a1 == getDate a2

opts :: JSONKeyOptions
opts = defaultJSONKeyOptions -- { keyModifier = toLower }

instance ToJSONKey DateType where
  toJSONKey = genericToJSONKey opts

instance FromJSONKey DateType where
  fromJSONKey = genericFromJSONKey opts

data OverrideType = CustomActionOnDates [ActionOnDate]
                    deriving (Show)

data DealStatus = DealAccelerated (Maybe Date)
                | DealDefaulted (Maybe Date)
                | Amortizing
                | Revolving
                | Ended
                | PreClosing
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
                 | SemiAnnual (Int, Int) (Int, Int)
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
               | CurrentPoolBorrowerNum
               | BondFactor
               | PoolFactor
               | PoolCollectionInt  -- a redirect map to `CurrentPoolCollectionInt T.Day`
               | UseCustomData String
               | PoolCollectionIncome PoolSource
               | AllAccBalance
               | AccBalance [String]
               | ReserveAccGap [String] 
               | MonthsTillMaturity BondName
               | ReserveAccGapAt Date [String] 
               | FutureCurrentPoolBalance
               | FutureCurrentPoolBegBalance Date
               | FutureCurrentBondBalance Date
               | FutureCurrentBondFactor Date
               | FutureCurrentPoolFactor Date
               | FutureCurrentPoolBorrowerNum Date
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
         | IfGTInt DealStats Int
         | IfGETInt DealStats Int
         | IfLTInt DealStats Int
         | IfLETInt DealStats Int
         | IfEqInt DealStats Int
         | IfEqBal DealStats Balance
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

instance TimeSeries (TsPoint a) where 
    getDate (TsPoint d a) = d
    sameDate (TsPoint d1 a1) (TsPoint d2 a2) = d1 == d2
    cmp t1 t2 = compare (getDate t1) (getDate t2)
    getDates tps = map getDate tps

instance Ord a => Ord (TsPoint a) where
  compare (TsPoint d1 tv1) (TsPoint d2 tv2) = compare d1 d2

data Ts = FloatCurve [TsPoint Rational]
        | BoolCurve [TsPoint Bool]
        | BalanceCurve [TsPoint Balance]
        | LeftBalanceCurve [TsPoint Balance]
        | RatioCurve [TsPoint Rational]
        | ThresholdCurve [TsPoint Rational]
        | IRateCurve [TsPoint IRate]
        | FactorCurveClosed [TsPoint Rational] Date
        | PricingCurve [TsPoint Rational] 
        deriving (Show,Eq,Ord,Read)


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
data CutoffType = Inc | Exc

data ResultComponent = CallAt Date
                  | DealStatusChangeTo Date DealStatus DealStatus
                  | BondOutstanding String Balance Balance -- when deal ends
                  | BondOutstandingInt String Balance Balance -- when deal ends
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

data SplitType = EqToLeft   -- if equal, the element belongs to left
               | EqToRight  -- if equal, the element belongs to right
               | EqToLeftKeepOne
               | EqToLeftKeepOnes
               deriving (Show,Eq)

class TimeSeries ts where 
    cmp :: ts -> ts -> Ordering
    sameDate :: ts -> ts -> Bool
    getDate :: ts -> Date
    getDates :: [ts] -> [Date]

data LookupType = Upward 
                | Downward
                | UpwardInclude
                | DownwardInclude

data Table a b = ThresholdTable [(a,b)]

lookupTable :: Ord a => Table a b -> LookupType -> a -> b -> b
lookupTable (ThresholdTable rows) lkupType lkupVal notFound
  =  case findIndex (lkUpFunc lkupVal) rs of 
       Nothing -> notFound
       Just i -> snd $ rows!!i
     where 
         rs = map fst rows
         lkUpFunc = case lkupType of 
                      Upward  ->  (>)
                      UpwardInclude -> (>=)
                      Downward  -> (<)
                      DownwardInclude -> (<=)


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
