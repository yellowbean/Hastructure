{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}


module Types
  (DayCount(..),DateType(..)
  ,DatePattern(..)
  ,BondName,BondNames,FeeName,FeeNames,AccName,AccNames,AccountName
  ,Ts(..),TsPoint(..),PoolSource(..)
  ,Period(..), Threshold(..)
  ,RangeType(..),CutoffType(..),DealStatus(..)
  ,Balance,Index(..)
  ,Cmp(..),TimeHorizion(..)
  ,Date,Dates,TimeSeries(..),IRate,Amount,Rate,StartDate,EndDate,Lag
  ,Spread,Floor,Cap,Interest,Principal,Cash,Default,Loss,Rental,PrepaymentPenalty
  ,SplitType(..),BookItem(..),BookItems,BalanceSheetReport(..),CashflowReport(..)
  ,Floater,CeName,RateAssumption(..)
  ,PrepaymentRate,DefaultRate,RecoveryRate,RemainTerms,Recovery,Prepayment
  ,Table(..),lookupTable,Direction(..),epocDate,BorrowerNum
  ,Txn(..),TxnComment(..)
  ,RoundingBy(..),DateDirection(..)
  ,BookDirection(..),IRR(..),DealCycle(..),Limit(..),Pre(..)
  ,Liable(..),CumPrepay,CumDefault,CumDelinq,CumPrincipal,CumLoss,CumRecovery,PoolId(..)
  ,DealName,lookupIntervalTable,CutoffFields(..),PriceResult(..)
  ,DueInt,DuePremium, DueIoI,DateVector,DealStats(..)
  ,PricingMethod(..),CustomDataType(..),ResultComponent(..),DealStatType(..)
  ,ActionWhen(..)
  ,getDealStatType,getPriceValue,preHasTrigger
  ,AccountName
  )
  
  where

import qualified Data.Text as Text
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Time as Time
import qualified Data.Time.Format as TF
import qualified Data.Map as Map
import qualified Data.List.Split
import Text.Regex.Base
import Text.Regex.PCRE
import GHC.Generics
import Language.Haskell.TH

import Text.Read (readMaybe)


import Data.Aeson hiding (json)
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Fixed
import Data.Ix


import Data.List (intercalate, findIndex)
-- import Cashflow (CashFlowFrame)

import Debug.Trace
-- import qualified Cashflow as CF
debug = flip trace



type BondName = String
type BondNames = [String]
type FeeName = String
type FeeNames = [String]
type AccName = String
type AccountName = String
type AccNames = [String]
type CeName = String
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
type PrepaymentPenalty = Centi

type CumPrepay = Centi
type CumPrincipal = Centi
type CumDefault = Centi
type CumDelinq = Centi
type CumLoss = Centi
type CumRecovery = Centi

type PrepaymentRate = Rate
type DefaultRate = Rate
type RecoveryRate = Rate
type RemainTerms = Int
type BorrowerNum = Int
type Lag = Int


type Valuation = Centi
type PerFace = Micro
type WAL = Centi
type Duration = Balance
type Convexity = Micro
type Yield = Micro
type AccruedInterest = Centi
type IRR = Rational
data YieldResult = Yiel

data Index = LPR5Y
            | LPR1Y
            | LIBOR1M
            | LIBOR3M
            | LIBOR6M
            | LIBOR1Y
            | USTSY1Y
            | USTSY2Y
            | USTSY3Y
            | USTSY5Y
            | USTSY7Y
            | USTSY10Y
            | USTSY20Y
            | USTSY30Y
            | USCMT1Y
            | PRIME
            | COFI
            | SOFR1M
            | SOFR3M
            | SOFR6M
            | SOFR1Y
            | EURIBOR1M
            | EURIBOR3M
            | EURIBOR6M
            | EURIBOR12M
            | BBSW
            | IRPH --  The IRPH (Índice de Referencia de Préstamos Hipotecarios) is a reference index used in Spain to fix the interest rate of mortgage loans
            | SONIA 
            deriving (Show,Eq,Generic,Ord,Read)

type Floater = (Index,Spread)

epocDate = Time.fromGregorian 1970 1 1
-- http://www.deltaquants.com/day-count-conventions
data DayCount = DC_30E_360       -- ^ ISMA European 30S/360 Special German Eurobond Basis
              | DC_30Ep_360      -- ^ 30E+/360
              | DC_ACT_360       -- ^ Actual/360 , French
              | DC_ACT_365
              | DC_ACT_365A      -- ^ Actual/365 Actual 
              | DC_ACT_365L      -- ^ Actual/365 Leap Year
              | DC_NL_365        -- ^ Actual/365 No leap year
              | DC_ACT_365F      -- ^ Actual /365 Fixed, English
              | DC_ACT_ACT       -- ^ Actual/Actual ISDA 
              | DC_30_360_ISDA   -- ^ IDSA
              | DC_30_360_German -- ^ Gernman
              | DC_30_360_US     -- ^ 30/360 US Municipal , Bond basis
              deriving (Show,Eq,Generic,Ord,Read)


data DateType = ClosingDate        -- ^ deal closing day
              | CutoffDate         -- ^ after which, the pool cashflow was aggregated to SPV
              | FirstPayDate       -- ^ first payment day for bond/waterfall to run with
              | StatedMaturityDate -- ^ sated maturity date, all cashflow projection/deal action stops by
              deriving (Show,Ord,Eq,Generic,Read)


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
                 | CustomDate [Date]
                 | DaysInYear [(Int, Int)]
                 | EveryNMonth Date Int
                 | Weekday Int 
                 | AllDatePattern [DatePattern]
                 | StartsExclusive Date DatePattern
                 | Exclude DatePattern [DatePattern]
                 | OffsetBy DatePattern Int
                 -- | DayOfWeek Int -- T.DayOfWeek
                 deriving (Show, Eq, Generic, Ord, Read)


data Period = Daily 
            | Weekly 
            | BiWeekly
            | Monthly 
            | Quarterly 
            | SemiAnnually 
            | Annually
            deriving (Show,Eq,Generic,Ord)

type DateVector = (Date, DatePattern)



data RoundingBy a = RoundCeil a 
                  | RoundFloor a
                  deriving (Show, Generic, Eq, Ord, Read)

type DealName = String

data PoolId = PoolName String                         -- ^ pool name
            | PoolConsol                              -- ^ consolidate pool ( the only pool )
            | DealBondFlow DealName String Date Rate  -- ^ bond flow from deal
            deriving (Eq,Ord,Generic)


data Cmp = G      -- ^ Greater than 
         | GE     -- ^ Greater Equal than
         | L      -- ^ Less than
         | LE     -- ^ Less Equal than
         | E      -- ^ Equals to
         deriving (Show,Generic,Eq,Ord,Read)


data PoolSource = CollectedInterest               -- ^ interest
                | CollectedPrincipal              -- ^ schdule principal
                | CollectedRecoveries             -- ^ recoveries 
                | CollectedPrepayment             -- ^ prepayment
                | CollectedPrepaymentPenalty      -- ^ prepayment pentalty
                | CollectedRental                 -- ^ rental from pool
                | CollectedFeePaid                -- ^ fee from pool
                | CollectedCash                   -- ^ cash from pool
                | NewDefaults                     -- ^ new defaults in balance
                | NewLosses                       -- ^ new losses in balance
                | NewDelinquencies                -- ^ new delinquencies in balance
                deriving (Show,Ord,Read,Eq, Generic)


data TsPoint a = TsPoint Date a
                deriving (Show,Eq,Read,Generic)

data RangeType = II     -- ^ include both start and end date
               | IE     -- ^ include start date ,but not end date
               | EI     -- ^ exclude start date but include end date
               | EE     -- ^ exclude either start date and end date 
               | NO_IE  -- ^ no handling on start date and end date

data CutoffType = Inc 
                | Exc
                deriving (Show,Read,Generic,Eq)

data DateDirection = Future 
                   | Past
                   deriving (Show,Read,Generic)

class TimeSeries ts where 
    cmp :: ts -> ts -> Ordering
    cmp t1 t2 = compare (getDate t1) (getDate t2)
    sameDate :: ts -> ts -> Bool
    sameDate t1 t2 =  getDate t1 == getDate t2
    getDate :: ts -> Date
    getDates :: [ts] -> [Date]
    getDates ts = [ getDate t | t <- ts ]
    filterByDate :: [ts] -> Date -> [ts]
    filterByDate ts d = filter (\x -> getDate x == d ) ts
    sliceBy :: RangeType -> StartDate -> EndDate -> [ts] -> [ts]
    sliceBy rt sd ed ts
      = case rt of 
          II -> filter (\x -> getDate x >= sd && getDate x <= ed ) ts 
          IE -> filter (\x -> getDate x >= sd && getDate x < ed ) ts 
          EI -> filter (\x -> getDate x > sd && getDate x <= ed) ts 
          EE -> filter (\x -> getDate x > sd && getDate x < ed ) ts 
          _  -> error "Not support NO_IE for sliceBy in TimeSeries"
    cutBy :: CutoffType -> DateDirection -> Date -> [ts] -> [ts]
    cutBy ct dd d ts 
      = case (ct,dd) of
          (Inc, Future) ->  filter (\x -> getDate x >= d) ts
          (Inc, Past) ->  filter (\x -> getDate x <= d) ts
          (Exc, Future) ->  filter (\x -> getDate x > d) ts
          (Exc, Past) ->  filter (\x -> getDate x < d) ts

    cmpWith :: ts -> Date -> Ordering
    cmpWith t d = compare (getDate t) d

    isAfter :: ts -> Date -> Bool 
    isAfter t d = getDate t > d
    isOnAfter :: ts -> Date -> Bool 
    isOnAfter t d = getDate t >= d
    isBefore :: ts -> Date -> Bool 
    isBefore t d = getDate t < d
    isOnBefore :: ts -> Date -> Bool 
    isOnBefore t d = getDate t <= d

    splitBy :: Date -> CutoffType -> [ts] -> ([ts],[ts])
    splitBy d ct tss = 
      let 
        ffunR x = case ct of
                    Inc -> getDate x > d -- include ts in the Left
                    Exc -> getDate x >= d  -- 
        ffunL x = case ct of
                    Inc -> getDate x <= d -- include ts in the Left
                    Exc-> getDate x < d  -- 
      in 
        (filter ffunL tss, filter ffunR tss)

    getByDate :: Date -> [ts] -> Maybe ts
    getByDate d ts = case filterByDate ts d of 
                      [] -> Nothing
                      (x:_) -> Just x
 
-- ^ different types of curves, which determine how to interpolate between two points
data Ts = FloatCurve [TsPoint Rational]
        | BoolCurve [TsPoint Bool]
        | BalanceCurve [TsPoint Balance]
        | LeftBalanceCurve [TsPoint Balance]
        | RatioCurve [TsPoint Rational]
        | ThresholdCurve [TsPoint Rational]
        | IRateCurve [TsPoint IRate]
        | FactorCurveClosed [TsPoint Rational] Date
        | PricingCurve [TsPoint Rational] 
        deriving (Show,Eq,Ord,Read,Generic)


data Direction = Up 
               | Down
               deriving (Show,Read,Generic,Eq,Ord)

-- ^ direction of the transaction, in terms of the book keeping
data BookDirection = Credit
                   | Debit
                   deriving (Show,Ord, Eq,Read, Generic)


type DueInt = Balance
type DuePremium = Balance
type DueIoI = Balance

data DealCycle = EndCollection         -- ^ | collection period <HERE> collection action , waterfall action
               | EndCollectionWF       -- ^ | collection period  collection action <HERE>, waterfall action
               | BeginDistributionWF   -- ^ | collection period  collection action , <HERE>waterfall action
               | EndDistributionWF     -- ^ | collection period  collection action , waterfall action<HERE>
               | InWF                  -- ^ | collection period  collection action , waterfall <HERE> action
               deriving (Show, Ord, Eq, Read, Generic)

-- ^ different status of the deal
data DealStatus = DealAccelerated (Maybe Date)      -- ^ Deal is accelerated status with optinal accerlerated date
                | DealDefaulted (Maybe Date)        -- ^ Deal is defaulted status with optinal default date
                | Amortizing                        -- ^ Deal is amortizing 
                | Revolving                         -- ^ Deal is revolving
                | PreClosing DealStatus             -- ^ Deal is not closed, but has a closing date
                | Warehousing DealStatus              -- ^ Deal is not closed, but closing date is not determined yet
                | Called                            -- ^ Deal is called
                | Ended                             -- ^ Deal is marked as closed
                deriving (Show,Ord,Eq,Read, Generic)

-- ^ different pricing methods
data PricingMethod = BalanceFactor Rate Rate          -- ^ [balance] to be multiply with rate1 and rate2 if status of asset is "performing" or "defaulted"
                   | BalanceFactor2 Rate Rate Rate    -- ^ [balance] by performing/delinq/default factor
                   | DefaultedBalance Rate            -- ^ [balance] only liquidate defaulted balance
                   | PV IRate IRate                   -- ^ discount factor, recovery pct on default
                   | PVCurve Ts                       -- ^ [CF] Pricing cashflow with a Curve
                   | PvRate Rate                      -- ^ [CF] Pricing cashflow with a constant rate
                   | PvByRef DealStats                -- ^ [CF] Pricing cashflow with a ref rate
                   | Custom Rate                      -- ^ custom amount
                   deriving (Show, Eq ,Generic, Read,Ord)

-- ^ condition which can be evaluated to a boolean value
data Pre = IfZero DealStats
         | If Cmp DealStats Balance
         | IfRate Cmp DealStats Micro
         | IfInt Cmp DealStats Int
         | IfCurve Cmp DealStats Ts
         | IfRateCurve Cmp DealStats Ts
         | IfIntCurve Cmp DealStats Ts
         | IfDate Cmp Date
         | IfBool DealStats Bool
         -- compare deal 
         | If2 Cmp DealStats DealStats
         | IfRate2 Cmp DealStats DealStats
         | IfInt2 Cmp DealStats DealStats
         -- | IfRateCurve DealStats Cmp Ts
         | IfDealStatus DealStatus
         | Always Bool
         | IfNot Pre
         | Any [Pre]
         | All [Pre]                            -- ^ 
         deriving (Show,Generic,Eq,Ord,Read)



data Table a b = ThresholdTable [(a,b)]
                 deriving (Show,Eq,Ord,Read,Generic)


data ActionType = ActionResetRate  -- ^ reset interest rate from curve
                | ActionAccrue     -- ^ accrue liablity
                 deriving (Show,Eq,Ord,Read,Generic)

-- ^ comment of the transaction in the accounts
data TxnComment = PayInt [BondName]
                | PayYield BondName 
                | PayPrin [BondName] 
                | WriteOff BondName Balance
                | FundWith BondName Balance
                | PayPrinResidual [BondName] 
                | PayFee FeeName
                | SeqPayFee [FeeName] 
                | PayFeeYield FeeName
                | Transfer AccName AccName 
                | TransferBy AccName AccName Limit
                | PoolInflow (Maybe [PoolId]) PoolSource
                | LiquidationProceeds
                | LiquidationSupport String
                | LiquidationDraw
                | LiquidationRepay
                | LiquidationSupportInt Balance Balance
                | BankInt
                | SupportDraw
                | Empty 
                | Tag String
                | UsingDS DealStats
                | SwapAccrue
                | SwapInSettle
                | SwapOutSettle
                | PurchaseAsset String Balance
                | IssuanceProceeds String
                | TxnDirection BookDirection
                | TxnComments [TxnComment]
                deriving (Eq, Show, Ord ,Read, Generic)

-- ^ transaction record in each entity
data Txn = BondTxn Date Balance Interest Principal IRate Cash DueInt DueIoI (Maybe Float) TxnComment     -- ^ bond transaction record for interest and principal 
         | AccTxn Date Balance Amount TxnComment                                                         -- ^ account transaction record 
         | ExpTxn Date Balance Amount Balance TxnComment                                                 -- ^ expense transaction record
         | SupportTxn Date (Maybe Balance) Amount Balance DueInt DuePremium TxnComment                   -- ^ liquidity provider transaction record
         | IrsTxn Date Balance Amount IRate IRate Balance TxnComment                                     -- ^ interest swap transaction record
         | EntryTxn Date Balance Amount TxnComment                                                       -- ^ ledger book entry
         | TrgTxn Date Bool TxnComment
         deriving (Show, Generic, Eq, Read)

-- ^ different types of deal stats
data DealStats = CurrentBondBalance
               | CurrentPoolBalance (Maybe [PoolId])
               | CurrentPoolBegBalance (Maybe [PoolId])
               | CurrentPoolDefaultedBalance
               | CumulativePoolDefaultedBalance (Maybe [PoolId])  -- ^ Depreciated, use PoolCumCollection
               | CumulativePoolRecoveriesBalance (Maybe [PoolId]) -- ^ Depreciated, use PoolCumCollection
               | CumulativeNetLoss (Maybe [PoolId])
               | CumulativePoolDefaultedRate (Maybe [PoolId])
               | CumulativePoolDefaultedRateTill Int (Maybe [PoolId])
               | CumulativeNetLossRatio (Maybe [PoolId])
               | OriginalBondBalance
               | OriginalBondBalanceOf [BondName]
               | OriginalPoolBalance (Maybe [PoolId])
               | DealIssuanceBalance (Maybe [PoolId])
               | CurrentPoolBorrowerNum (Maybe [PoolId])
               | ProjCollectPeriodNum
               | BondFactor
               | PoolFactor (Maybe [PoolId])
               | BondWaRate [BondName]
               | UseCustomData String
               | PoolCumCollection [PoolSource] (Maybe [PoolId])
               | PoolCumCollectionTill Int [PoolSource] (Maybe [PoolId])
               | PoolCurCollection [PoolSource] (Maybe [PoolId])
               | PoolCollectionStats Int [PoolSource] (Maybe [PoolId])
               | AllAccBalance
               | AccBalance [AccName]
               | LedgerBalance [String]
               | LedgerTxnAmt [String] (Maybe TxnComment)
               | ReserveAccGap [AccName]
               | ReserveExcess [AccName] 
               | MonthsTillMaturity BondName
               | HasPassedMaturity [BondName]
               | ReserveAccGapAt Date [AccName] 
               | ReserveExcessAt Date [AccName] 
               | FutureCurrentPoolBalance (Maybe [PoolId])
               | FutureCurrentSchedulePoolBalance (Maybe [PoolId])
               | FutureCurrentSchedulePoolBegBalance (Maybe [PoolId])
               | PoolScheduleCfPv PricingMethod (Maybe [PoolId])
               | FuturePoolScheduleCfPv Date PricingMethod (Maybe [PoolId])
               | FutureWaCurrentPoolBalance Date Date (Maybe [PoolId])
               -- | FutureCurrentPoolBegBalance Date
               | FutureCurrentPoolBegBalance (Maybe [PoolId])
               | FutureCurrentBondBalance Date
               | FutureCurrentBondFactor Date
               | FutureCurrentPoolFactor Date (Maybe [PoolId])
               | FutureCurrentPoolBorrowerNum Date (Maybe [PoolId])
               | CurrentBondBalanceOf [BondName]
               | IsMostSenior BondName [BondName]
               | IsPaidOff [BondName]
               | IsOutstanding [BondName]
               | BondIntPaidAt Date BondName
               | BondsIntPaidAt Date [BondName]
               | BondPrinPaidAt Date BondName
               | BondsPrinPaidAt Date [BondName]
               | BondBalanceGap BondName
               | BondBalanceGapAt Date BondName
               | BondDuePrin [BondName]
               | FeePaidAt Date FeeName
               | FeeTxnAmt [FeeName] (Maybe TxnComment)
               | BondTxnAmt [BondName] (Maybe TxnComment)
               | AccTxnAmt  [AccName] (Maybe TxnComment)
               | FeeTxnAmtBy Date [FeeName] (Maybe TxnComment)
               | BondTxnAmtBy Date [BondName] (Maybe TxnComment)
               | AccTxnAmtBy Date [AccName] (Maybe TxnComment)
               | FeesPaidAt Date [FeeName] 
               | CurrentDueBondInt [BondName]
               | CurrentDueBondIntOverInt [BondName]
               | CurrentDueBondIntTotal [BondName]
               | CurrentDueFee [FeeName]
               | LastBondIntPaid [BondName]
               | LastBondPrinPaid [BondName]
               | LastFeePaid [FeeName]
               | LiqCredit [String]
               | LiqBalance [String]
               | RateCapNet String
               | RateSwapNet String
               | BondBalanceHistory Date Date
               | PoolCollectionHistory PoolSource Date Date (Maybe [PoolId])
               | UnderlyingBondBalance (Maybe [BondName])
               | TriggersStatus DealCycle String
               | IsDealStatus DealStatus
               | TestRate DealStats Cmp Micro
               | TestAny Bool [DealStats]
               | TestAll Bool [DealStats]
               | TestNot DealStats
               | PoolWaRate (Maybe [PoolId])
               | BondRate BondName
               -- weighted average balancer over period
               | WeightedAvgCurrentPoolBalance Date Date (Maybe [PoolId])
               | WeightedAvgCurrentBondBalance Date Date [BondName]
               | WeightedAvgOriginalPoolBalance Date Date (Maybe [PoolId])
               | WeightedAvgOriginalBondBalance Date Date [BondName]

               -- 
               | Factor DealStats Rational
               | Multiply [DealStats]
               | Max [DealStats]
               | Min [DealStats]
               | Sum [DealStats]
               | Substract [DealStats]
               | Subtract [DealStats]
               | Excess [DealStats]
               | Avg [DealStats]
               | Divide DealStats DealStats
               | DivideRatio DealStats DealStats
               | Constant Rational
               | FloorAndCap DealStats DealStats DealStats
               | CustomData String Date
               | FloorWith DealStats DealStats
               | FloorWithZero DealStats
               | CapWith DealStats DealStats
               | Abs DealStats
               | Round DealStats (RoundingBy Balance)
               deriving (Show,Eq,Ord,Read,Generic)

preHasTrigger :: Pre -> [(DealCycle,String)]
preHasTrigger (IfBool (TriggersStatus dc tName) _) = [(dc,tName)]
preHasTrigger (Any ps) = concat $ preHasTrigger <$> ps
preHasTrigger (All ps) = concat $ preHasTrigger <$> ps
preHasTrigger _ = []


data Limit = DuePct Rate            -- ^ up to % of total amount due
           | DueCapAmt Balance      -- ^ up to $ amount 
           | KeepBalAmt DealStats   -- ^ pay till a certain amount remains in an account
           | DS DealStats           -- ^ transfer with limit described by a `DealStats`
           | ClearLedger String     -- ^ when transfer, clear the ledger by transfer amount
           | BookLedger String      -- ^ when transfer, book the ledger by the transfer amount
           | RemainBalPct Rate      -- ^ pay till remain balance equals to a percentage of `stats`
           | TillTarget             -- ^ transfer amount which make target account up reach reserve balanace
           | TillSource             -- ^ transfer amount out till source account down back to reserve balance
           | Multiple Limit Float   -- ^ factor of a limit
           deriving (Show,Ord,Eq,Read, Generic)

type BookItems = [BookItem]

data BookItem = Item String Balance 
              | ParentItem String BookItems
              deriving (Show,Read,Generic)


data BalanceSheetReport = BalanceSheetReport {
                            asset :: BookItems
                            ,liability :: BookItems
                            ,equity :: BookItems
                            ,reportDate :: Date}         -- ^ snapshot date of the balance sheet
                            deriving (Show,Read,Generic)
 
data CashflowReport = CashflowReport {
                        inflow :: BookItems
                        ,outflow :: BookItems
                        ,net :: Balance
                        ,startDate :: Date 
                        ,endDate :: Date }
                        deriving (Show,Read,Generic)

data Threshold = Below
               | EqBelow
               | Above
               | EqAbove
               deriving (Show,Eq,Ord,Read,Generic)


data SplitType = EqToLeft   -- if equal, the element belongs to left
               | EqToRight  -- if equal, the element belongs to right
               | EqToLeftKeepOne
               | EqToLeftKeepOnes
               deriving (Show, Eq, Generic)


data CutoffFields = IssuanceBalance      -- ^ pool issuance balance
                  | HistoryRecoveries    -- ^ cumulative recoveries
                  | HistoryInterest      -- ^ cumulative interest collected
                  | HistoryPrepayment    -- ^ cumulative prepayment collected
                  | HistoryPrincipal     -- ^ cumulative principal collected
                  | HistoryRental        -- ^ cumulative rental collected
                  | HistoryDefaults      -- ^ cumulative default balance
                  | HistoryDelinquency   -- ^ cumulative delinquency balance
                  | HistoryLoss          -- ^ cumulative loss/write-off balance
                  | HistoryCash          -- ^ cumulative cash
                  | AccruedInterest      -- ^ accrued interest at closing
                  deriving (Show,Ord,Eq,Read,Generic)


data PriceResult = PriceResult Valuation PerFace WAL Duration Convexity AccruedInterest [Txn]
                 | AssetPrice Valuation WAL Duration Convexity AccruedInterest
                 | OASResult PriceResult [Valuation] Spread  
                 | ZSpread Spread 
                 deriving (Show, Eq, Generic)



getPriceValue :: PriceResult -> Balance
getPriceValue (AssetPrice v _ _ _ _ ) = v
getPriceValue (PriceResult v _ _ _ _ _ _) = v
getPriceValue x = error  $ "failed to match with type when geting price value" ++ show x


getValuation :: PriceResult -> PerFace
getValuation (PriceResult _ val _ _ _ _ _) = val
getValuation (OASResult pr _ _) = getValuation pr
getValuation pr =  error $ "not support for pricing result"++ show pr


class Liable lb where 

  -- must implement
  isPaidOff :: lb -> Bool
  getCurBalance :: lb -> Balance
  getOriginBalance :: lb -> Balance
  getDueInt :: lb -> Balance

  -- optional implement
  -- getTotalDue :: [lb] -> Balance
  -- getTotalDue lbs =  sum $ getDue <$> lbs

lookupTable :: Ord a => Table a b -> Direction -> (a -> Bool) -> Maybe b
lookupTable (ThresholdTable rows) direction lkUpFunc
  = case findIndex lkUpFunc rs of 
      Nothing -> Nothing
      Just i -> Just $ vs!!i  
    where 
        rs = case direction of 
                Up -> reverse $ map fst rows
                Down -> map fst rows
        vs = case direction of 
                Up -> reverse $ map snd rows
                Down -> map snd rows

lookupIntervalTable :: Ord a => Table a b -> Direction -> (a -> Bool) -> Maybe ((a,b),(a,b))
lookupIntervalTable (ThresholdTable rows) direction lkUpFunc
  = case findIndex lkUpFunc rs of 
      Nothing -> Nothing
      Just i -> if succ i == length rows then 
                  Nothing
                else
                  Just $ (rows!!i, rows!!(i+1)) -- `debug` ("Find index"++ show i)
    where 
        rs = case direction of 
                Up -> reverse $ map fst rows
                Down -> map fst rows


data RateAssumption = RateCurve Index Ts     -- ^ a rate curve ,which value of rates depends on time
                    | RateFlat Index IRate   -- ^ a rate constant
                    deriving (Show, Generic)

data TimeHorizion = ByMonth
                  | ByYear
                  | ByQuarter

instance TimeSeries (TsPoint a) where 
    getDate (TsPoint d a) = d

instance Ord a => Ord (TsPoint a) where
  compare (TsPoint d1 tv1) (TsPoint d2 tv2) = compare d1 d2



instance Show PoolId where
  show (PoolName n)  = n
  show PoolConsol = "PoolConsol"
  show (DealBondFlow dn bn sd r) = "BondFlow:"++dn++":"++bn++":"++show sd++":"++show r

instance (Read PoolId) where
  readsPrec d "PoolConsol" = [(PoolConsol,"")]
  readsPrec d rStr = 
    let 
      pn = Data.List.Split.splitOn ":" rStr
    in
      case pn of
        [dn,bn,sd,r] -> 
          let 
            sd' = TF.parseTimeOrError True TF.defaultTimeLocale "%Y-%m-%d" sd
            r' = read r::Rate
          in 
            [(DealBondFlow dn bn sd' r',"")]
        ["PoolName",pn] -> [(PoolName pn,"")]
        _ -> error $ "Invalid PoolId: "++ show pn


$(deriveJSON defaultOptions ''TsPoint)
$(deriveJSON defaultOptions ''Ts)
$(deriveJSON defaultOptions ''Cmp)
$(deriveJSON defaultOptions ''PoolSource)
$(deriveJSON defaultOptions ''RoundingBy)
$(deriveJSON defaultOptions ''PoolId)

instance ToJSONKey PoolId where
  toJSONKey :: ToJSONKeyFunction PoolId
  toJSONKey = toJSONKeyText (T.pack . show)

instance FromJSONKey PoolId where
  fromJSONKey = FromJSONKeyTextParser $ \t -> case readMaybe (T.unpack t) of
    Just k -> pure k
    Nothing -> fail ("Invalid key: " ++ show t++">>"++ show (T.unpack t))

-- ^ different types of waterfall execution
data ActionWhen = EndOfPoolCollection             -- ^ waterfall executed at the end of pool collection
                | DistributionDay DealStatus      -- ^ waterfall executed depends on deal status
                | CleanUp                         -- ^ waterfall exectued upon a clean up call
                | OnClosingDay                    -- ^ waterfall executed on deal closing day
                | DefaultDistribution             -- ^ default waterfall executed
                | RampUp                          -- ^ ramp up
                | WithinTrigger String            -- ^ waterfall executed within a trigger  
                deriving (Show,Ord,Eq,Generic,Read)


data ResultComponent = CallAt Date                                    -- ^ the date when deal called
                     | DealStatusChangeTo Date DealStatus DealStatus  -- ^ record when status changed
                     | BondOutstanding String Balance Balance         -- ^ when deal ends,calculate oustanding principal balance 
                     | BondOutstandingInt String Balance Balance      -- ^ when deal ends,calculate oustanding interest due 
                     | InspectBal Date DealStats Balance              -- ^ A bal value from inspection
                     | InspectInt Date DealStats Int                  -- ^ A int value from inspection
                     | InspectRate Date DealStats Micro               -- ^ A rate value from inspection
                     | InspectBool Date DealStats Bool                -- ^ A bool value from inspection
                     | RunningWaterfall Date ActionWhen               -- ^ running waterfall at a date 
                     | FinancialReport StartDate EndDate BalanceSheetReport CashflowReport
                     | InspectWaterfall Date (Maybe String) [DealStats] [String]
                     | ErrorMsg String
                     | WarningMsg String
                     | EndRun (Maybe Date) String                             -- ^ end of run with a message
                     -- | SnapshotCashflow Date String CashFlowFrame
                     deriving (Show, Generic)


listToStrWithComma :: [String] -> String
listToStrWithComma = intercalate ","

instance ToJSON TxnComment where 
  toJSON (PayInt bns ) = String $ T.pack $ "<PayInt:"++ listToStrWithComma bns ++ ">"
  toJSON (PayYield bn ) = String $ T.pack $ "<PayYield:"++ bn ++">"
  toJSON (PayPrin bns ) =  String $ T.pack $ "<PayPrin:"++ listToStrWithComma bns ++ ">"
  toJSON (WriteOff bn amt ) =  String $ T.pack $ "<WriteOff:"++ bn ++","++ show amt ++ ">"
  toJSON (FundWith b bal) = String $ T.pack $ "<FundWith:"++b++","++show bal++">"
  toJSON (PayPrinResidual bns ) =  String $ T.pack $ "<PayPrinResidual:"++ listToStrWithComma bns ++ ">"
  toJSON (PayFee fn ) =  String $ T.pack $ "<PayFee:" ++ fn ++ ">"
  toJSON (SeqPayFee fns) =  String $ T.pack $ "<SeqPayFee:"++ listToStrWithComma fns++">"
  toJSON (PayFeeYield fn) =  String $ T.pack $ "<PayFeeYield:"++ fn++">"
  toJSON (Transfer an1 an2) =  String $ T.pack $ "<Transfer:"++ an1 ++","++ an2++">"
  toJSON (TransferBy an1 an2 limit) =  String $ T.pack $ "<TransferBy:"++ an1 ++","++ an2++","++show limit++">"
  toJSON (PoolInflow mPids ps) =  String $ T.pack $ "<Pool"++ maybe "" (intercalate "|" . (show <$>)) mPids ++":"++ show ps++">"
  toJSON LiquidationProceeds =  String $ T.pack $ "<Liquidation>"
  toJSON (UsingDS ds) =  String $ T.pack $ "<DS:"++ show ds++">"
  toJSON BankInt =  String $ T.pack $ "<BankInterest:>"
  toJSON Empty =  String $ T.pack $ "" 
  toJSON (LiquidationSupport source) = String $ T.pack $ "<Support:"++source++">"
  toJSON (LiquidationSupportInt b1 b2) =  String $ T.pack $ "<SupportExp:(Int:"++ show b1 ++ ",Fee:" ++ show b2 ++")>"
  toJSON LiquidationDraw = String $ T.pack $ "<Draw:>"
  toJSON LiquidationRepay = String $ T.pack $ "<Repay:>"
  toJSON SwapAccrue = String $ T.pack $ "<Accure:>"
  toJSON SwapInSettle = String $ T.pack $ "<SettleIn:>"
  toJSON SwapOutSettle = String $ T.pack $ "<SettleOut:>"
  toJSON (PurchaseAsset rPoolName bal) = String $ T.pack $ "<PurchaseAsset:"<> rPoolName <>","++show bal++">"
  toJSON (TxnDirection dr) = String $ T.pack $ "<TxnDirection:"++show dr++">"
  toJSON SupportDraw = String $ T.pack $ "<SupportDraw:>"
  toJSON (IssuanceProceeds nb) = String $ T.pack $ "<IssuanceProceeds:"++nb++">"
  toJSON (Tag cmt) = String $ T.pack $ "<Tag:"++cmt++">"
  toJSON (TxnComments tcms) = Array $ V.fromList $ map toJSON tcms

instance FromJSON TxnComment where
    parseJSON = withText "Empty" parseTxn

parseTxn :: T.Text -> Parser TxnComment 
parseTxn "" = return Empty 
parseTxn "<BankInt>" = return BankInt
parseTxn t = case tagName of 
  "Transfer" -> let 
                  sv = T.splitOn (T.pack ",") $ T.pack contents
                in 
                  return $ Transfer (T.unpack (head sv)) (T.unpack (sv!!1))
  "Support" -> return $ LiquidationSupport contents
  "PayInt" -> return $ PayInt [contents]
  "PayYield" -> return $ PayYield contents
  "PayPrin" -> return $ PayPrin [contents]
  "WriteOff" -> let 
                  sv = T.splitOn (T.pack ",") $ T.pack contents
                in 
                  return $ WriteOff (T.unpack (head sv)) (read (T.unpack (sv!!1))::Balance)
  "PayPrinResidual" -> return $ PayPrinResidual [contents]
  "PayFee" -> return $ PayFee contents
  "SeqPayFee" -> return $ SeqPayFee [contents]
  "PayFeeYield" -> return $ PayFeeYield contents
  "TransferBy" -> let 
                  sv = T.splitOn (T.pack ",") $ T.pack contents
                in 
                  return $ TransferBy (T.unpack (head sv)) (T.unpack (sv!!1)) (read (T.unpack (sv!!2))::Limit)
  "Pool" -> let 
              sr = T.splitOn (T.pack ":") $ T.pack contents
              mPids = if head sr == "Nothing" then 
                        Nothing 
                      else 
                        Just (read <$> T.unpack <$> sr)::(Maybe [PoolId])
            in 
              return $ PoolInflow mPids (read (T.unpack (sr!!1))::PoolSource)
  "Liquidation" -> return LiquidationProceeds
  "DS" -> return $ UsingDS (read (contents)::DealStats)
  "LiquidationSupportExp" -> let 
                              sv = T.splitOn (T.pack ",") $ T.pack contents
                            in 
                              return $ LiquidationSupportInt (read (T.unpack (head sv))::Balance) (read (T.unpack (sv!!1))::Balance)
  "SupportDraw" -> return SupportDraw
  "Draw" -> return LiquidationDraw
  "Repay" -> return LiquidationRepay
  "Accure" -> return SwapAccrue
  "SettleIn" -> return SwapInSettle
  "SettleOut" -> return SwapOutSettle
  "PurchaseAsset" -> let 
                      sv = T.splitOn (T.pack ",") $ T.pack contents
                     in 
                      return $ PurchaseAsset (read (T.unpack (sv!!0))::String)  (read (T.unpack (sv!!1))::Balance)

  "TxnDirection" -> return $ TxnDirection (read contents::BookDirection)
  "FundWith" -> let 
                  sv = T.splitOn (T.pack ",") $ T.pack contents
                in 
                  return $ FundWith (T.unpack (head sv)) (read (T.unpack (sv!!1))::Balance)
--   toJSON (IssuanceProceeds nb) = String $ T.pack $ "<IssuanceProceeds:"++nb++">"
  "IssuanceProceeds" -> return $ IssuanceProceeds contents                  
  "Tag" -> return $ Tag contents                  
  where 
      pat = "<(\\S+):(\\S+)>"::String
      sr = (T.unpack t =~ pat)::[[String]]
      tagName =  head sr!!1::String
      contents = head sr!!2::String


data DealStatType = RtnBalance 
                  | RtnRate 
                  | RtnBool 
                  | RtnInt
                  deriving (Show,Eq,Ord,Read,Generic)

getDealStatType :: DealStats -> DealStatType
getDealStatType (CumulativePoolDefaultedRateTill _ _) = RtnRate
getDealStatType (CumulativePoolDefaultedRate _) = RtnRate
getDealStatType (CumulativeNetLossRatio _) = RtnRate
getDealStatType BondFactor = RtnRate
getDealStatType (PoolFactor _) = RtnRate
getDealStatType (FutureCurrentBondFactor _) = RtnRate
getDealStatType (FutureCurrentPoolFactor _ _) = RtnRate
getDealStatType (BondWaRate _) = RtnRate
getDealStatType (PoolWaRate _) = RtnRate
getDealStatType (BondRate _) = RtnRate

getDealStatType (CurrentPoolBorrowerNum _) = RtnInt
getDealStatType (MonthsTillMaturity _) = RtnInt
getDealStatType ProjCollectPeriodNum = RtnInt

getDealStatType (IsMostSenior _ _) = RtnBool
getDealStatType (TriggersStatus _ _)= RtnBool
getDealStatType (IsDealStatus _)= RtnBool
getDealStatType TestRate {} = RtnBool
getDealStatType (TestAny _ _) = RtnBool
getDealStatType (TestAll _ _) = RtnBool

getDealStatType (Avg dss) = getDealStatType (head dss)
getDealStatType (Max dss) = getDealStatType (head dss)
getDealStatType (Min dss) = getDealStatType (head dss)
getDealStatType (Divide ds1 ds2) = getDealStatType ds1
getDealStatType _ = RtnBalance

dealStatType _ = RtnBalance

data CustomDataType = CustomConstant Rational 
                    | CustomCurve    Ts 
                    | CustomDS       DealStats
                    deriving (Show,Ord,Eq,Read,Generic)




$(deriveJSON defaultOptions ''DealStatus)

$(concat <$> traverse (deriveJSON defaultOptions) [''DealStats, ''PricingMethod, ''DealCycle, ''DateType, ''Period, 
  ''DatePattern, ''Table, ''BalanceSheetReport, ''BookItem, ''CashflowReport, ''Txn] )


instance ToJSONKey DateType where
  toJSONKey = genericToJSONKey defaultJSONKeyOptions

instance FromJSONKey DateType where
  fromJSONKey = genericFromJSONKey defaultJSONKeyOptions

$(deriveJSON defaultOptions ''Pre)


$(deriveJSON defaultOptions ''CustomDataType)
$(deriveJSON defaultOptions ''ActionWhen)

instance ToJSONKey ActionWhen where
  toJSONKey = toJSONKeyText (T.pack . show)

instance FromJSONKey ActionWhen where
  fromJSONKey = FromJSONKeyTextParser $ \t -> case readMaybe (T.unpack t) of
    Just k -> pure k
    Nothing -> fail ("Invalid key: " ++ show t++">>"++ show (T.unpack t))

$(deriveJSON defaultOptions ''ResultComponent)

$(deriveJSON defaultOptions ''PriceResult)
$(deriveJSON defaultOptions ''CutoffFields)




instance ToJSONKey DealCycle where
  toJSONKey = toJSONKeyText (T.pack . show)

instance FromJSONKey DealCycle where
  fromJSONKey = FromJSONKeyTextParser $ \t -> case readMaybe (T.unpack t) of
    Just k -> pure k
    Nothing -> fail ("Invalid key: " ++ show t)


instance ToJSONKey CutoffFields where
  toJSONKey = toJSONKeyText (Text.pack . show)

instance FromJSONKey CutoffFields where
  fromJSONKey = FromJSONKeyTextParser $ \t -> case readMaybe (Text.unpack t) of
    Just k -> pure k
    Nothing -> fail ("Invalid key: " ++ show t)


opts :: JSONKeyOptions
opts = defaultJSONKeyOptions -- { keyModifier = toLower }


$(deriveJSON defaultOptions ''Index)
$(deriveJSON defaultOptions ''DayCount)
$(deriveJSON defaultOptions ''Threshold)
instance ToJSONKey Threshold where
  toJSONKey = genericToJSONKey opts
instance FromJSONKey Threshold where
  fromJSONKey = genericFromJSONKey opts


$(deriveJSON defaultOptions ''RateAssumption)
$(deriveJSON defaultOptions ''BookDirection)
$(deriveJSON defaultOptions ''Direction)

$(concat <$> traverse (deriveJSON defaultOptions) [''Limit] )
