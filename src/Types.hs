{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Types
  (DayCount(..),DateType(..),OverrideType(..)
  ,ActionOnDate(..),DealStatus(..),DatePattern(..)
  ,BondName,BondNames,FeeName,FeeNames,AccName,AccNames,AccountName
  ,Pre(..),Ts(..),TsPoint(..),PoolSource(..)
  ,DateDesp(..),Period(..), Threshold(..)
  ,RangeType(..),CutoffType(..),CustomDataType(..)
  ,Balance,DealStats(..),Index(..),FormulaType(..)
  ,DealCycle(..),Cmp(..),TimeHorizion(..)
  ,Date,Dates,TimeSeries(..),IRate,Amount,Rate,StartDate,EndDate
  ,Spread,Floor,Cap,Interest,Principal,Cash,Default,Loss,Rental
  ,ResultComponent(..),SplitType(..),BookItem(..),BookItems,BalanceSheetReport(..),CashflowReport(..)
  ,Floater,CeName,RateAssumption(..)
  ,PrepaymentRate,DefaultRate,RecoveryRate,RemainTerms,Recovery,Prepayment
  ,Table(..),lookupTable,LookupType(..),epocDate,BorrowerNum
  ,PricingMethod(..),sortActionOnDate,PriceResult(..),IRR,Limit(..)
  ,RoundingBy(..)
  ,TxnComment(..),Direction(..)
  )
  
  where

import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Time as Time
import qualified Data.Map as Map
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

import Data.List

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

type PrepaymentRate = Rate
type DefaultRate = Rate
type RecoveryRate = Rate
type RemainTerms = Int
type BorrowerNum = Int

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
            deriving (Show,Eq,Generic)

type Floater = (Index,Spread)

epocDate = Time.fromGregorian 1970 1 1
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
              deriving (Show, Eq, Generic)

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
            deriving (Show,Eq, Generic)

type DateVector = (Date, DatePattern)

data DateDesp = FixInterval (Map.Map DateType Date) Period Period 
                        --  cutoff pool       closing bond payment dates 
              | CustomDates Date [ActionOnDate] Date [ActionOnDate]
              | PatternInterval (Map.Map DateType (Date, DatePattern, Date))
              --             cutoff closing mRevolving end-date dp1-pc dp2-bond-pay 
              | PreClosingDates Date Date (Maybe Date) Date DateVector DateVector
              --             (last collect,last pay), mRevolving end-date dp1-pool-pay dp2-bond-pay
              | CurrentDates (Date,Date) (Maybe Date) Date DateVector DateVector
              deriving (Show,Eq, Generic)

data ActionOnDate = EarnAccInt Date AccName -- sweep bank account interest
                  | ChangeDealStatusTo Date DealStatus
                  | AccrueFee Date FeeName
                  | ResetLiqProvider Date String
                  | PoolCollection Date String
                  | RunWaterfall Date String
                  | DealClosed Date 
                  | InspectDS Date DealStats
                  | ResetIRSwapRate Date String
                  | ResetBondRate Date String
                  | BuildReport StartDate EndDate
                  deriving (Show,Generic,Read)


instance TimeSeries ActionOnDate where
    getDate (RunWaterfall d _) = d
    getDate (ResetLiqProvider d _) = d
    getDate (PoolCollection d _) = d
    getDate (EarnAccInt d _) = d
    getDate (AccrueFee d _) = d
    getDate (DealClosed d ) = d
    getDate (ChangeDealStatusTo d _ ) = d
    getDate (InspectDS d _ ) = d
    getDate (ResetIRSwapRate d _ ) = d
    getDate (ResetBondRate d _ ) = d 
    getDate (BuildReport sd ed) = ed

sortActionOnDate :: ActionOnDate -> ActionOnDate -> Ordering
sortActionOnDate a1 a2 
  | d1 == d2 = case (a1,a2) of
                 (BuildReport sd1 ed1 ,_) -> GT 
                 (_ , BuildReport sd1 ed1) -> LT
                 (_,_) -> EQ 
  | otherwise = compare d1 d2
  where 
    d1 = getDate a1 
    d2 = getDate a2 

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
                    deriving (Show,Generic)

data DealStatus = DealAccelerated (Maybe Date)
                | DealDefaulted (Maybe Date)
                | Amortizing
                | Revolving
                | Ended
                | PreClosing
                deriving (Show,Ord,Eq,Read, Generic)

data DealCycle = EndCollection
               | EndCollectionWF
               | BeginDistributionWF
               | EndDistributionWF
               | InWF
               deriving (Show, Ord, Eq, Read, Generic)

instance ToJSONKey DealCycle where
  toJSONKey = toJSONKeyText (T.pack . show)

instance FromJSONKey DealCycle where
  fromJSONKey = FromJSONKeyTextParser $ \t -> case readMaybe (T.unpack t) of
    Just k -> pure k
    Nothing -> fail ("Invalid key: " ++ show t)
 

data CustomDataType = CustomConstant Rational 
                    | CustomCurve    Ts 
                    | CustomDS       DealStats
                    deriving (Show,Ord,Eq,Read,Generic)

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
                 | AllDatePattern [DatePattern]
                 | StartsExclusive Date DatePattern
                 | Exclude DatePattern [DatePattern]
                 | OffsetBy DatePattern Int
                 -- | DayOfWeek Int -- T.DayOfWeek
                 deriving (Show,Eq, Generic)

data Direction = Credit
               | Debit
               deriving (Show,Ord, Eq,Read, Generic)

data TxnComment = PayInt [BondName]
                | PayYield BondName 
                | PayPrin [BondName] 
                | PayFee FeeName
                | SeqPayFee [FeeName] 
                | PayFeeYield FeeName
                | Transfer AccName AccName 
                | TransferBy AccName AccName Limit
                | PoolInflow PoolSource
                | LiquidationProceeds
                | LiquidationSupport String
                | LiquidationDraw
                | LiquidationRepay
                | LiquidationSupportInt Balance Balance
                | BankInt
                | Empty 
                | Tag String
                | UsingDS DealStats
                | UsingFormula FormulaType
                | SwapAccure
                | SwapInSettle
                | SwapOutSettle
                | PurchaseAsset
                | TxnDirection Direction
                | TxnComments [TxnComment]
                deriving (Eq, Show, Ord ,Read, Generic)

instance ToJSON TxnComment where 
  toJSON (PayInt bns ) = String $ T.pack $ "<PayInt:"++ concat bns ++ ">"
  toJSON (PayYield bn ) = String $ T.pack $ "<PayYield:"++ bn ++">"
  toJSON (PayPrin bns ) =  String $ T.pack $ "<PayPrin:"++ concat bns ++ ">"
  toJSON (PayFee fn ) =  String $ T.pack $ "<PayFee:" ++ fn ++ ">"
  toJSON (SeqPayFee fns) =  String $ T.pack $ "<SeqPayFee:"++ concat fns++">"
  toJSON (PayFeeYield fn) =  String $ T.pack $ "<PayFeeYield:"++ fn++">"
  toJSON (Transfer an1 an2) =  String $ T.pack $ "<Transfer:"++ an1 ++","++ an2++">"
  toJSON (TransferBy an1 an2 limit) =  String $ T.pack $ "<TransferBy:"++ an1 ++","++ an2++","++show limit++">"
  toJSON (PoolInflow ps) =  String $ T.pack $ "<PoolInflow:"++ show ps++">"
  toJSON LiquidationProceeds =  String $ T.pack $ "<Liquidation>"
  toJSON (UsingDS ds) =  String $ T.pack $ "<DS:"++ show ds++">"
  toJSON (UsingFormula fm) =  String $ T.pack $ "<Formula:"++ show fm++">"
  toJSON BankInt =  String $ T.pack $ "<BankInterest:>"
  toJSON Empty =  String $ T.pack $ "" 
  toJSON (TxnComments tcms) = Array $ V.fromList $ map toJSON tcms
  toJSON (LiquidationSupport source) = String $ T.pack $ "<Support:"++source++">"
  toJSON (LiquidationSupportInt b1 b2) =  String $ T.pack $ "<SupportExp:(Int:"++ show b1 ++ ",Fee:" ++ show b2 ++")>"
  toJSON LiquidationDraw = String $ T.pack $ "<Draw:>"
  toJSON LiquidationRepay = String $ T.pack $ "<Repay:>"
  toJSON SwapAccure = String $ T.pack $ "<Accure:>"
  toJSON SwapInSettle = String $ T.pack $ "<SettleIn:>"
  toJSON SwapOutSettle = String $ T.pack $ "<SettleOut:>"
  toJSON PurchaseAsset = String $ T.pack $ "<PurchaseAsset:>"
  toJSON (TxnDirection dr) = String $ T.pack $ "<TxnDirection:"++show dr++">"

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
  where 
      pat = "<(\\S+):(\\S+)>"::String
      sr = (T.unpack t =~ pat)::[[String]]
      tagName =  head sr!!1::String
      contents = head sr!!2::String

data PoolSource = CollectedInterest
                | CollectedPrincipal
                | CollectedRecoveries
                | CollectedPrepayment
                | CollectedRental
                | CollectedFee
                deriving (Show,Ord,Read,Eq, Generic)


data DealStats =  CurrentBondBalance
               | CurrentPoolBalance
               | CurrentPoolBegBalance
               | CurrentPoolDefaultedBalance
               | CumulativePoolDefaultedBalance
               | CumulativePoolRecoveriesBalance
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
               | LedgerBalance [String]
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
               | PoolNewDefaultedAt Date
               | BondBalanceGap String
               | BondBalanceGapAt Date String
               | FeePaidAt Date String
               | FeeTxnAmt [String] (Maybe TxnComment)
               | BondTxnAmt [String] (Maybe TxnComment)
               | AccTxnAmt  [String] (Maybe TxnComment)
               | FeeTxnAmtBy Date [String] (Maybe TxnComment)
               | BondTxnAmtBy Date [String] (Maybe TxnComment)
               | AccTxnAmtBy Date [String] (Maybe TxnComment)
               | FeesPaidAt Date [String] 
               | CurrentDueBondInt [String]
               | CurrentDueFee [String]
               | LastBondIntPaid [String]
               | LastBondPrinPaid [String]
               | LastFeePaid [String]
               | LastPoolDefaultedBal
               | LiqCredit [String]
               | LiqBalance [String]
               | BondBalanceHistory Date Date
               | PoolCollectionHistory PoolSource Date Date
               | TriggersStatusAt DealCycle Int
               | PoolWaRate
               | BondRate String
               | Factor DealStats Rational
               | Max [DealStats]
               | Min [DealStats]
               | Sum [DealStats]
               | Substract [DealStats]
               | Divide DealStats DealStats
               | Constant Rational
               | FloorAndCap DealStats DealStats DealStats
               | CustomData String Date
               | FloorWith DealStats DealStats
               | FloorWithZero DealStats
               | CapWith DealStats DealStats
               | Round DealStats (RoundingBy Balance)
               deriving (Show,Eq,Ord,Read,Generic)


data Cmp = G 
         | GE
         | L
         | LE
         | E
         deriving (Show,Generic,Eq)


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
         | Any [Pre]
         | All [Pre]
         deriving (Show,Generic,Eq)


data TsPoint a = TsPoint Date a
                deriving (Show,Eq,Read,Generic)

instance TimeSeries (TsPoint a) where 
    getDate (TsPoint d a) = d

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
        deriving (Show,Eq,Ord,Read,Generic)

data RangeType = II | IE | EI | EE
data CutoffType = Inc | Exc

data BookItem = Item String Balance 
              | ParentItem String [BookItem]
              deriving (Show,Read,Generic)

type BookItems = [BookItem]

data BalanceSheetReport = BalanceSheetReport {
                        asset :: BookItems
                        ,liability :: BookItems
                        ,equity :: BookItems
                        ,reportDate :: Date}
                        deriving (Show,Read,Generic)
 
data CashflowReport = CashflowReport {
                      inflow :: BookItems
                     ,outflow :: BookItems
                     ,net :: Balance
                     ,startDate :: Date 
                     ,endDate :: Date }
                deriving (Show,Read,Generic)

data ResultComponent = CallAt Date
                  | DealStatusChangeTo Date DealStatus DealStatus
                  | BondOutstanding String Balance Balance -- when deal ends
                  | BondOutstandingInt String Balance Balance -- when deal ends
                  | InspectBal Date DealStats Balance
                  | InspectInt Date DealStats Int
                  | InspectRate Date DealStats Rate
                  | InspectBool Date DealStats Bool
                  | FinancialReport StartDate EndDate BalanceSheetReport CashflowReport
                  deriving (Show, Generic)

data Threshold = Below
               | EqBelow
               | Above
               | EqAbove
               deriving (Show,Eq,Ord,Read,Generic)
      
instance ToJSONKey Threshold where
  toJSONKey = genericToJSONKey opts
instance FromJSONKey Threshold where
  fromJSONKey = genericFromJSONKey opts


data SplitType = EqToLeft   -- if equal, the element belongs to left
               | EqToRight  -- if equal, the element belongs to right
               | EqToLeftKeepOne
               | EqToLeftKeepOnes
               deriving (Show, Eq, Generic)

class TimeSeries ts where 
    cmp :: ts -> ts -> Ordering
    cmp t1 t2 = compare (getDate t1) (getDate t2)
    sameDate :: ts -> ts -> Bool
    sameDate t1 t2 =  (getDate t1) ==  (getDate t2)
    getDate :: ts -> Date
    getDates :: [ts] -> [Date]
    getDates ts = [ getDate t | t <- ts ]
    filterByDate :: [ts] -> Date -> [ts]
    filterByDate ts d = filter (\x -> getDate x == d ) ts
    beforeOnDate :: [ts] -> Date -> [ts]
    beforeOnDate ts d = filter (\x -> getDate x <= d ) ts

class Liable lb where 
  getDue :: lb -> Balance
  getLastPaidDate :: lb -> Date 

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

data RateAssumption = RateCurve Index Ts
                    | RateFlat Index IRate
                    deriving (Show,Generic)

data PricingMethod = BalanceFactor Rate Rate -- [balance] by performing & default balace
                   | BalanceFactor2 Rate Rate Rate --[balance] by performing/delinq/default factor
                   | DefaultedBalance Rate  --[balance] only liquidate defaulted balance
                   | PV IRate IRate -- discount factor, recovery pct on default
                   | PVCurve Ts --[CF] Pricing cashflow with a Curve
                   | Custom Rate -- custom amount
                   deriving (Show, Eq ,Generic)

type Valuation = Centi
type PerFace = Micro
type WAL = Centi
type Duration = Micro
type Convexity = Micro
type Yield = Micro
type AccruedInterest = Centi
type IRR = Rational
data YieldResult = Yield

data PriceResult = PriceResult Valuation PerFace WAL Duration Convexity AccruedInterest -- valuation,wal,accu,duration
                 | AssetPrice Valuation WAL Duration Convexity AccruedInterest
                 | ZSpread Spread 
                 deriving (Show, Eq, Generic)

data TimeHorizion = ByMonth
                  | ByYear
                  | ByQuarter

data FormulaType = ABCD
                 | Other
                 deriving (Show,Ord,Eq,Read,Generic)

data Limit = DuePct Balance  --
           | DueCapAmt Balance  -- due fee
           | RemainBalPct Rate -- pay till remain balance equals to a percentage of `stats`
           | KeepBalAmt DealStats -- pay till a certain amount remains in an account
           | Multiple Limit Float -- factor of a limit:w
           | Formula FormulaType
           | DS DealStats
           | ClearPDL String
           deriving (Show,Ord,Eq,Read,Generic)

data RoundingBy a = RoundCeil a 
                  | RoundFloor a
                  deriving (Show,Generic, Eq, Ord, Read)




$(deriveJSON defaultOptions ''Index)
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
$(deriveJSON defaultOptions ''DateDesp)
$(deriveJSON defaultOptions ''Period)
$(deriveJSON defaultOptions ''PoolSource)
$(deriveJSON defaultOptions ''CustomDataType)
$(deriveJSON defaultOptions ''ResultComponent)
$(deriveJSON defaultOptions ''CashflowReport)
$(deriveJSON defaultOptions ''BookItem)
$(deriveJSON defaultOptions ''BalanceSheetReport)
$(deriveJSON defaultOptions ''DealCycle)
$(deriveJSON defaultOptions ''Cmp)
$(deriveJSON defaultOptions ''PricingMethod)
$(deriveJSON defaultOptions ''FormulaType)
$(deriveJSON defaultOptions ''PriceResult)
$(deriveJSON defaultOptions ''Limit)
$(deriveJSON defaultOptions ''RoundingBy)