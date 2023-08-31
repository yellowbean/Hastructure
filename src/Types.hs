{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Types
  (DayCount(..),DateType(..),OverrideType(..),IssuanceFields(..)
  ,ActionOnDate(..),DealStatus(..),DatePattern(..)
  ,BondName,BondNames,FeeName,FeeNames,AccName,AccNames,AccountName
  ,Pre(..),Ts(..),TsPoint(..),PoolSource(..)
  ,DateDesp(..),Period(..), Threshold(..)
  ,RangeType(..),CutoffType(..),CustomDataType(..)
  ,Balance,DealStats(..),Index(..)
  ,DealCycle(..),Cmp(..),TimeHorizion(..)
  ,Date,Dates,TimeSeries(..),IRate,Amount,Rate,StartDate,EndDate
  ,Spread,Floor,Cap,Interest,Principal,Cash,Default,Loss,Rental,PrepaymentPenalty
  ,ResultComponent(..),SplitType(..),BookItem(..),BookItems,BalanceSheetReport(..),CashflowReport(..)
  ,Floater,CeName,RateAssumption(..)
  ,PrepaymentRate,DefaultRate,RecoveryRate,RemainTerms,Recovery,Prepayment
  ,Table(..),lookupTable,LookupType(..),epocDate,BorrowerNum
  ,PricingMethod(..),sortActionOnDate,PriceResult(..),IRR,Limit(..)
  ,RoundingBy(..),DateDirection(..)
  ,TxnComment(..),Direction(..)
  )
  
  where

import qualified Data.Text as Text
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
type PrepaymentPenalty = Centi

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
            | IRPH --  The IRPH (Índice de Referencia de Préstamos Hipotecarios) is a reference index used in Spain to fix the interest rate of mortgage loans
            | SONIA 
            deriving (Show,Eq,Generic)

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

data ActionOnDate = EarnAccInt Date AccName              -- sweep bank account interest
                  | ChangeDealStatusTo Date DealStatus   -- ^ change deal status
                  | AccrueFee Date FeeName               -- ^ accure fee
                  | ResetLiqProvider Date String         -- ^ reset credit for liquidity provider
                  | ResetLiqProviderRate Date String     -- ^ accure interest/premium amount for liquidity provider
                  | PoolCollection Date String           -- ^ collect pool cashflow and deposit to accounts
                  | RunWaterfall Date String             -- ^ execute waterfall
                  | DealClosed Date                      
                  | InspectDS Date DealStats             -- ^ inspect formula
                  | ResetIRSwapRate Date String          -- ^ reset interest rate swap dates
                  | ResetBondRate Date String            -- ^ reset bond interest rate per bond's interest rate info
                  | BuildReport StartDate EndDate        -- ^ build cashflow report between dates and balance report at end date
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
                | PayPrinResidual [BondName] 
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
  toJSON (PayPrinResidual bns ) =  String $ T.pack $ "<PayPrinResidual:"++ concat bns ++ ">"
  toJSON (PayFee fn ) =  String $ T.pack $ "<PayFee:" ++ fn ++ ">"
  toJSON (SeqPayFee fns) =  String $ T.pack $ "<SeqPayFee:"++ concat fns++">"
  toJSON (PayFeeYield fn) =  String $ T.pack $ "<PayFeeYield:"++ fn++">"
  toJSON (Transfer an1 an2) =  String $ T.pack $ "<Transfer:"++ an1 ++","++ an2++">"
  toJSON (TransferBy an1 an2 limit) =  String $ T.pack $ "<TransferBy:"++ an1 ++","++ an2++","++show limit++">"
  toJSON (PoolInflow ps) =  String $ T.pack $ "<PoolInflow:"++ show ps++">"
  toJSON LiquidationProceeds =  String $ T.pack $ "<Liquidation>"
  toJSON (UsingDS ds) =  String $ T.pack $ "<DS:"++ show ds++">"
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

data IssuanceFields = IssuanceBalance      -- ^ pool issuance balance
                    | HistoryRecoveries    -- ^ cumulative recoveries
                    | HistoryInterest      -- ^ cumulative interest collected
                    | HistoryPrepayment    -- ^ cumulative prepayment collected
                    | HistoryPrincipal     -- ^ cumulative principal collected
                    | HistoryRental        -- ^ cumulative rental collected
                    deriving (Show,Ord,Eq,Read,Generic)

instance ToJSONKey IssuanceFields where
  toJSONKey = toJSONKeyText (Text.pack . show)

instance FromJSONKey IssuanceFields where
  fromJSONKey = FromJSONKeyTextParser $ \t -> case readMaybe (Text.unpack t) of
    Just k -> pure k
    Nothing -> fail ("Invalid key: " ++ show t)

data PoolSource = CollectedInterest               -- ^ interest
                | CollectedPrincipal              -- ^ schdule principal
                | CollectedRecoveries             -- ^ recoveries 
                | CollectedPrepayment             -- ^ prepayment
                | CollectedPrepaymentPenalty      -- ^ prepayment pentalty
                | CollectedRental                 -- ^ rental from pool
                | NewDefaults
                | NewLosses
                deriving (Show,Ord,Read,Eq, Generic)


data DealStats = CurrentBondBalance
               | CurrentPoolBalance
               | CurrentPoolBegBalance
               | CurrentPoolDefaultedBalance
               | CumulativePoolDefaultedBalance   -- Depreciated, use PoolCumCollection
               | CumulativePoolRecoveriesBalance  -- Depreciated, use PoolCumCollection
               | CumulativeNetLoss
               | CumulativePoolDefaultedRate
               | CumulativePoolDefaultedRateTill Int
               | CumulativeNetLossRatio
               | OriginalBondBalance
               | OriginalPoolBalance
               | CurrentPoolBorrowerNum
               | BondFactor
               | PoolFactor
               | BondWaRate [BondName]
               | UseCustomData String
               | PoolCumCollection [PoolSource]
               | PoolCumCollectionTill Int [PoolSource]
               | PoolCurCollection [PoolSource]
               | PoolCollectionStats Int [PoolSource]
               | AllAccBalance
               | AccBalance [AccName]
               | LedgerBalance [String]
               | LedgerTxnAmt [String] (Maybe TxnComment)
               | ReserveAccGap [AccName]
               | ReserveExcess [AccName] 
               | MonthsTillMaturity BondName
               | ReserveAccGapAt Date [AccName] 
               | ReserveExcessAt Date [AccName] 
               | FutureCurrentPoolBalance
               -- | FutureCurrentPoolBegBalance Date
               | FutureCurrentPoolBegBalance
               | FutureCurrentBondBalance Date
               | FutureCurrentBondFactor Date
               | FutureCurrentPoolFactor Date
               | FutureCurrentPoolBorrowerNum Date
               | FutureOriginalPoolBalance
               | CurrentBondBalanceOf [BondName]
               | IsMostSenior BondName [BondName]
               | BondIntPaidAt Date BondName
               | BondsIntPaidAt Date [BondName]
               | BondPrinPaidAt Date BondName
               | BondsPrinPaidAt Date [BondName]
               | BondBalanceGap BondName
               | BondBalanceGapAt Date BondName
               | FeePaidAt Date FeeName
               | FeeTxnAmt [FeeName] (Maybe TxnComment)
               | BondTxnAmt [BondName] (Maybe TxnComment)
               | AccTxnAmt  [AccName] (Maybe TxnComment)
               | FeeTxnAmtBy Date [FeeName] (Maybe TxnComment)
               | BondTxnAmtBy Date [BondName] (Maybe TxnComment)
               | AccTxnAmtBy Date [AccName] (Maybe TxnComment)
               | FeesPaidAt Date [FeeName] 
               | CurrentDueBondInt [BondName]
               | CurrentDueFee [FeeName]
               | LastBondIntPaid [BondName]
               | LastBondPrinPaid [BondName]
               | LastFeePaid [FeeName]
               | LiqCredit [String]
               | LiqBalance [String]
               | BondBalanceHistory Date Date
               | PoolCollectionHistory PoolSource Date Date
               | TriggersStatusAt DealCycle Int
               | TestRate DealStats Cmp Micro
               | TestAny Bool [DealStats]
               | TestAll Bool [DealStats]
               | PoolWaRate
               | BondRate BondName
               | Factor DealStats Rational
               | Max [DealStats]
               | Min [DealStats]
               | Sum [DealStats]
               | Substract [DealStats]
               | Avg [DealStats]
               | Divide DealStats DealStats
               | Constant Rational
               | FloorAndCap DealStats DealStats DealStats
               | CustomData String Date
               | FloorWith DealStats DealStats
               | FloorWithZero DealStats
               | CapWith DealStats DealStats
               | Abs DealStats
               | Round DealStats (RoundingBy Balance)
               deriving (Show,Eq,Ord,Read,Generic)


data Cmp = G      -- ^ Greater than 
         | GE     -- ^ Greater Equal than
         | L      -- ^ Less than
         | LE     -- ^ Less Equal than
         | E      -- ^ Equals to
         deriving (Show,Generic,Eq,Ord,Read)


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
         | All [Pre]                            -- ^ 
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

data RangeType = II | IE | EI | EE | NO_IE

data CutoffType = Inc | Exc

data DateDirection = Future | Past

type BookItems = [BookItem]

data BookItem = Item String Balance 
              | ParentItem String BookItems
              deriving (Show,Read,Generic)


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
                     | DealStatusChangeTo Date DealStatus DealStatus  -- ^ record when status changed
                     | BondOutstanding String Balance Balance         -- ^ when deal ends,calculate oustanding principal balance 
                     | BondOutstandingInt String Balance Balance      -- ^ when deal ends,calculate oustanding interest due 
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
    sameDate t1 t2 =  getDate t1 == getDate t2
    getDate :: ts -> Date
    getDates :: [ts] -> [Date]
    getDates ts = [ getDate t | t <- ts ]
    filterByDate :: [ts] -> Date -> [ts]
    filterByDate ts d = filter (\x -> getDate x == d ) ts
    sliceBy :: RangeType -> StartDate -> EndDate -> [ts] -> [ts]
    sliceBy rt sd ed ts
      = case rt of 
          II -> filter (\x -> (getDate x) >= sd && (getDate x) <= ed ) ts 
          IE -> filter (\x -> (getDate x) >= sd  && (getDate x) < ed ) ts 
          EI -> filter (\x -> (getDate x) > sd && (getDate x) <= ed) ts 
          EE -> filter (\x -> (getDate x) > sd && (getDate x) < ed ) ts 
          _  -> error "Not support NO_IE for sliceBy in TimeSeries"
    cutBy :: CutoffType -> DateDirection -> Date -> [ts] -> [ts]
    cutBy ct dd d ts 
      = case (ct,dd) of
          (Inc, Future) ->  filter (\x -> getDate x >= d) ts
          (Inc, Past) ->  filter (\x -> getDate x <= d) ts
          (Exc, Future) ->  filter (\x -> getDate x > d) ts
          (Exc, Past) ->  filter (\x -> getDate x < d) ts


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

data Limit = DuePct Rate  --
           | DueCapAmt Balance  -- due fee
           | KeepBalAmt DealStats -- pay till a certain amount remains in an account
           | DS DealStats
           | ClearLedger String
           | BookLedger String
           | RemainBalPct Rate -- pay till remain balance equals to a percentage of `stats`
           | TillTarget
           | TillSource
           | Multiple Limit Float -- factor of a limit
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
$(deriveJSON defaultOptions ''PriceResult)
$(deriveJSON defaultOptions ''Limit)
$(deriveJSON defaultOptions ''RoundingBy)
$(deriveJSON defaultOptions ''IssuanceFields)
