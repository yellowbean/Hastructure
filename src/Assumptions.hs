{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Assumptions (BondPricingInput(..)
                    ,AssumptionInput(..),ApplyAssumptionType(..)
                    ,lookupAssumptionByIdx,lookupRate,AssetPerfAssumption(..)
                    ,ExtraStress(..),RevolvingAssumption(..)
                    ,AssetPrepayAssumption(..),AssetDefaultAssumption(..),RecoveryAssumption(..)
                    ,getRateAssumption,projRates,lookupRate0
                    ,LeaseAssetGapAssump(..)
                    ,LeaseAssetRentAssump(..)
                    ,NonPerfAssumption(..),AssetPerf
                    ,AssetDelinquencyAssumption(..)
                    ,AssetDelinqPerfAssumption(..),AssetDefaultedPerfAssumption(..)
                    ,getCDR,calcResetDates,IssueBondEvent(..)
                    ,TagMatchRule(..),ObligorStrategy(..),RefiEvent(..),InspectType(..)
                    ,FieldMatchRule(..),CallOpt(..))
where

import Call as C
import Lib (Ts(..),TsPoint(..),toDate,mkRateTs)
import Liability (Bond,InterestInfo)
import Util
import DateUtil
import qualified Data.Map as Map 
import Data.List
import qualified Data.Set as Set
import Data.Aeson hiding (json)
import Language.Haskell.TH
import Data.Aeson.TH
import Data.Aeson.Types
import Types
import qualified Data.Time as T
import Data.Fixed
import Data.Ratio
import Revolving
import GHC.Generics
import AssetClass.AssetBase
import Debug.Trace
import InterestRate
debug = flip trace

type AssetPerf = (AssetPerfAssumption,AssetDelinqPerfAssumption,AssetDefaultedPerfAssumption)
type StratPerfByIdx = ([Int],AssetPerf)

lookupAssumptionByIdx :: [StratPerfByIdx] -> Int -> Either String AssetPerf
lookupAssumptionByIdx sbi i
  = case find (\(indxs,_) -> Set.member i  (Set.fromList indxs) ) sbi of
        Just (_, aps ) ->  Right aps
        Nothing -> Left ("Lookup assumption by ID: Can't find idx"++ show i ++"in starfication list"++ show sbi)

type ObligorTagStr = String

data TagMatchRule = TagEq      -- ^ match exactly
                  | TagSubset
                  | TagSuperset
                  | TagAny     -- ^ match any tag hit
                  | TagNot  TagMatchRule   -- ^ Negative match
                  deriving (Show, Generic, Read)

data FieldMatchRule = FieldIn String [String]
                    | FieldCmp String Cmp Double
                    | FieldInRange String RangeType Double Double
                    | FieldNot FieldMatchRule
                    deriving (Show, Generic, Read)


data ObligorStrategy = ObligorById [String] AssetPerf
                     | ObligorByTag [ObligorTagStr] TagMatchRule AssetPerf
                     | ObligorByField [FieldMatchRule] AssetPerf
                     | ObligorByDefault AssetPerf
                     deriving (Show, Generic, Read)

data ApplyAssumptionType = PoolLevel AssetPerf -- ^ assumption apply to all assets in the pool
                         | ByIndex [StratPerfByIdx] -- ^ assumption which only apply to a set of assets in the pool
                         | ByName (Map.Map PoolId AssetPerf) -- ^ assumption for a named pool
                         | ByObligor [ObligorStrategy]
                         | ByPoolId (Map.Map PoolId ApplyAssumptionType) -- ^ assumption for a pool
                         | ByDealName (Map.Map DealName (ApplyAssumptionType, NonPerfAssumption)) -- ^ assumption for a named deal 
                         deriving (Show, Generic)

type RateFormula = DealStats
type BalanceFormula = DealStats

data IssueBondEvent = IssueBondEvent (Maybe Pre) BondName AccName Bond (Maybe BalanceFormula) (Maybe RateFormula)
                    | FundingBondEvent (Maybe Pre) BondName AccName Balance 
                    deriving (Show, Generic, Read)

data RefiEvent = RefiRate AccountName BondName InterestInfo
               | RefiBond AccountName Bond
               | RefiEvents [RefiEvent]
               deriving (Show, Generic, Read)

data InspectType = InspectPt DatePattern DealStats
                 | InspectRpt DatePattern [DealStats]
                 deriving (Show, Generic, Read)

data CallOpt = LegacyOpts [C.CallOption]                 -- ^ legacy support
             | CallPredicate [Pre]                           -- ^ default test call for each pay day, keep backward compatible
             | CallOnDates DatePattern [Pre]             -- ^ test call at end of day
             deriving (Show, Generic, Read, Ord, Eq)

data NonPerfAssumption = NonPerfAssumption {
  stopRunBy :: Maybe Date                                    -- ^ optional stop day,which will stop cashflow projection
  ,projectedExpense :: Maybe [(FeeName,Ts)]                  -- ^ optional expense projection
  ,callWhen :: Maybe [CallOpt]                               -- ^ optional call options set, once any of these were satisfied, then clean up waterfall is triggered
  ,revolving :: Maybe RevolvingAssumption                    -- ^ optional revolving assumption with revoving assets
  ,interest :: Maybe [RateAssumption]                        -- ^ optional interest rates assumptions
  ,inspectOn :: Maybe [InspectType]                          -- ^ optional tuple list to inspect variables during waterfall run
  ,buildFinancialReport :: Maybe DatePattern                 -- ^ optional dates to build financial reports
  ,pricing :: Maybe BondPricingInput                         -- ^ optional bond pricing input( discount curve etc)
  ,fireTrigger :: Maybe [(Date,DealCycle,String)]            -- ^ optional fire a trigger
  ,makeWholeWhen :: Maybe (Date,Spread,Table Float Spread)
  ,issueBondSchedule :: Maybe [TsPoint IssueBondEvent]                            
  ,refinance :: Maybe [TsPoint RefiEvent]
} deriving (Show, Generic)

data AssumptionInput = Single ApplyAssumptionType  NonPerfAssumption                          -- ^ one assumption request
                     | Multiple (Map.Map String ApplyAssumptionType)  NonPerfAssumption       -- ^ multiple assumption request in a single request
                     deriving (Show,Generic)

data AssetDefaultAssumption = DefaultConstant Rate              -- ^ using constant default rate
                            | DefaultCDR Rate                   -- ^ using annualized default rate
                            | DefaultVec [Rate]                 -- ^ using default rate vector
                            | DefaultVecPadding [Rate]          -- ^ using default rate vector
                            | DefaultByAmt (Balance,[Rate])
                            | DefaultAtEnd                      -- ^ default 100% at end
                            | DefaultAtEndByRate Rate Rate      -- ^ life time default rate and default rate at end
                            | DefaultStressByTs Ts AssetDefaultAssumption
                            deriving (Show,Generic,Read)

data AssetPrepayAssumption = PrepaymentConstant Rate
                           | PrepaymentCPR Rate 
                           | PrepaymentVec [Rate] 
                           | PrepaymentVecPadding [Rate] 
                           | PrepayByAmt (Balance,[Rate])
                           | PrepayStressByTs Ts AssetPrepayAssumption
                           deriving (Show,Generic,Read)

data AssetDelinquencyAssumption = DelinqCDR Rate (Lag,Rate)                 -- ^ Annualized Rate to Delinq status , period lag become defaulted, loss rate, period lag become loss
                                | DelinqByAmt (Balance,[Rate]) (Lag,Rate)
                                | Dummy3
                                deriving (Show,Generic,Read)

data RecoveryAssumption = Recovery (Rate,Int)                    -- ^ recovery rate, recovery lag
                        | RecoveryTiming (Rate,[Rate])           -- ^ recovery rate, with distribution of recoveries
                        | RecoveryByDays Rate [(Int, Rate)]      -- ^ recovery rate, with distribution of recoveries by offset dates
                        deriving (Show,Generic,Read)

data LeaseAssetGapAssump = GapDays Int                         -- ^ days between leases, when creating dummy leases
                         | GapDaysByAmount [(Amount,Int)] Int  -- ^ days depends on the size of leases, when a default a default days for size greater
                         deriving (Show,Generic,Read)

data LeaseAssetRentAssump = BaseAnnualRate Rate
                          | BaseCurve Ts 
                          deriving (Show,Generic,Read)

data ExtraStress = ExtraStress {
                     defaultFactors :: Maybe Ts                 -- ^ stress default rate via a time series based factor curve
                     ,prepaymentFactors :: Maybe Ts             -- ^ stress prepayment rate via a time series based factor curve
                     ,poolHairCut :: Maybe [(PoolSource, Rate)] -- ^ haircut on pool income source
                   } deriving (Show,Generic,Read)

type ExtendCashflowDates = DatePattern

data AssetDefaultedPerfAssumption = DefaultedRecovery Rate Int [Rate]
                                  | DummyDefaultAssump
                                  deriving (Show,Generic,Read)

data AssetDelinqPerfAssumption = DummyDelinqAssump
                               deriving (Show,Generic,Read)

data AssetPerfAssumption = MortgageAssump    (Maybe AssetDefaultAssumption) (Maybe AssetPrepayAssumption) (Maybe RecoveryAssumption)  (Maybe ExtraStress)
                         | MortgageDeqAssump (Maybe AssetDelinquencyAssumption) (Maybe AssetPrepayAssumption) (Maybe RecoveryAssumption) (Maybe ExtraStress)
                         | LeaseAssump       LeaseAssetGapAssump LeaseAssetRentAssump EndDate  (Maybe ExtraStress)
                         | LoanAssump        (Maybe AssetDefaultAssumption) (Maybe AssetPrepayAssumption) (Maybe RecoveryAssumption) (Maybe ExtraStress)
                         | InstallmentAssump (Maybe AssetDefaultAssumption) (Maybe AssetPrepayAssumption) (Maybe RecoveryAssumption) (Maybe ExtraStress)
                         | ReceivableAssump  (Maybe AssetDefaultAssumption) (Maybe RecoveryAssumption) (Maybe ExtraStress)
                         | FixedAssetAssump  Ts Ts   -- util rate, price
                         deriving (Show,Generic,Read)

data RevolvingAssumption = AvailableAssets RevolvingPool ApplyAssumptionType
                         | AvailableAssetsBy (Map.Map String (RevolvingPool, ApplyAssumptionType))
                         deriving (Show,Generic)

type HistoryCash = Ts
type CurrentHolding = Balance
type PricingDate = Date


data BondPricingInput = DiscountCurve PricingDate Ts                               -- ^ PV curve used to discount bond cashflow and a PV date where cashflow discounted to 
                      | RunZSpread Ts (Map.Map BondName (Date,Rational))    -- ^ PV curve as well as bond trading price with a deal used to calc Z - spread
                      -- | OASInput Date BondName Balance [Spread] (Map.Map String Ts)                        -- ^ only works in multiple assumption request 
                      | DiscountRate PricingDate Rate
                      | IRRInput  (Map.Map BondName (HistoryCash,CurrentHolding,Maybe (Dates, PricingMethod)))        -- ^ IRR calculation for a list of bonds
                      deriving (Show,Generic)

getCDR :: Maybe AssetDefaultAssumption -> Maybe Rate
getCDR (Just (DefaultCDR r)) = Just r
getCDR _ = Nothing

getIndexFromRateAssumption :: RateAssumption -> Index 
getIndexFromRateAssumption (RateCurve idx _) = idx
getIndexFromRateAssumption (RateFlat idx _) = idx


-- ^ lookup rate from rate assumption with index and spread
lookupRate :: [RateAssumption] -> Floater -> Date -> IRate 
lookupRate rAssumps (index,spd) d
  = case find (\x -> getIndexFromRateAssumption x == index ) rAssumps of 
      Just (RateCurve _ ts) -> spd + fromRational (getValByDate ts Inc d)
      Just (RateFlat _ r) -> r + spd
      Nothing -> error $ "Failed to find Index " ++ show index

-- ^ lookup rate from rate assumption with index
lookupRate0 :: [RateAssumption] -> Index -> Date -> Either String IRate 
lookupRate0 rAssumps index d
  = case find (\x -> getIndexFromRateAssumption x == index ) rAssumps of 
      Just (RateCurve _ ts) -> Right $ fromRational (getValByDate ts Inc d)
      Just (RateFlat _ r) -> Right r
      Nothing -> Left $ "Failed to find Index " ++ show index ++ " from Rate Assumption" ++ show rAssumps


getRateAssumption :: [RateAssumption] -> Index -> Maybe RateAssumption
getRateAssumption assumps idx
  = find (\case
           (RateCurve _idx _) -> idx == _idx 
           (RateFlat _idx _) -> idx == _idx
           _ -> False)
         assumps

-- | project rates used by rate type ,with interest rate assumptions and observation dates
projRates :: IRate -> RateType -> Maybe [RateAssumption] -> [Date] -> [IRate]
projRates sr (Fix _ r) _ ds = replicate (length ds) sr 
projRates sr (Floater _ idx spd r dp rfloor rcap mr) (Just assumps) ds 
  = case getRateAssumption assumps idx of
      Nothing -> error ("Failed to find index rate " ++ show idx ++ " from "++ show assumps)
      Just _rateAssumption -> 
        let 
          resetDates = genSerialDatesTill2 NO_IE (head ds) dp (last ds)
          ratesFromCurve = case _rateAssumption of
                             (RateCurve _ ts) -> (\x -> spd + (fromRational x) ) <$> (getValByDates ts Inc resetDates)
                             (RateFlat _ v)   -> (spd +) <$> replicate (length resetDates) v
                             _ -> error ("Invalid rate type "++ show _rateAssumption)
          ratesUsedByDates =  getValByDates
                                (mkRateTs $ zip ((head ds):resetDates) (sr:ratesFromCurve))
                                Inc
                                ds 
        in 
          case (rfloor,rcap) of 
            (Nothing, Nothing) -> fromRational <$> ratesUsedByDates  
            (Just fv, Just cv) -> capWith cv $ floorWith fv $ fromRational <$> ratesUsedByDates 
            (Just fv, Nothing) -> floorWith fv $ fromRational <$> ratesUsedByDates 
            (Nothing, Just cv) -> capWith cv $ fromRational <$> ratesUsedByDates 
projRates _ rt rassump ds = error ("Invalid rate type: "++ show rt++" assump: "++ show rassump)


-- ^ Given a list of rates, calcualte whether rates was reset
calcResetDates :: [IRate] -> [Bool] -> [Bool]
calcResetDates [] bs = bs
calcResetDates (r:rs) bs 
  | null rs = calcResetDates [] (False:bs)
  | r == head rs = calcResetDates rs (bs++[False])
  | otherwise = calcResetDates rs (bs++[True])


$(deriveJSON defaultOptions ''CallOpt)
$(deriveJSON defaultOptions ''BondPricingInput)
$(deriveJSON defaultOptions ''IssueBondEvent)
$(deriveJSON defaultOptions ''RefiEvent)



$(concat <$> traverse (deriveJSON defaultOptions) [''FieldMatchRule,''TagMatchRule, ''ObligorStrategy,''ApplyAssumptionType, ''AssetPerfAssumption
  , ''AssetDefaultedPerfAssumption, ''AssetDelinqPerfAssumption, ''NonPerfAssumption, ''AssetDefaultAssumption
  , ''AssetPrepayAssumption, ''RecoveryAssumption, ''ExtraStress
  , ''LeaseAssetGapAssump, ''LeaseAssetRentAssump, ''RevolvingAssumption, ''AssetDelinquencyAssumption,''InspectType])
