{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Assumptions (BondPricingInput(..)
                    ,AssumptionInput(..),ApplyAssumptionType(..)
                    ,lookupAssumptionByIdx,lookupRate,AssetPerfAssumption(..)
                    ,ExtraStress(..),RevolvingAssumption(..)
                    ,AssetPrepayAssumption(..),AssetDefaultAssumption(..),RecoveryAssumption(..)
                    ,getRateAssumption,projRates
                    ,LeaseAssetGapAssump(..)
                    ,LeaseAssetRentAssump(..)
                    ,NonPerfAssumption(..),AssetPerf
                    ,AssetDelinquencyAssumption(..)
                    ,AssetDelinqPerfAssumption(..),AssetDefaultedPerfAssumption(..)
                    ,getCDR,calcResetDates)
where

import Call as C
import Lib (Ts(..),TsPoint(..),toDate,mkRateTs)
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

lookupAssumptionByIdx :: [StratPerfByIdx] -> Int -> AssetPerf
lookupAssumptionByIdx sbi i
  = case find (\(indxs,_) -> Set.member i  (Set.fromList indxs) ) sbi of
        Just (_, aps ) ->  aps
        Nothing -> error ("Can't find idx"++ show i ++"in starfication list"++ show sbi)


data ApplyAssumptionType = PoolLevel AssetPerf
                           -- ^ assumption apply to all assets in the pool
                         | ByIndex [StratPerfByIdx]
                           -- ^ assumption which only apply to a set of assets in the pool
                         deriving (Show,Generic)

data NonPerfAssumption = NonPerfAssumption {
  stopRunBy :: Maybe Date                              -- ^ optional stop day,which will stop cashflow projection
  ,projectedExpense :: Maybe [(FeeName,Ts)]            -- ^ optional expense projection
  ,callWhen :: Maybe [C.CallOption]                    -- ^ optional call options set, once any of these were satisfied, then clean up waterfall is triggered
  ,revolving :: Maybe RevolvingAssumption              -- ^ optional revolving assumption with revoving assets
  ,interest :: Maybe [RateAssumption]                  -- ^ optional interest rates assumptions
  ,inspectOn :: Maybe [(DatePattern,DealStats)]        -- ^ optional tuple list to inspect variables during waterfall run
  ,buildFinancialReport :: Maybe DatePattern           -- ^ optional dates to build financial reports
  ,pricing :: Maybe BondPricingInput                   -- ^ optional bond pricing input( discount curve etc)
} deriving (Show,Generic)

data AssumptionInput = Single ApplyAssumptionType  NonPerfAssumption                          -- ^ one assumption request
                     | Multiple (Map.Map String ApplyAssumptionType)  NonPerfAssumption       -- ^ multiple assumption request in a single request
                     deriving (Show,Generic)

data AssetDefaultAssumption = DefaultConstant Rate
                            | DefaultCDR Rate
                            | DefaultVec [Rate]
                            deriving (Show,Generic)

data AssetPrepayAssumption = PrepaymentConstant Rate
                           | PrepaymentCPR Rate 
                           | PrepaymentVec [Rate] 
                           deriving (Show,Generic)

data AssetDelinquencyAssumption = DelinqCDR Rate (Lag,Rate)    -- ^ Annualized Rate to Delinq status , period lag become defaulted, loss rate, period lag become loss
                                | Dummy3
                                deriving (Show,Generic)

data LeaseAssetGapAssump = GapDays Int                         -- ^ days between leases, when creating dummy leases
                         | GapDaysByAmount [(Amount,Int)] Int  -- ^ days depends on the size of leases, when a default a default days for size greater
                         deriving (Show,Generic)

data LeaseAssetRentAssump = BaseAnnualRate Rate
                          | BaseCurve Ts 
                          deriving (Show,Generic)

data ExtraStress = ExtraStress {
                     defaultFactors :: Maybe Ts                 -- ^ stress default rate via a time series based factor curve
                     ,prepaymentFactors :: Maybe Ts             -- ^ stress prepayment rate via a time series based factor curve
                     ,poolHairCut :: Maybe [(PoolSource, Rate)] -- ^ haircut on pool income source
                   } deriving (Show,Generic)

data RecoveryAssumption = Recovery (Rate,Int)           -- ^ recovery rate, recovery lag
                        | RecoveryTiming (Rate,[Rate])  -- ^ recovery rate, with distribution of recoveries
                        deriving (Show,Generic)

type ExtendCashflowDates = DatePattern

data AssetDefaultedPerfAssumption = DefaultedRecovery Rate Int [Rate]
                                  | DummyDefaultAssump
                                  deriving (Show,Generic)

data AssetDelinqPerfAssumption = DummyDelinqAssump
                               deriving (Show,Generic)

data AssetPerfAssumption = MortgageAssump    (Maybe AssetDefaultAssumption) (Maybe AssetPrepayAssumption) (Maybe RecoveryAssumption)  (Maybe ExtraStress)
                         | MortgageDeqAssump (Maybe AssetDelinquencyAssumption) (Maybe AssetPrepayAssumption) (Maybe RecoveryAssumption) (Maybe ExtraStress)
                         | LeaseAssump       LeaseAssetGapAssump LeaseAssetRentAssump EndDate  (Maybe ExtraStress)
                         | LoanAssump        (Maybe AssetDefaultAssumption) (Maybe AssetPrepayAssumption) (Maybe RecoveryAssumption) (Maybe ExtraStress)
                         | InstallmentAssump (Maybe AssetDefaultAssumption) (Maybe AssetPrepayAssumption) (Maybe RecoveryAssumption) (Maybe ExtraStress)
                         deriving (Show,Generic)

data RevolvingAssumption = AvailableAssets RevolvingPool ApplyAssumptionType
                         | Dummy4 
                         deriving (Show,Generic)

data BondPricingInput = DiscountCurve Date Ts                               -- ^ PV curve used to discount bond cashflow and a PV date where cashflow discounted to 
                      | RunZSpread Ts (Map.Map BondName (Date,Rational))    -- ^ PV curve as well as bond trading price with a deal used to calc Z - spread
                      deriving (Show,Generic)

getCDR :: Maybe AssetDefaultAssumption -> Maybe Rate
getCDR (Just (DefaultCDR r)) = Just r
getCDR _ = Nothing

getIndexFromRateAssumption :: RateAssumption -> Index 
getIndexFromRateAssumption (RateCurve idx _) = idx
getIndexFromRateAssumption (RateFlat idx _) = idx

lookupRate :: [RateAssumption] -> Floater -> Date -> IRate 
lookupRate rAssumps (index,spd) d
  = case find (\x -> getIndexFromRateAssumption x == index ) rAssumps of 
      Just (RateCurve _ ts) -> spd + fromRational (getValByDate ts Inc d)
      Just (RateFlat _ r) -> r + spd
      Nothing -> error $ "Failed to find Index " ++ show index

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
projRates _ rt rassump ds = error ("Invalid rate type: "++ show rt++" assump"++ show rassump)


-- ^ Given a list of rates, calcualte whether rates was reset
calcResetDates :: [IRate] -> [Bool] -> [Bool]
calcResetDates [] bs = bs
calcResetDates (r:rs) bs 
  | null rs = calcResetDates [] (False:bs)
  | r == head rs = calcResetDates rs (bs++[False])
  | otherwise = calcResetDates rs (bs++[True])


$(deriveJSON defaultOptions ''BondPricingInput)
$(deriveJSON defaultOptions ''ApplyAssumptionType)
$(deriveJSON defaultOptions ''AssetPerfAssumption)
$(deriveJSON defaultOptions ''AssetDelinqPerfAssumption)
$(deriveJSON defaultOptions ''AssetDefaultedPerfAssumption)
$(deriveJSON defaultOptions ''AssetDefaultAssumption)
$(deriveJSON defaultOptions ''AssetPrepayAssumption)
$(deriveJSON defaultOptions ''RecoveryAssumption)
$(deriveJSON defaultOptions ''ExtraStress)
$(deriveJSON defaultOptions ''AssetDelinquencyAssumption)
$(deriveJSON defaultOptions ''LeaseAssetGapAssump)
$(deriveJSON defaultOptions ''LeaseAssetRentAssump)
$(deriveJSON defaultOptions ''RevolvingAssumption)
$(deriveJSON defaultOptions ''NonPerfAssumption)
