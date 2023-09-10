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
                    ,NonPerfAssumption(..)
                    ,AssetDelinquencyAssumption(..)
                    ,getCDR
                    )
where

import Call as C
import Lib (Ts(..),TsPoint(..),toDate,mkRateTs)
import Util
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

type StratificationByIdx = ([Int],AssetPerfAssumption)

lookupAssumptionByIdx :: [StratificationByIdx] -> Int -> AssetPerfAssumption
lookupAssumptionByIdx sbi i
  = case find (\(indxs,_) -> Set.member i  (Set.fromList indxs) ) sbi of
        Just (_, aps ) ->  aps
        Nothing -> error ("Can't find idx"++ (show i)++"in starfication list"++ (show sbi))

data ApplyAssumptionType = PoolLevel AssetPerfAssumption -- (Maybe [InterestAssumption])-- ^ assumption apply to all assets in the pool
                         | ByIndex [StratificationByIdx] -- (Maybe [InterestAssumption])-- ^ assumption which only apply to a set of assets in the pool
                         deriving (Show,Generic)

data NonPerfAssumption = NonPerfAssumption {
  stopRunBy :: Maybe Date
  ,projectedExpense :: Maybe (FeeName,Ts)
  ,callWhen :: Maybe [C.CallOption]
  ,revolving :: Maybe RevolvingAssumption
  ,interest :: Maybe [RateAssumption]
  ,inspectOn :: Maybe [(DatePattern,DealStats)]
  ,buildFinancialReport :: Maybe DatePattern
  ,pricing :: Maybe BondPricingInput
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

data AssetDelinquencyAssumption = DelinqCDR Rate Lag Rate Lag -- Annualized Rate to Delinq status , period lag become defaulted, loss rate, period become loss
                                | Dummy3
                                deriving (Show,Generic)

data LeaseAssetGapAssump = GapDays Int 
                         | GapDaysByAmount [(Amount,Int)] Int
                         deriving (Show,Generic)

data LeaseAssetRentAssump = BaseAnnualRate Rate
                          | BaseCurve Ts 
                          deriving (Show,Generic)

data ExtraStress = ExtraStress {
                 defaultFactors :: Maybe Ts              -- ^ stress default rate via a time series based factor curve
                 ,prepaymentFactors :: Maybe Ts          -- ^ stress prepayment rate via a time series based factor curve
                 ,poolHairCut :: Maybe (PoolSource,Rate) -- ^ haircut on pool income source
                 } deriving (Show,Generic)

data RecoveryAssumption = Recovery (Rate,Int)           -- ^ recovery rate, recovery lag
                        | RecoveryTiming (Rate,[Rate])  -- ^ recovery rate, with distribution of recoveries
                        deriving (Show,Generic)

data AssetPerfAssumption = MortgageAssump    (Maybe AssetDefaultAssumption) (Maybe AssetPrepayAssumption) (Maybe RecoveryAssumption)  (Maybe ExtraStress)
                         | MortgageDeqAssump (Maybe AssetDelinquencyAssumption) (Maybe ExtraStress)
                         | LeaseAssump       LeaseAssetGapAssump LeaseAssetRentAssump EndDate  (Maybe ExtraStress)
                         | LoanAssump        (Maybe AssetDefaultAssumption) (Maybe AssetPrepayAssumption) (Maybe RecoveryAssumption) (Maybe ExtraStress)
                         | InstallmentAssump (Maybe AssetDefaultAssumption) (Maybe AssetPrepayAssumption) (Maybe RecoveryAssumption) (Maybe ExtraStress)
                         | DefaultedRecovery Rate Int [Rate]
                         deriving (Show,Generic)

data RevolvingAssumption = AvailableAssets RevolvingPool AssetPerfAssumption
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
      Nothing -> error $ "Failed to find Index "++show index

getRateAssumption :: [RateAssumption] -> Index -> Maybe RateAssumption
getRateAssumption assumps idx
  = find (\case
           (RateCurve _idx _) -> idx == _idx 
           (RateFlat _idx _) -> idx == _idx
           _ -> False)
         assumps

-- | project rates used by rate type ,with interest rate assumptions and observation dates
projRates :: RateType -> Maybe [RateAssumption] -> [Date] -> [IRate]
projRates (Fix r) _ ds = replicate (length ds) r 
projRates (Floater idx spd r dp rfloor rcap mr) (Just assumps) ds 
  = case getRateAssumption assumps idx of
      Nothing -> error ("Failed to find index rate " ++ show idx ++ " from "++ show assumps)
      Just _rateAssumption -> 
        let 
          resetDates = genSerialDatesTill2 NO_IE (head ds) dp (last ds)
          ratesFromCurve = case _rateAssumption of
                             (RateCurve _ ts)   -> (\x -> spd + (fromRational x) ) <$>  (getValByDates ts Inc resetDates)
                             (RateFlat _ v) -> (spd +) <$> replicate (length resetDates) v
                             _ -> error ("Invalid rate type "++ show _rateAssumption)
          ratesUsedByDates =  getValByDates
                                (mkRateTs $ zip resetDates ratesFromCurve)
                                Inc
                                ds
        in 
          case (rfloor,rcap) of 
            (Nothing, Nothing) -> fromRational <$> ratesUsedByDates 
            (Just fv, Just cv) -> capWith cv $ floorWith fv $ fromRational <$> ratesUsedByDates 
            (Just fv, Nothing) -> floorWith fv $ fromRational <$> ratesUsedByDates 
            (Nothing, Just cv) -> capWith cv $ fromRational <$> ratesUsedByDates 


$(deriveJSON defaultOptions ''BondPricingInput)
$(deriveJSON defaultOptions ''ApplyAssumptionType)
$(deriveJSON defaultOptions ''AssetPerfAssumption)
$(deriveJSON defaultOptions ''AssetDefaultAssumption)
$(deriveJSON defaultOptions ''AssetPrepayAssumption)
$(deriveJSON defaultOptions ''RecoveryAssumption)
$(deriveJSON defaultOptions ''ExtraStress)
$(deriveJSON defaultOptions ''AssetDelinquencyAssumption)
$(deriveJSON defaultOptions ''LeaseAssetGapAssump)
$(deriveJSON defaultOptions ''LeaseAssetRentAssump)
$(deriveJSON defaultOptions ''RevolvingAssumption)
$(deriveJSON defaultOptions ''NonPerfAssumption)
