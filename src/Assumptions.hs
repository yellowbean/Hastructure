{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Assumptions (BondPricingInput(..)
                    ,AssumptionInput(..),ApplyAssumptionType(..)
                    ,lookupAssumptionByIdx,lookupRate,AssetPerfAssumption(..)
                    ,ExtraStress(..)
                    ,AssetPrepayAssumption(..),AssetDefaultAssumption(..),RecoveryAssumption(..)
                    ,getRateAssumption,projRates
                    ,InterestAssumption(..)
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

data ApplyAssumptionType = PoolLevel AssetPerfAssumption -- ^ assumption apply to all assets in the pool
                         | ByIndex [StratificationByIdx] -- ^ assumption which only apply to a set of assets in the pool
                         deriving (Show,Generic)

data AssumptionInput = Single ApplyAssumptionType                          -- ^ one assumption request
                     | Multiple (Map.Map String ApplyAssumptionType)       -- ^ multiple assumption request in a single request
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

data InterestAssumption = InterestRateCurve Index Ts 
                        | InterestRateConstant Index IRate
                        deriving (Show,Generic)

data LeaseAssetGapAssump = GapDays Int 
                         | GapDaysByAmount [(Amount,Int)] Int
                         deriving (Show,Generic)

data LeaseAssetRentAssump = BaseAnnualRate Rate
                          | BaseCurve Ts 
                          deriving (Show,Generic)

data ExtraStress = DefaultFactors Ts 
                 | PrepaymentFactors Ts
                 | PoolHairCut PoolSource Rate
                 | ExtraStresses [ExtraStress]
                 deriving (Show,Generic)

data RecoveryAssumption = Recovery (Rate,Int)
                        deriving (Show,Generic)

data AssetPerfAssumption = MortgageAssump    (Maybe AssetDefaultAssumption) (Maybe AssetPrepayAssumption) (Maybe RecoveryAssumption) (Maybe [InterestAssumption]) (Maybe ExtraStress)
                         | MortgageDeqAssump (Maybe AssetDelinquencyAssumption) (Maybe [InterestAssumption]) (Maybe ExtraStress)
                         | LeaseAssump       LeaseAssetGapAssump LeaseAssetRentAssump EndDate (Maybe [InterestAssumption]) (Maybe ExtraStress)
                         | LoanAssump        (Maybe AssetDefaultAssumption) (Maybe AssetPrepayAssumption) (Maybe RecoveryAssumption) (Maybe [InterestAssumption]) (Maybe ExtraStress)
                         | InstallmentAssump (Maybe AssetDefaultAssumption) (Maybe AssetPrepayAssumption) (Maybe RecoveryAssumption) (Maybe [InterestAssumption]) (Maybe ExtraStress)
                         | DefaultedRecovery Rate Int [Rate]
                         deriving (Show,Generic)

data ReovolvingAssumption = AvailableAssets RevolvingPool AssetPerfAssumption
                          | Dummy4 
                          deriving (Show,Generic)

data DealRunAssumption = StopRunBy Date
                       | ProjectedExpense FeeName Ts
                       | CallWhen [C.CallOption]
                       deriving (Show,Generic)

data QueryInput = InspectOn [(DatePattern,DealStats)]
                | BuildFinancialReport DatePattern
                deriving (Show,Generic)

data BondPricingInput = DiscountCurve Date Ts                               -- ^ PV curve used to discount bond cashflow and a PV date where cashflow discounted to 
                      | RunZSpread Ts (Map.Map BondName (Date,Rational))    -- ^ PV curve as well as bond trading price with a deal used to calc Z - spread
                      deriving (Show,Generic)

-- splitAssumptions :: [AssumptionBuilder] -> ([AssumptionBuilder],[AssumptionBuilder]) -> ([AssumptionBuilder],[AssumptionBuilder])
-- splitAssumptions (a:aps) (dealAssump,assetAssump)
--   = case a of
--     InterestRateConstant _ _ -> splitAssumptions aps (a:dealAssump,assetAssump)
--     InterestRateCurve _ _  -> splitAssumptions aps (a:dealAssump,assetAssump)
--     CallWhen _ -> splitAssumptions aps (a:dealAssump,assetAssump)
--     StopRunBy _ -> splitAssumptions aps (a:dealAssump,assetAssump)
--     InspectOn _ -> splitAssumptions aps (a:dealAssump,assetAssump)
--     PoolHairCut _ _ -> splitAssumptions aps (a:dealAssump,assetAssump)
--     BuildFinancialReport _ -> splitAssumptions aps (a:dealAssump,assetAssump)
--     AvailableAssets _ _ -> splitAssumptions aps (a:dealAssump,assetAssump)
--     ProjectedExpense _ _ -> splitAssumptions aps (a:dealAssump,assetAssump)
--     _  -> splitAssumptions aps (dealAssump,a:assetAssump)

-- splitAssumptions [] r = r

-- getCDR :: AssumptionLists -> Maybe Rate
-- getCDR [] = Nothing
-- getCDR (ap:aps) = 
--     case ap of 
--       DefaultCDR r -> Just r 
--       _ -> getCDR aps
-- 
-- getCPR :: AssumptionLists -> Maybe Rate
-- getCPR [] = Nothing
-- getCPR (ap:aps) = 
--     case ap of 
--       PrepaymentCPR r -> Just r 
--       _ -> getCPR aps

getIndexFromRateAssumption :: RateAssumption -> Index 
getIndexFromRateAssumption (RateCurve idx _) = idx
getIndexFromRateAssumption (RateFlat idx _) = idx

lookupRate :: [RateAssumption] -> Floater -> Date -> IRate 
lookupRate rAssumps (index,spd) d
  = case find (\x -> getIndexFromRateAssumption x == index ) rAssumps of 
      Just (RateCurve _ ts) -> spd + fromRational (getValByDate ts Inc d)
      Just (RateFlat _ r) -> r + spd
      Nothing -> error $ "Failed to find Index "++show index

getRateAssumption :: [InterestAssumption] -> Index -> Maybe InterestAssumption
getRateAssumption assumps idx
  = find (\case
           (InterestRateCurve _idx _) -> idx == _idx 
           (InterestRateConstant _idx _) -> idx == _idx
           _ -> False)
         assumps

-- | project rates used by rate type ,with interest rate assumptions and observation dates
projRates :: RateType -> Maybe [InterestAssumption] -> [Date] -> [IRate]
projRates (Fix r) _ ds = replicate (length ds) r 
projRates (Floater idx spd r dp rfloor rcap mr) (Just assumps) ds 
  = case getRateAssumption assumps idx of
      Nothing -> error ("Failed to find index rate " ++ show idx ++ " from "++ show assumps)
      Just _rateAssumption -> 
        let 
          resetDates = genSerialDatesTill2 NO_IE (head ds) dp (last ds)
          ratesFromCurve = case _rateAssumption of
                             (InterestRateCurve _ ts)   -> (\x -> spd + (fromRational x) ) <$>  (getValByDates ts Inc resetDates)
                             (InterestRateConstant _ v) -> (spd +) <$> replicate (length resetDates) v
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


$(deriveJSON defaultOptions ''InterestAssumption)
$(deriveJSON defaultOptions ''BondPricingInput)
$(deriveJSON defaultOptions ''AssumptionInput)
$(deriveJSON defaultOptions ''ApplyAssumptionType)
$(deriveJSON defaultOptions ''AssetPerfAssumption)
$(deriveJSON defaultOptions ''AssetDefaultAssumption)
$(deriveJSON defaultOptions ''AssetPrepayAssumption)
$(deriveJSON defaultOptions ''RecoveryAssumption)
$(deriveJSON defaultOptions ''ExtraStress)
$(deriveJSON defaultOptions ''AssetDelinquencyAssumption)
$(deriveJSON defaultOptions ''LeaseAssetGapAssump)
$(deriveJSON defaultOptions ''LeaseAssetRentAssump)
