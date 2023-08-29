{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Assumptions (BondPricingInput(..),toPeriodRateByInterval
                    ,AssumptionInput(..),AssumptionLists(..),ApplyAssumptionType(..)
                    ,lookupAssumptionByIdx,splitAssumptions,lookupRate
                    ,getRateAssumption,projRates)
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

data PrepaymentType = PrepaymentConstant Rate
                    | PrepaymentCurve    Ts
                    | PrepaymentVec      [Rate]
                    | PrepaymentCPR      Rate
                    | PrepaymentFactors  Ts
                    deriving (Show,Generic)

data DefaultType = DefaultConstant Rate
                 | DefaultCurve Ts
                 | DefaultCDR Rate
                 | DefaultVec [Rate]
                 | DefaultFactors Ts
                 deriving (Show,Generic)

data RecoveryType = Recovery (Rate,Int)
                  | RecoveryDistribution (Rate,[Rate])
                  | EvenRecoveryOnDefault Float Int
                  deriving (Show,Generic)

data InterestAssump = InterestRateConstant Index IRate
                    | InterestRateCurve Index Ts
                    deriving (Show,Generic)

data RevolvingAssump = AvailableAssets RevolvingPool [AssumptionByAssetType]
                     | Dummy2
                     deriving (Show,Generic)

data LeaseAssump = LeaseProjectionEnd Date
                 | LeaseBaseAnnualRate Rate
                 | LeaseBaseCurve Ts
                 | LeaseGapDays Int
                 | LeaseGapDaysByAmount [(Amount,Int)] Int
                 deriving (Show,Generic)

data RecoveryOnDefaultsAssump = DefaultedRecovery Rate Int [Rate]
                               deriving (Show,Generic)

data OutputRequired = BuildFinancialReport DatePattern
                    | InspectOn [(DatePattern,DealStats)]
                    deriving (Show,Generic)

data AssumptionByAssetType = MortgageAssumption  (Maybe PrepaymentType) (Maybe DefaultType) (Maybe RecoveryType) (Maybe RecoveryOnDefaultsAssump)
                           | LoanAssumption      (Maybe PrepaymentType) (Maybe DefaultType) (Maybe RecoveryType) (Maybe RecoveryOnDefaultsAssump)
                           | LeaseAssumption      EndDate
                           | RevolvingAssumption  
                           deriving (Show,Generic)

data DealRunAssumption = PoolHairCut PoolSource Rate
                       | CallWhen [C.CallOption]
                       | ProjectedExpense FeeName Ts
                       | StopRunBy Date
                       deriving (Show,Generic)

type StratificationByIdx = ([Int],AssumptionByAssetType)

lookupAssumptionByIdx :: [StratificationByIdx] -> Int -> Maybe AssumptionByAssetType
lookupAssumptionByIdx sbi i
  = case find (\(indxs,_) -> Set.member i  (Set.fromList indxs) ) sbi of
        Just (_, aps ) ->  Just aps
        Nothing -> Nothing

type NonePoolAssumps = ([DealRunAssumption],[OutputRequired],[InterestAssump])

data ApplyAssumptionType = PoolLevel AssumptionByAssetType NonePoolAssumps        -- ^ assumption apply to all assets in the pool
                         | ByIndex [StratificationByIdx] NonePoolAssumps          -- ^ assumption which only apply to a set of assets in the pool
                         deriving (Show,Generic)

data AssumptionInput = Single ApplyAssumptionType                                 -- ^ one assumption request
                     | Multiple (Map.Map ScenarioName ApplyAssumptionType)            -- ^ multiple assumption request in a single request
                     deriving (Show,Generic)


data BondPricingInput = DiscountCurve Date Ts                               -- ^ PV curve used to discount bond cashflow and a PV date where cashflow discounted to 
                      | RunZSpread Ts (Map.Map BondName (Date,Rational))    -- ^ PV curve as well as bond trading price with a deal used to calc Z - spread
                      deriving (Show,Generic)


toPeriodRateByInterval :: Rate -> Int -> Rate
toPeriodRateByInterval annualRate days
  = toRational $ 1 - fromRational (1-annualRate) ** (fromIntegral days / 365) -- `debug` ("days>>"++show days++"DIV"++ show ((fromIntegral days) / 365))


getIndexFromRateAssumption :: RateAssumption -> Index 
getIndexFromRateAssumption (RateCurve idx _) = idx
getIndexFromRateAssumption (RateFlat idx _) = idx

lookupRate :: [RateAssumption] -> Floater -> Date -> IRate 
lookupRate rAssumps (index,spd) d
  = case find (\x -> getIndexFromRateAssumption x == index ) rAssumps of 
      Just (RateCurve _ ts) -> spd + fromRational (getValByDate ts Inc d)
      Just (RateFlat _ r) -> r + spd
      Nothing -> error $ "Failed to find Index "++show index

-- | project rates used by rate type ,with interest rate assumptions and observation dates
-- projRates :: RateType -> [AssumptionBuilder] -> [Date] -> [IRate]
-- projRates (Fix r) _ ds = replicate (length ds) r 
-- projRates (Floater idx spd r dp rfloor rcap mr) assumps ds 
--   = case getRateAssumption assumps idx of
--       Nothing -> error ("Failed to find index rate " ++ show idx ++ " from "++ show assumps)
--       Just _rateAssumption -> 
--         let 
--           resetDates = genSerialDatesTill2 NO_IE (head ds) dp (last ds)
--           ratesFromCurve = case _rateAssumption of
--                              (InterestRateCurve _ ts)   -> (\x -> spd + (fromRational x) ) <$>  (getValByDates ts Inc resetDates)
--                              (InterestRateConstant _ v) -> (spd +) <$> replicate (length resetDates) v
--                              _ -> error ("Invalid rate type "++ show _rateAssumption)
--           ratesUsedByDates =  getValByDates
--                                 (mkRateTs $ zip resetDates ratesFromCurve)
--                                 Inc
--                                 ds
--         in 
--           case (rfloor,rcap) of 
--             (Nothing, Nothing) -> fromRational <$> ratesUsedByDates 
--             (Just fv, Just cv) -> capWith cv $ floorWith fv $ fromRational <$> ratesUsedByDates 
--             (Just fv, Nothing) -> floorWith fv $ fromRational <$> ratesUsedByDates 
--             (Nothing, Just cv) -> capWith cv $ fromRational <$> ratesUsedByDates 


-- $(deriveJSON defaultOptions ''AssumptionBuilder)
$(deriveJSON defaultOptions ''BondPricingInput)
$(deriveJSON defaultOptions ''AssumptionInput)
$(deriveJSON defaultOptions ''ApplyAssumptionType)
$(deriveJSON defaultOptions ''PrepaymentType)
$(deriveJSON defaultOptions ''DefaultType)
$(deriveJSON defaultOptions ''RecoveryType)
$(deriveJSON defaultOptions ''InterestAssump)
$(deriveJSON defaultOptions ''RevolvingAssump)
$(deriveJSON defaultOptions ''LeaseAssump)