{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Assumptions (AssumptionBuilder(..),BondPricingInput(..),toPeriodRateByInterval
                    ,AssumptionInput(..),AssumptionLists(..),getCDR,getCPR,ApplyAssumptionType(..)
                    ,lookupAssumptionByIdx,splitAssumptions,lookupRate
                    ,getRateAssumption)
where

import Call as C
import Lib (Ts(..),TsPoint(..),toDate)
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
debug = flip trace

type AssumptionLists = [AssumptionBuilder]
type StratificationByIdx = ([Int],AssumptionLists)

lookupAssumptionByIdx :: [StratificationByIdx] -> Int -> AssumptionLists
lookupAssumptionByIdx sbi i
  = case find (\(indxs,_) -> Set.member i  (Set.fromList indxs) ) sbi of
        Just (_, aps ) ->  aps
        Nothing -> []

data ApplyAssumptionType = PoolLevel AssumptionLists 
                         | ByIndex [StratificationByIdx] AssumptionLists
                         deriving (Show,Generic)

data AssumptionInput = Single ApplyAssumptionType
                     | Multiple (Map.Map String ApplyAssumptionType)
                     deriving (Show,Generic)

data AssumptionBuilder = MortgageByAge ([Int],[Float])
                -- | MortgageByRate ([Float],[Float])
                | PrepaymentConstant Rate
                | PrepaymentCurve Ts
                | PrepaymentVec [Rate]
                | PrepaymentCPR Rate
                | PrepaymentFactors Ts
                | DefaultConstant Rate
                | DefaultCurve Ts
                | DefaultCDR Rate
                | DefaultVec [Rate]
                | DefaultFactors Ts
                | Recovery (Rate,Int)
                | RecoveryDistribution (Rate,[Rate])
                | PrepaymentDistribution Float [Float] -- total default rate, distribution pct
                | PrepaymentByAging [(Int,Float)]
                | EvenRecoveryOnDefault Float Int
                | InterestRateConstant Index IRate
                | InterestRateCurve Index Ts
                | CallWhen [C.CallOption]
                | PoolHairCut PoolSource Rate
                -- Revolving
                | AvailableAssets RevolvingPool [AssumptionBuilder]
                -- | AvailableMortgage (RevolvingPool2 Mortgage) [AssumptionBuilder]
                -- | AvailableLease (RevolvingPool2 Lease) [AssumptionBuilder]
                -- | AvailableLoan (RevolvingPool2 Loan) [AssumptionBuilder]
                -- | AvailableInstallment (RevolvingPool2 Installment) [AssumptionBuilder]
                -- Lease Assumption 
                | LeaseProjectionEnd Date
                | LeaseBaseAnnualRate Rate
                | LeaseBaseCurve Ts
                | LeaseGapDays Int
                | LeaseGapDaysByAmount [(Amount,Int)] Int
                -- For Defaulted Asset
                | DefaultedRecovery Rate Int [Rate]
                -- Debug 
                | StopRunBy Date
                | InspectOn [(DatePattern,DealStats)]
                -- Financial Report
                | BuildFinancialReport DatePattern
                deriving (Show,Generic)

data BondPricingInput = DiscountCurve Date Ts
                      | RunZSpread Ts (Map.Map BondName (Date,Balance))
                      deriving (Show,Generic)

toPeriodRateByInterval :: Rate -> Int -> Rate
toPeriodRateByInterval annualRate days
  = toRational $ 1 - fromRational (1-annualRate) ** (fromIntegral days / 365) -- `debug` ("days>>"++show days++"DIV"++ show ((fromIntegral days) / 365))

splitAssumptions :: [AssumptionBuilder] -> ([AssumptionBuilder],[AssumptionBuilder]) -> ([AssumptionBuilder],[AssumptionBuilder])
splitAssumptions (a:aps) (dealAssump,assetAssump)
 = case a of
     InterestRateConstant _ _ -> splitAssumptions aps (a:dealAssump,assetAssump)
     InterestRateCurve _ _  -> splitAssumptions aps (a:dealAssump,assetAssump)
     CallWhen _ -> splitAssumptions aps (a:dealAssump,assetAssump)
     StopRunBy _ -> splitAssumptions aps (a:dealAssump,assetAssump)
     InspectOn _ -> splitAssumptions aps (a:dealAssump,assetAssump)
     PoolHairCut _ _ -> splitAssumptions aps (a:dealAssump,assetAssump)
     BuildFinancialReport _ -> splitAssumptions aps (a:dealAssump,assetAssump)
     AvailableAssets _ _ -> splitAssumptions aps (a:dealAssump,assetAssump)
    -- AvailableMortgage _ _ -> splitAssumptions aps (a:dealAssump,assetAssump)
    -- AvailableLoan _ _ -> splitAssumptions aps (a:dealAssump,assetAssump)
    -- AvailableInstallment _ _ -> splitAssumptions aps (a:dealAssump,assetAssump)
    -- AvailableLease _ _ -> splitAssumptions aps (a:dealAssump,assetAssump)
     _  -> splitAssumptions aps (dealAssump,a:assetAssump)

splitAssumptions [] r = r

getCDR :: AssumptionLists -> Maybe Rate
getCDR [] = Nothing
getCDR (ap:aps) = 
    case ap of 
      DefaultCDR r -> Just r 
      _ -> getCDR aps

getCPR :: AssumptionLists -> Maybe Rate
getCPR [] = Nothing
getCPR (ap:aps) = 
    case ap of 
      PrepaymentCPR r -> Just r 
      _ -> getCPR aps

getIndexFromRateAssumption :: RateAssumption -> Index 
getIndexFromRateAssumption (RateCurve idx _) = idx
getIndexFromRateAssumption (RateFlat idx _) = idx

lookupRate :: [RateAssumption] -> Floater -> Date -> IRate 
lookupRate rAssumps (index,spd) d
  = case find (\x -> (getIndexFromRateAssumption x) == index ) rAssumps of 
      Just (RateCurve _ ts) -> spd + (fromRational (getValByDate ts Inc d))
      Just (RateFlat _ r) -> r + spd
      Nothing -> error $ "Failed to find Index "++show index

getRateAssumption :: [AssumptionBuilder] -> Index -> Maybe AssumptionBuilder
getRateAssumption assumps idx
  = find (\x ->
            case x of
              InterestRateCurve _idx _ -> idx == _idx 
              InterestRateConstant _idx _ -> idx == _idx
              _ -> False) assumps



$(deriveJSON defaultOptions ''AssumptionBuilder)
$(deriveJSON defaultOptions ''BondPricingInput)
$(deriveJSON defaultOptions ''AssumptionInput)
$(deriveJSON defaultOptions ''ApplyAssumptionType)
