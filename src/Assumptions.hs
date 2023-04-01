{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Assumptions (AssumptionBuilder(..),BondPricingInput(..),toPeriodRateByInterval
                    ,AssumptionInput(..),AssumptionLists(..),getCDR,getCPR,ApplyAssumptionType(..)
                    ,lookupAssumptionByIdx,splitAssumptions)
where

import Call as C
import Lib (Index(..),Ts(..),TsPoint(..),toDate)
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
                         deriving (Show)

data AssumptionInput = Single ApplyAssumptionType
                     | Multiple (Map.Map String ApplyAssumptionType)
                     deriving (Show)

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
                | PrepaymentDistribution Float [Float] -- total default rate, distribution pct
                | PrepaymentByAging [(Int,Float)]
                | EvenRecoveryOnDefault Float Int
                | InterestRateConstant Index IRate
                | InterestRateCurve Index [(Date,IRate)] -- Deprecating
                | InterestRateCurve2 Index Ts
                | CallWhen [C.CallOption]
                | PoolHairCut PoolSource Rate
                | AvailableAssets 
                -- Lease Assumption 
                | LeaseProjectionEnd Date
                | LeaseBaseAnnualRate Rate
                | LeaseBaseCurve Ts
                | LeaseGapDays Int
                | LeaseGapDaysByAmount [(Amount,Int)] Int
                -- Debug 
                | StopRunBy Date
                | InspectOn [(DatePattern,DealStats)]
                deriving (Show)

data BondPricingInput = DiscountCurve Date Ts
                      | RunZSpread Ts (Map.Map BondName (Date,Balance))
                      deriving (Show)

toPeriodRateByInterval :: Rate -> Int -> Rate
toPeriodRateByInterval annualRate days
  = toRational $ 1 - fromRational (1-annualRate) ** (fromIntegral days / 365) -- `debug` ("days>>"++show days++"DIV"++ show ((fromIntegral days) / 365))

splitAssumptions :: [AssumptionBuilder] -> ([AssumptionBuilder],[AssumptionBuilder]) -> ([AssumptionBuilder],[AssumptionBuilder])
splitAssumptions (a:aps) (dealAssump,assetAssump)
 = case a of
     InterestRateConstant _ _ -> splitAssumptions aps (a:dealAssump,assetAssump)
     InterestRateCurve _ _  -> splitAssumptions aps (a:dealAssump,assetAssump)
     InterestRateCurve2 _ _ -> splitAssumptions aps (a:dealAssump,assetAssump)
     CallWhen _ -> splitAssumptions aps (a:dealAssump,assetAssump)
     StopRunBy _ -> splitAssumptions aps (a:dealAssump,assetAssump)
     InspectOn _ -> splitAssumptions aps (a:dealAssump,assetAssump)
     PoolHairCut _ _ -> splitAssumptions aps (a:dealAssump,assetAssump)
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

$(deriveJSON defaultOptions ''AssumptionBuilder)
$(deriveJSON defaultOptions ''BondPricingInput)
$(deriveJSON defaultOptions ''AssumptionInput)
$(deriveJSON defaultOptions ''ApplyAssumptionType)
