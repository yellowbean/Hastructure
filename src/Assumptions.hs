{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Assumptions (AssumptionBuilder(..),BondPricingInput(..),toPeriodRateByInterval
                    ,AssumptionInput(..),AssumptionLists(..),getCDR,getCPR,ApplyAssumptionType(..)
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

type AssumptionLists = [AssumptionBuilder]
type StratificationByIdx = ([Int],AssumptionLists)

lookupAssumptionByIdx :: [StratificationByIdx] -> Int -> AssumptionLists
lookupAssumptionByIdx sbi i
  = case find (\(indxs,_) -> Set.member i  (Set.fromList indxs) ) sbi of
        Just (_, aps ) ->  aps
        Nothing -> []

data ApplyAssumptionType = PoolLevel AssumptionLists                       -- ^ assumption apply to all assets in the pool
                         | ByIndex [StratificationByIdx] AssumptionLists   -- ^ assumption which only apply to a set of assets in the pool
                         deriving (Show,Generic)

data AssumptionInput = Single ApplyAssumptionType                          -- ^ one assumption request
                     | Multiple (Map.Map String ApplyAssumptionType)       -- ^ multiple assumption request in a single request
                     deriving (Show,Generic)

data AssumptionBuilder = MortgageByAge ([Int],[Float])
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
                       -- Expense Assumption
                       | ProjectedExpense FeeName Ts
                       -- Debug 
                       | StopRunBy Date
                       | InspectOn [(DatePattern,DealStats)]
                       -- Financial Report
                       | BuildFinancialReport DatePattern
                       deriving (Show,Generic)

data BondPricingInput = DiscountCurve Date Ts                               -- ^ PV curve used to discount bond cashflow and a PV date where cashflow discounted to 
                      | RunZSpread Ts (Map.Map BondName (Date,Rational))    -- ^ PV curve as well as bond trading price with a deal used to calc Z - spread
                      deriving (Show,Generic)

data PoolAssumptionType = PpyDefault



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
     ProjectedExpense _ _ -> splitAssumptions aps (a:dealAssump,assetAssump)
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
  = case find (\x -> getIndexFromRateAssumption x == index ) rAssumps of 
      Just (RateCurve _ ts) -> spd + fromRational (getValByDate ts Inc d)
      Just (RateFlat _ r) -> r + spd
      Nothing -> error $ "Failed to find Index "++show index

getRateAssumption :: [AssumptionBuilder] -> Index -> Maybe AssumptionBuilder
getRateAssumption assumps idx
  = find (\case
           (InterestRateCurve _idx _) -> idx == _idx 
           (InterestRateConstant _idx _) -> idx == _idx
           _ -> False)
         assumps

-- | project rates used by rate type ,with interest rate assumptions and observation dates
projRates :: RateType -> [AssumptionBuilder] -> [Date] -> [IRate]
projRates (Fix r) _ ds = replicate (length ds) r 
projRates (Floater idx spd r dp rfloor rcap mr) assumps ds 
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


$(deriveJSON defaultOptions ''AssumptionBuilder)
$(deriveJSON defaultOptions ''BondPricingInput)
$(deriveJSON defaultOptions ''AssumptionInput)
$(deriveJSON defaultOptions ''ApplyAssumptionType)
