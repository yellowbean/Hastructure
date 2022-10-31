{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Assumptions (AssumptionBuilder(..),BondPricingInput(..),toPeriodRateByInterval
                    ,AssumptionInput(..))

 where

import Call as C
import Lib (IRate,Rate,Index(..),Ts(..),TsPoint(..),toDate)
import qualified Data.Map as Map 
import Data.List
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

data AssumptionInput = Single AssumptionLists
                     | Multiple (Map.Map String AssumptionLists)
                     deriving (Show)

data AssumptionBuilder = MortgageByAge ([Int],[Float])
                -- | MortgageByRate ([Float],[Float])
                | PrepaymentConstant Rate
                | PrepaymentCPR Rate
                | PrepaymentFactors Ts
                | DefaultConstant Rate
                | DefaultCDR Rate
                | DefaultFactors Ts
                | Recovery (Rate,Int)
                | InterestRateConstant Index IRate
                | InterestRateCurve Index [(T.Day,IRate)] -- Deprecating
                | InterestRateCurve2 Index Ts
                | CallWhen [C.CallOption]
                | StopRunBy T.Day
                -- To be implement
                | PoolHairCut PoolSource Rate
                | PrepaymentDistribution Float [Float] -- total default rate, distribution pct
                -- | DefaultDistribution Float [Float] -- total default rate, distribution pct
                -- | LinearTo Int Float
                | PrepaymentByAging [(Int,Float)]
                | EvenRecoveryOnDefault Float Int
                deriving (Show)

-- getAssumption :: [AssumptionBuilder] -> AssumptionBuilder

data BondPricingInput = DiscountCurve T.Day Ts
                deriving (Show)

toPeriodRateByInterval :: Rate -> Int -> Rate
toPeriodRateByInterval annualRate days
  = toRational $ 1 - (fromRational (1-annualRate)) ** ((fromIntegral days) / 365) -- `debug` ("days>>"++show days++"DIV"++ show ((fromIntegral days) / 365))

$(deriveJSON defaultOptions ''AssumptionBuilder)
$(deriveJSON defaultOptions ''BondPricingInput)
$(deriveJSON defaultOptions ''AssumptionInput)
