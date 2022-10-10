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
                     | Multiple [AssumptionLists]
                     deriving (Show)

data AssumptionBuilder = MortgageByAge ([Int],[Float])
                -- | MortgageByRate ([Float],[Float])
                | PrepaymentConstant Rate
                | PrepaymentCPR Rate
                -- | PrepaymentCPRCurve [Rate]     -- this will ignore the payment interval
                | PrepaymentDistribution Float [Float] -- total default rate, distribution pct
                | DefaultConstant Rate
                | DefaultCDR Rate
                -- | DefaultDistribution Float [Float] -- total default rate, distribution pct
                | Recovery (Rate,Int)
                -- | LinearTo Int Float
                | PrepaymentFactors Ts
                | InterestRateConstant Index IRate
                | InterestRateCurve Index [(T.Day,IRate)]
                | PrepaymentByAging [(Int,Float)]
                | CallWhen [C.CallOption]
                | StopRunBy T.Day
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
