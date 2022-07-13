{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Assumptions (AssumptionBuilder(..),BondPricingInput(..))

 where

import Call as C
import Lib (Rate,Index,Ts(..),TsPoint(..))
import qualified Data.Map as Map 
import Data.List
import Data.Aeson hiding (json)
import Language.Haskell.TH
import Data.Aeson.TH
import Data.Aeson.Types
import qualified Data.Time as T


data AssumptionBuilder =  MortgageByAge ([Int],[Float])
                | MortgageByRate ([Float],[Float])
                | PrepaymentConstant Float
                | DefaultConstant Float
                | Recovery (Rate,Int)
                | LinearTo Int Float
                | InterestRateConstant Index Float
                | InterestRateCurve Index [(T.Day,Float)]
                | PrepaymentByAging [(Int,Float)]
                | CallWhen [C.CallOption]
                | StopRunBy T.Day
                deriving (Show)

data BondPricingInput = DiscountCurve T.Day Ts
                deriving (Show)


$(deriveJSON defaultOptions ''AssumptionBuilder)
$(deriveJSON defaultOptions ''BondPricingInput)
