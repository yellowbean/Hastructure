{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Assumptions (AssumptionBuilder(..))

where

import Call as C

import Lib (Rate,Index)
import qualified Data.Map as Map 
import Data.List
import Data.Aeson hiding (json)
import Language.Haskell.TH
import Data.Aeson.TH
import Data.Aeson.Types
import qualified Data.Time as T

data CallWhen = Earliest
              | When C.CallOption 
              | Any [C.CallOption]

data AssumptionBuilder =  MortgageByAge ([Int],[Float])
                | MortgageByRate ([Float],[Float])
                | PrepaymentConstant Float
                | DefaultConstant Float
                | Recovery (Rate,Int)
                | LinearTo Int Float
                | InterestRateConstant Index Float
                | InterestRateCurve Index [(T.Day,Float)]
                | PrepaymentByAging [(Int,Float)]
                | CallWhen
                | StopRunBy T.Day
                deriving (Show)



$(deriveJSON defaultOptions ''AssumptionBuilder)
