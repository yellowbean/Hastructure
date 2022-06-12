{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Assumptions (AssumptionBuilder(..))

where

import Lib (Rate,Index)
import qualified Data.Map as Map
import Data.List
import Data.Aeson hiding (json)
import Language.Haskell.TH
import Data.Aeson.TH
import Data.Aeson.Types



data AssumptionBuilder =  MortgageByAge ([Int],[Float])
                | MortgageByRate ([Float],[Float])
                | PrepaymentConstant Float
                | DefaultConstant Float
                | Recovery (Rate,Int)
                | LinearTo Int Float
                | InterestRateConstant (Index,Float)
                deriving (Show)



$(deriveJSON defaultOptions ''AssumptionBuilder)
