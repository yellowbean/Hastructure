{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module InterestRate
  (ARM(..),RateType(..),runInterestRate)
  where

import Language.Haskell.TH
import Data.Aeson       hiding (json)
import Data.Aeson.TH
import Data.Maybe
import Data.Fixed
import GHC.Generics

import Types
import Util

import Debug.Trace
debug = flip trace

type InitPeriod = Int 
type PeriodicCap = Maybe Spread
type LifetimeCap = Maybe IRate
type PaymentCap = Maybe Balance
type RateFloor = Maybe IRate
type InitCap = Maybe IRate
type ResetDates = [Date]
type StartRate = IRate

data RateType = Fix IRate
              | Floater Index Spread IRate Period (Maybe Floor)
              | Floater2 Index Spread IRate DatePattern
              deriving (Show,Generic)

data ARM = ARM InitPeriod InitCap PeriodicCap LifetimeCap RateFloor
         | OtherARM
         deriving (Show,Generic)

runInterestRate :: ARM -> StartRate -> RateType -> ResetDates -> Ts -> [IRate]
runInterestRate (ARM ip icap pc lifeCap floor) sr (Floater2 idx spd initRate dp) resetDates rc
  = sr:cappedRates
    where 
      fr:rrs = (spd +) . fromRational <$> getValByDates rc Inc resetDates
      firstRate = min (sr + fromMaybe 0 icap) fr
      restRates = tail $
                    scanl 
                      (\lastRate idxRate -> 
                          min (lastRate + fromMaybe 0 pc) idxRate)
                      firstRate
                      rrs
      flooredRates =  max (fromMaybe 0 floor) <$> (firstRate:restRates)  `debug` ("reset dates" ++ show (firstRate:restRates))
      cappedRates = min (fromMaybe 1 lifeCap) <$> flooredRates

      




$(deriveJSON defaultOptions ''ARM)
$(deriveJSON defaultOptions ''RateType)
