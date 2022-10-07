{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module InterestRate
  ()
  where

import Language.Haskell.TH
import Data.Aeson       hiding (json)
import Data.Aeson.TH
import Data.Fixed

import Types
import Util


import Debug.Trace
debug = flip trace
--
--data RateReset = ByInterval Period (Maybe Date) -- period, maybe a start day
--               | MonthOfYear  Int  -- month index, 0 => Janaury
--               deriving (Show)
--
--data InterestInfo = 
--          Floater Index Spread RateReset (Maybe Floor) (Maybe Cap)
--          | Fix IRate
--          | InterestByYield IRate
--          deriving (Show)