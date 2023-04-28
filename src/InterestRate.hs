{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module InterestRate
  ()
  where

import Language.Haskell.TH
import Data.Aeson       hiding (json)
import Data.Aeson.TH
import Data.Fixed
import GHC.Generics

import Types
import Util

import Debug.Trace
debug = flip trace

-- type RateReset = DatePattern
-- data InterestInfo = Floater Index Spread RateReset (Maybe Floor) (Maybe Cap)
--                   | Fix IRate
--                   deriving (Show, Generic)
-- 
-- data InterestRate = InterestRate {
--   rateType :: InterestInfo
--   ,currentRate :: IRate 
-- } deriving (Show, Generic)