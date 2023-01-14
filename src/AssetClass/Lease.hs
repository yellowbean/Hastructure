{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module AssetClass.Lease
  (Lease(..),LeaseInfo(..))
  where

import qualified Data.Time as T
import qualified Cashflow as CF -- (Cashflow,Amount,Interests,Principals)
import qualified Assumptions as A
import Asset
import Types
import Lib
import Util

import qualified Data.Map as Map
import Data.List
import Data.Aeson hiding (json)
import Language.Haskell.TH
import Data.Aeson.TH
import Data.Aeson.Types

import Debug.Trace
debug = flip trace

type PeriodAmount = Balance

data LeaseInfo = LeaseInfo {
    startDate :: Date
    ,originTerm :: Int 
    ,paymentDates :: DatePattern
    }
    deriving (Show)

data LeaseStepUp = LeaseStepUp Balance

data Lease = RegularLease LeaseInfo PeriodAmount Int 
           -- | StepUpLease LeaseInfo 
    deriving (Show)

instance Asset Lease where 
    calcCashflow l@(RegularLease (LeaseInfo sd ot dp) pmt rt) d =
        CF.CashFlowFrame $ zipWith CF.LeaseFlow cf_dates pmts 
      where 
        -- cf_dates =  drop (ot - rt) $ genSerialDates dp sd ot
        cf_dates =  filter (> d ) $ genSerialDates dp sd ot
        pmts = replicate rt pmt

