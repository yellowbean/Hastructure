{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module AssetClass.Lease
  (Lease(..),LeaseInfo(..),accrueRentals,LeaseStepUp(..))
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
    ,originRental :: Amount
    }
    deriving (Show)

data LeaseStepUp = FlatRate DatePattern Rate
                 | ByRateCurve DatePattern [Rate]
    deriving (Show)


data Lease = RegularLease LeaseInfo Int 
           | StepUpLease LeaseInfo LeaseStepUp Int
    deriving (Show)

type LastAccuredDate = Date
type DailyRate = Balance
type AccuralPeriod = (Date,DailyRate)

accrueRentals :: [AccuralPeriod] -> [Date] -> LastAccuredDate -> [Amount] -> [Amount] -> [Amount]
accrueRentals _ [] _ _ payAmts = payAmts
accrueRentals ad@((accrueD,dr):accrueDs) pd@(payD:payDs) lastAccrueD accAmts payAmts
  |accrueD < payD = accrueRentals 
                      accrueDs 
                      pd
                      accrueD 
                      (accAmts++ [((fromRational (toRational (daysBetween lastAccrueD accrueD))) * dr)]) 
                      payAmts -- `debug` (">> acc amts->"++show accAmts++">>adding"++ show (mulBR (fromRational (toRational (daysBetween lastAccrueD accrueD))) dr ))
  |accrueD == payD = let 
                      _accAmt = ((fromRational (toRational (daysBetween lastAccrueD accrueD))) * dr)
                     in  
                      accrueRentals 
                      accrueDs 
                      payDs
                      accrueD 
                      [] 
                      (payAmts++[(sum (_accAmt:accAmts))])
  |otherwise = accrueRentals ad payDs lastAccrueD [] (payAmts++[(sum accAmts)])


instance Asset Lease where 
    calcCashflow l@(RegularLease (LeaseInfo sd ot dp pmt) rt) d =
        CF.CashFlowFrame $ zipWith CF.LeaseFlow cf_dates pmts 
      where 
        cf_dates =  filter (> d ) $ genSerialDates dp sd ot
        pmts = replicate (length cf_dates) pmt

    calcCashflow l@(StepUpLease (LeaseInfo sd ot dp dr) lsu rt) d =
        CF.CashFlowFrame $ zipWith CF.LeaseFlow cf_dates pmts
      where 
        p_dates = genSerialDates dp sd ot
        cf_dates =  filter (> d) p_dates
        accrueEndsAt = last cf_dates
        pmts = case lsu of 
                 (FlatRate _dp _r) ->
                   let 
                     a_dates = genSerialDatesTill2 II sd _dp accrueEndsAt
                     lastAccD:accrueDates = sliceDates (SliceAfterKeepPrevious d) a_dates
                     lengthFutureAccD = length accrueDates
                     lengthAccD= length a_dates
                     dailyRates = [ mulBR dr ((toRational (1+_r))^^x) | x <- [(lengthAccD - lengthFutureAccD - 1)..lengthAccD]] -- `debug` (">>LAD"++show lastAccD++">>"++show accrueDates)]
                     accruePeriods = zip accrueDates dailyRates 
                   in  
                     accrueRentals accruePeriods cf_dates lastAccD [] [] -- `debug` ("Acc P>>"++show accruePeriods++">> pay dates"++show cf_dates)
                 (ByRateCurve _dp _rs) -> 
                   let 
                     a_dates = genSerialDatesTill2 II sd _dp accrueEndsAt
                     lastAccD:accrueDates = sliceDates (SliceAfterKeepPrevious d) a_dates
                     factors = scanl (*) 1.0 $ [ (_r + 1)  | _r <- _rs]
                     dailyRates =  drop ((length a_dates) - (length accrueDates) - 1) $ [ mulBR dr f | f <- factors ] -- `debug` (">>LAD"++show lastAccD++">>"++show accrueDates)]
                     accruePeriods = zip accrueDates dailyRates -- `debug` ("LastACD"++ show lastAccD)
                   in 
                     accrueRentals accruePeriods cf_dates lastAccD [] [] -- `debug` ("AP->"++show accruePeriods)


$(deriveJSON defaultOptions ''LeaseInfo)
$(deriveJSON defaultOptions ''LeaseStepUp)
$(deriveJSON defaultOptions ''Lease)
