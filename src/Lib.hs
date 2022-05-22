{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib
    (Rate,Dates,Period(..),calcInt,calcIntRate,Balance,DayCount(..)
    ,genDates,StartDate,EndDate,LastIntPayDate
    ,Spread,Index
    ,paySeqLiabilities,prorataFactors,periodToYear
    ) where

import qualified Data.Time as T
import qualified Data.Set as S

import Language.Haskell.TH
import Data.Aeson.TH
import Data.Aeson.Types

type Rate = Float
type Spread = Float
type Balance = Float
type Dates = [T.Day]
type StartDate = T.Day
type EndDate = T.Day
type LastIntPayDate = T.Day

data Period = Daily 
              | Weekly 
              | Monthly 
              | Quarter 
              | SemiAnnually 
              | Annually
              deriving (Show)

$(deriveJSON defaultOptions ''Period)

data Index = LPR5Y
            | LIBOR1M
            | LIBOR1Y
            deriving (Show)
-- data Interval = CalendarDiffDays 1 0 |CalendarDiffDays 3 0 | CalendarDiffDays 6 0 |CalendarDiffDays 12 0

data DayCount = ACT_360
               | ACT_365
               deriving (Show)

periodToYear :: T.Day -> T.Day -> DayCount -> Float
periodToYear start_date end_date day_count =
  case day_count of
    ACT_360 -> days / 360
    ACT_365 -> days / 365
  where
    days = fromIntegral (T.diffDays end_date start_date)

calcIntRate :: T.Day -> T.Day -> Rate -> DayCount -> Float
calcIntRate start_date end_date int_rate day_count =
   int_rate * (periodToYear start_date end_date day_count)

calcInt :: Balance -> T.Day -> T.Day -> Rate -> DayCount -> Float
calcInt bal start_date end_date int_rate day_count =
  bal * (calcIntRate start_date end_date int_rate day_count)

addD :: T.Day -> T.CalendarDiffDays -> T.Day
addD d calendarMonth = T.addGregorianDurationClip T.calendarMonth d

genDates :: T.Day -> Period -> Int -> [T.Day]
genDates start_day p n =
   [ T.addGregorianDurationClip (T.CalendarDiffDays (toInteger i*mul) 0) start_day | i <- [1..n]]
   where
     mul = case p of
       Monthly -> 1
       Quarter -> 3
       SemiAnnually -> 6
       Annually -> 12
       _ -> 0


prorataFactors :: [Float] -> Float -> [Float]
prorataFactors bals amt =
  map (\y -> y * amtToPay) factors
  where
    s = foldl (+) 0 bals
    amtToPay = min s amt
    factors = map (\x -> x / s) bals

paySeqLiabilities :: Float -> [Float] -> [(Float,Float)]
paySeqLiabilities startAmt liabilities =
  tail $ reverse $ foldl pay [(startAmt, 0)] liabilities
  where pay accum@((amt, _):xs) target = if amt >= target then
                            (amt-target, 0):accum
                         else
                            (0, target-amt):accum
