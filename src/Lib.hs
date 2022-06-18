{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lib
    (Rate,Dates,Period(..),calcInt,calcIntRate,Balance,DayCount(..)
    ,genDates,StartDate,EndDate,LastIntPayDate
    ,Spread,Index(..)
    ,paySeqLiabilities,prorataFactors,periodToYear
    ,afterNPeriod,DealStats(..),Ts(..)
    ,Txn(..),combineTxn,Statement(..)
    ,appendStmt,periodRateFromAnnualRate
    ,Floor,Cap,TsPoint(..),RateAssumption(..)
    ,getValByDate
    ) where

import qualified Data.Time as T
import Data.List
import Language.Haskell.TH
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Aeson hiding (json)

type Rate = Float
type Spread = Float
type Balance = Float
type Amount = Float
type Comment = String
type Dates = [T.Day]
type StartDate = T.Day
type EndDate = T.Day
type LastIntPayDate = T.Day
type Floor = Float
type Cap = Float

data Period = Daily 
              | Weekly 
              | Monthly 
              | Quarterly 
              | SemiAnnually 
              | Annually
              deriving (Show)

data DealStats = PoolInt
              | CurrentBondBalance
              | CurrentPoolBalance
              | OriginalBondBalance
              | OriginalPoolBalance
              deriving (Show)

$(deriveJSON defaultOptions ''DealStats)
$(deriveJSON defaultOptions ''Period)

data Index = LPR5Y
            | LIBOR1M
            | LIBOR3M
            | LIBOR6M
            | LIBOR1Y
            | SOFR1M
            | SOFR3M
            | SOFR6M
            | SOFR1Y
            deriving (Show,Eq)
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

annualRateToPeriodRate :: Period -> Float -> Float
annualRateToPeriodRate p annualRate =
    1 - (1 - annualRate ) ** n
  where 
    n = case p of 
      Monthly -> 1/12
      Quarterly -> 1/4 
      SemiAnnually -> 1/2
      Annually -> 1.0

periodRateFromAnnualRate :: Period -> Float -> Float
periodRateFromAnnualRate Annually annual_rate  = annual_rate
periodRateFromAnnualRate Monthly annual_rate  = annual_rate / 12
periodRateFromAnnualRate Quarterly annual_rate  = annual_rate / 4
periodRateFromAnnualRate SemiAnnually annual_rate  = annual_rate / 2


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
       Quarterly -> 3
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
  where pay accum@((amt, _):xs) target = 
                         if amt >= target then
                            (amt-target, 0):accum
                         else
                            (0, target-amt):accum

afterNPeriod :: T.Day -> Integer -> Period -> T.Day
afterNPeriod d i p =
  T.addGregorianMonthsClip ( months * i)  d
  where
    months = case p of
      Monthly -> 1
      Quarterly -> 3
      SemiAnnually -> 6
      Annually -> 12

data Txn = BondTxn T.Day Balance Float Float Float Comment
          | AccTxn T.Day Balance Amount Comment
          | ExpTxn T.Day Balance Amount Balance String
        deriving (Show)

appendStmt :: Maybe Statement -> Txn -> Statement
appendStmt (Just stmt@(Statement txns)) txn = Statement (txns++[txn])
appendStmt Nothing txn = Statement [txn]

combineTxn :: Txn -> Txn -> Txn
combineTxn (BondTxn d1 b1 i1 p1 r1 m1) (BondTxn d2 b2 i2 p2 r2 m2)
    = BondTxn d1 (min b1 b2) (i1 + i2) (p1 + p2) (r1+r2) ""

jointTxn :: [Txn] -> [Txn] -> [Txn]
jointTxn txnsA txnsB
  = txnsA


data Statement = Statement [Txn]
        deriving (Show)

--joinStatement :: (String,Statement) -> (String,Statement) -> [(String,[Txn])]
--jointStatement (stmtsA,(Statement txnsA)) (stmtsB,(Statement txnsB)) =

instance Ord Txn where
  compare (BondTxn d1 _ _ _ _ _ ) (BondTxn d2 _ _ _ _ _ )
    = compare d1 d2

instance Eq Txn where 
  (BondTxn d1 _ _ _ _ _) == (BondTxn d2 _ _ _ _ _)
    = d1 == d2


data TsPoint a = TsPoint T.Day a
                deriving (Show)

data Ts = FloatCurve [(TsPoint Float)]
         |BoolCurve [(TsPoint Bool)]
         |AmountCurve [(TsPoint Float)]
         deriving (Show)

data RateAssumption = RateCurve Index Ts
                    | RateFlat Index Float
                      


getValByDate :: Ts -> T.Day -> Float
getValByDate (FloatCurve dps) d 
  = case find (\(TsPoint _d _) -> ( d > _d )) (reverse dps)  of 
      Just (TsPoint _d v) -> v
      Nothing -> 0

getValByDates :: Ts -> [T.Day] -> [Float]
getValByDates rc ds = map (getValByDate rc) ds


rc = FloatCurve [(TsPoint (T.fromGregorian 2022 1 1) 0.01)
               ,(TsPoint (T.fromGregorian 2022 2 1) 0.02)
               ,(TsPoint (T.fromGregorian 2022 3 1) 0.03)
               ]

$(deriveJSON defaultOptions ''Txn)
$(deriveJSON defaultOptions ''Ts)
$(deriveJSON defaultOptions ''TsPoint)
$(deriveJSON defaultOptions ''Index)
$(deriveJSON defaultOptions ''Statement)
