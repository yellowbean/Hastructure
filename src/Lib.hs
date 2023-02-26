{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lib
    (Amount,Rate,Dates,Period(..),Balance
    ,genDates,StartDate,EndDate,daysBetween,daysBetweenI
    ,Spread,Index(..),Date
    ,paySeqLiabilities,prorataFactors
    ,afterNPeriod,Ts(..),periodsBetween
    ,periodRateFromAnnualRate
    ,previousDate,inSamePeriod
    ,Floor,Cap,TsPoint(..),RateAssumption(..)
    ,toDate
    ,getValOnByDate,sumValTs,subTsBetweenDates,splitTsByDate
    ,paySeqLiabilitiesAmt,getIntervalDays,getIntervalFactors,nextDate
    ,zipWith8,zipWith9,zipWith10, monthsOfPeriod
    ,weightedBy, mkTs, DealStatus(..),isTsEmpty
    ,mkRateTs,Pre(..)
    ) where

import qualified Data.Time as T
import qualified Data.Time.Format as TF
import Data.List
import Data.Fixed
-- import qualified Data.Scientific as SCI
import qualified Data.Map as M
import Language.Haskell.TH
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Aeson hiding (json)
import Text.Regex.TDFA
import Data.Fixed

import Types
import Debug.Trace
debug = flip trace

data DealFlags = Flags Bool -- dummy , this data intends to provide boolean flags regards to a deal

data Index = LPR5Y
            | LPR1Y
            | LIBOR1M
            | LIBOR3M
            | LIBOR6M
            | LIBOR1Y
            | PRIME
            | SOFR1M
            | SOFR3M
            | SOFR6M
            | SOFR1Y
            deriving (Show,Eq)
-- data Interval = CalendarDiffDays 1 0 |CalendarDiffDays 3 0 | CalendarDiffDays 6 0 |CalendarDiffDays 12 0

annualRateToPeriodRate :: Period -> Float -> Float
annualRateToPeriodRate p annualRate =
    1 - (1 - annualRate ) ** n
  where 
    n = case p of 
      Monthly -> 1/12
      Quarterly -> 1/4 
      SemiAnnually -> 1/2
      Annually -> 1.0

periodRateFromAnnualRate :: Period -> IRate -> IRate
periodRateFromAnnualRate Annually annual_rate  = annual_rate
periodRateFromAnnualRate Monthly annual_rate  = annual_rate / 12
periodRateFromAnnualRate Quarterly annual_rate  = annual_rate / 4
periodRateFromAnnualRate SemiAnnually annual_rate  = annual_rate / 2

addD :: Date -> T.CalendarDiffDays -> Date
addD d calendarMonth = T.addGregorianDurationClip T.calendarMonth d

genDates :: Date -> Period -> Int -> [Date]
genDates start_day p n =
   [ T.addGregorianDurationClip (T.CalendarDiffDays (toInteger i*mul) 0) start_day | i <- [1..n]]
   where
     mul = case p of
       Monthly -> 1
       Quarterly -> 3
       SemiAnnually -> 6
       Annually -> 12
       _ -> 0

nextDate :: Date -> Period -> Date
nextDate d p
  = T.addGregorianMonthsClip m d
    where
      m = case p of
        Monthly -> 1
        Quarterly -> 3
        SemiAnnually -> 6
        Annually -> 12
        _ -> 0

getIntervalDays :: [Date] -> [Int]
getIntervalDays ds
  = map (\(x,y)-> (fromIntegral (T.diffDays y x))) $ zip (init ds) (tail ds)

getIntervalFactors :: [Date] -> [Rate]
getIntervalFactors ds
  = map (\x -> toRational x / 365) (getIntervalDays ds) -- `debug` ("Interval Days"++show(ds))

previousDate :: T.Day -> Period -> T.Day
previousDate start_day p
   = T.addGregorianDurationClip (T.CalendarDiffDays (toInteger (-1*mul)) 0) start_day
   where
     mul = case p of
       Monthly -> 1
       Quarterly -> 3
       SemiAnnually -> 6
       Annually -> 12
       _ -> 0

monthsOfPeriod :: Period -> Int 
monthsOfPeriod p = 
    case p of 
      Monthly -> 1
      Quarterly -> 3
      SemiAnnually -> 6
      Annually -> 12


prorataFactors :: [Centi] -> Centi -> [Centi]
prorataFactors bals amt =
  case s of 
    0.0 -> replicate (length bals) 0.0
    _ -> map (\y -> (fromRational (y * (toRational amtToPay)))) weights -- `debug` ("Weights->>"++ show weights)
           where 
             weights = map (\x -> (toRational x) / s) bals
  where
    s = toRational $ sum bals
    amtToPay = min s (toRational amt)

paySeqLiabilities :: Amount -> [Balance] -> [(Amount,Balance)]
paySeqLiabilities startAmt liabilities =
  tail $ reverse $ foldl pay [(startAmt, 0)] liabilities
  where pay accum@((amt, _):xs) target = 
                         if amt >= target then
                            (amt-target, 0):accum
                         else
                            (0, target-amt):accum

paySeqLiabilitiesAmt :: Amount -> [Balance] -> [Amount]
paySeqLiabilitiesAmt startAmt funds =
    map (\(a,b) -> (a-b)) $ zip funds remainBals
  where 
    remainBals = map snd $ paySeqLiabilities startAmt funds 

afterNPeriod :: T.Day -> Integer -> Period -> T.Day
afterNPeriod d i p =
  T.addGregorianMonthsClip ( months * i)  d
  where
    months = case p of
      Monthly -> 1
      Quarterly -> 3
      SemiAnnually -> 6
      Annually -> 12

periodsBetween :: T.Day -> T.Day -> Period -> Integer
periodsBetween t1 t2 p
  = case p of
      Weekly ->  div (T.diffDays t1 t2) 7
      Monthly -> _diff
      Annually -> div _diff 12
      Quarterly -> div _diff 4
  where
    _diff = T.cdMonths $ T.diffGregorianDurationClip t1 t2


data RateAssumption = RateCurve Index Ts
                    | RateFlat Index IRate
                    deriving (Show)

mkTs :: [(Date,Rational)] -> Ts
mkTs [] = FloatCurve []
mkTs ps = FloatCurve [ TsPoint d v | (d,v) <- ps]

isTsEmpty :: Ts -> Bool
isTsEmpty (FloatCurve []) = True
isTsEmpty (RatioCurve []) = True
isTsEmpty _ = False

mkRateTs :: [(Date,IRate)] -> Ts
mkRateTs ps = IRateCurve [ TsPoint d v | (d,v) <- ps]

getValOnByDate :: Ts -> Date -> Balance
getValOnByDate (BalanceCurve dps) d 
  = case find (\(TsPoint _d _) -> ( d >= _d )) (reverse dps)  of 
      Just (TsPoint _d v) -> v
      Nothing -> 0


splitTsByDate :: Ts -> T.Day -> (Ts, Ts)
splitTsByDate (BalanceCurve ds) d
  = case (findIndex (\(TsPoint _d _) -> _d > d ) ds) of
      Nothing -> (BalanceCurve ds, BalanceCurve [])
      Just idx -> (BalanceCurve l, BalanceCurve r)
                  where
                   (l,r) = splitAt idx ds

subTsBetweenDates :: Ts -> Maybe T.Day -> Maybe T.Day -> Ts
subTsBetweenDates (BalanceCurve vs) (Just sd) (Just ed)
  =  BalanceCurve $ filter(\(TsPoint x _) -> (x > sd) && (x < ed) ) vs
subTsBetweenDates (BalanceCurve vs) Nothing (Just ed)
  =  BalanceCurve $ filter(\(TsPoint x _) ->  x < ed ) vs
subTsBetweenDates (BalanceCurve vs) (Just sd) Nothing
  =  BalanceCurve $ filter(\(TsPoint x _) ->  x > sd ) vs

sumValTs :: Ts -> Amount
sumValTs (BalanceCurve ds) = foldr (\(TsPoint _ v) acc -> acc+v ) 0 ds


toDate :: String -> T.Day
toDate s = TF.parseTimeOrError True TF.defaultTimeLocale "%Y%m%d" s

inSamePeriod :: T.Day -> T.Day -> Period -> Bool
inSamePeriod t1 t2 p
  = case p of
      Monthly -> m1 == m2
      Annually ->  y1 == y2
    where
      (y1,m1,d1) = T.toGregorian t1
      (y2,m2,d2) = T.toGregorian t2


$(deriveJSON defaultOptions ''Index)


zipWith8 :: (a->b->c->d->e->f->g->h->i) -> [a]->[b]->[c]->[d]->[e]->[f]->[g]->[h]->[i]
zipWith8 z (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs) (g:gs) (h:hs)
                   =  z a b c d e f g h : zipWith8 z as bs cs ds es fs gs hs
zipWith8 _ _ _ _ _ _ _ _ _ = []

zipWith9 :: (a->b->c->d->e->f->g->h->i->j) -> [a]->[b]->[c]->[d]->[e]->[f]->[g]->[h]->[i]->[j]
zipWith9 z (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs) (g:gs) (h:hs) (j:js)
                   =  z a b c d e f g h j : zipWith9 z as bs cs ds es fs gs hs js
zipWith9 _ _ _ _ _ _ _ _ _ _ = []

zipWith10 :: (a->b->c->d->e->f->g->h->i->j->k) -> [a]->[b]->[c]->[d]->[e]->[f]->[g]->[h]->[i]->[j]->[k]
zipWith10 z (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs) (g:gs) (h:hs) (j:js) (k:ks)
                   =  z a b c d e f g h j k: zipWith10 z as bs cs ds es fs gs hs js ks
zipWith10 _ _ _ _ _ _ _ _ _ _ _ = []


floatToFixed :: HasResolution a => Float -> Fixed a
floatToFixed x = y where
  y = MkFixed (round (fromInteger (resolution y) * x))

weightedBy :: [Centi] -> [Rational] -> Rational
weightedBy ws vs = if sum_weights == 0 then 
                     0
                   else
                     (sum $ zipWith (*) vs $ _ws ) / sum_weights
                  where 
                      _ws = map toRational ws
                      sum_weights = sum _ws

daysBetween :: Date -> Date -> Integer -- start date , end date
daysBetween sd ed = (fromIntegral (T.diffDays ed sd))

daysBetweenI :: Date -> Date -> Int 
daysBetweenI sd ed = fromInteger $ T.diffDays ed sd

