{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lib
    (Amount,Rate,Dates,Period(..),calcInt,calcIntRate,Balance,DayCount(..)
    ,genDates,StartDate,EndDate,LastIntPayDate,daysBetween
    ,Spread,Index(..),Date
    ,paySeqLiabilities,prorataFactors,periodToYear
    ,afterNPeriod,DealStats(..),Ts(..),periodsBetween
    ,periodRateFromAnnualRate
    ,previousDate,inSamePeriod
    ,Floor,Cap,TsPoint(..),RateAssumption(..)
    ,toDate
    ,getValByDate,getValOnByDate,sumValTs,subTsBetweenDates,splitTsByDate
    ,paySeqLiabilitiesAmt,getIntervalDays,getIntervalFactors,nextDate
    ,zipWith8,zipWith9, pv2, monthsOfPeriod
    ,weightedBy, getValByDates, mkTs, DealStatus(..)
    ,mulBI,mkRateTs,Pre(..)
    ,Interest,Principal,IRate,Cash,Comment
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


type Rate = Rational  -- general Rate like pool factor
type IRate = Micro    -- Interest Rate Type
type Spread = Micro
type Balance = Centi
type Amount = Centi
type Comment = String
type Dates = [Date]
type Date = T.Day
type StartDate = T.Day
type EndDate = T.Day
type LastIntPayDate = T.Day
type Floor = Micro
type Principal = Centi
type Interest = Centi
type Cash = Centi
type Cap = Micro

data Period = Daily 
              | Weekly 
              | Monthly 
              | Quarterly 
              | SemiAnnually 
              | Annually
              deriving (Show,Eq)

data DealStats =  CurrentBondBalance
              | CurrentPoolBalance
              | CurrentPoolBegBalance
              | CurrentPoolDefaultedBalance
              | OriginalBondBalance
              | OriginalPoolBalance
              | BondFactor
              | PoolFactor
              | PoolCollectionInt  -- a redirect map to `CurrentPoolCollectionInt T.Day`
              | AllAccBalance
              | CumulativeDefaultBalance Date
              | FutureCurrentPoolBalance Date
              | FutureCurrentPoolBegBalance Date
              | FutureCurrentPoolDefaultBalance Date
              | FutureCurrentBondBalance Date
              | FutureCurrentBondFactor Date
              | FutureCurrentPoolFactor Date
              | FutureOriginalPoolBalance
              | CurrentBondBalanceOf [String]
              | Factor DealStats Float
              | BondIntPaidAt Date String
              | BondsIntPaidAt Date [String]
              | FeePaidAt Date String
              | FeesPaidAt Date [String]
              | CurrentDueBondInt [String]
              | CurrentDueFee [String]
              | Max DealStats DealStats
              | Min DealStats DealStats
              | LastBondIntPaid [String]
              | LastFeePaid [String]
              | BondBalanceHistory Date Date
              | PoolCollectionIntHistory Date Date
              | Sum [DealStats]
              deriving (Show,Eq)


data DealFlags = Flags Bool -- dummy , this data intends to provide boolean flags regards to a deal
$(deriveJSON defaultOptions ''Period)

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


periodToYear :: Date -> Date -> DayCount -> Rational
periodToYear start_date end_date day_count =
  case day_count of
    DC_ACT_360 -> days / 360
    DC_ACT_365 -> days / 365
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

periodRateFromAnnualRate :: Period -> IRate -> IRate
periodRateFromAnnualRate Annually annual_rate  = annual_rate
periodRateFromAnnualRate Monthly annual_rate  = annual_rate / 12
periodRateFromAnnualRate Quarterly annual_rate  = annual_rate / 4
periodRateFromAnnualRate SemiAnnually annual_rate  = annual_rate / 2


calcIntRate :: T.Day -> T.Day -> IRate -> DayCount -> IRate
calcIntRate start_date end_date int_rate day_count =
   int_rate * (fromRational (periodToYear start_date end_date day_count))

calcInt :: Balance -> T.Day -> T.Day -> IRate -> DayCount -> Amount
calcInt bal start_date end_date int_rate day_count =
  fromRational $ (toRational bal) * (toRational (calcIntRate start_date end_date int_rate day_count)) --TODO looks strange

addD :: T.Day -> T.CalendarDiffDays -> T.Day
addD d calendarMonth = T.addGregorianDurationClip T.calendarMonth d

mulBI :: Balance -> IRate -> Amount
mulBI bal r = fromRational  $ (toRational bal) * (toRational r)

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
  = map (\x -> (toRational x) / 365) (getIntervalDays ds) -- `debug` ("Interval Days"++show(ds))

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
    0.0 -> bals
    _ -> map (\y -> y * amtToPay) (map (\x -> x / s) bals)
  where
    s = foldl (+) 0 bals
    amtToPay = min s amt

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




data TsPoint a = TsPoint T.Day a
                deriving (Show,Eq)

data Ts = FloatCurve [TsPoint Rational]
         |BoolCurve [TsPoint Bool]
         |AmountCurve [TsPoint Amount]
         |BalanceCurve [TsPoint Balance]
         |IRateCurve [TsPoint IRate]
         deriving (Show,Eq)

instance Ord a => Ord (TsPoint a) where
  compare (TsPoint d1 tv1) (TsPoint d2 tv2)
    = compare d1 d2


data RateAssumption = RateCurve Index Ts
                    | RateFlat Index IRate
                    deriving (Show)

mkTs :: [(T.Day,Rational)] -> Ts
mkTs ps = FloatCurve [ TsPoint d v | (d,v) <- ps]

mkRateTs :: [(T.Day,IRate)] -> Ts
mkRateTs ps = IRateCurve [ TsPoint d v | (d,v) <- ps]

getValOnByDate :: Ts -> T.Day -> Amount
getValOnByDate (AmountCurve dps) d 
  = case find (\(TsPoint _d _) -> ( d >= _d )) (reverse dps)  of 
      Just (TsPoint _d v) -> v
      Nothing -> 0

getValByDate :: Ts -> T.Day -> Rational
getValByDate (AmountCurve dps) d 
  = case find (\(TsPoint _d _) -> ( d > _d )) (reverse dps)  of 
      Just (TsPoint _d v) -> toRational v
      Nothing -> 0

getValByDate (FloatCurve dps) d 
  = case find (\(TsPoint _d _) -> ( d > _d )) (reverse dps)  of 
      Just (TsPoint _d v) -> toRational v  -- `debug` ("Getting rate "++show(_d)++show(v))
      Nothing -> 0              -- `debug` ("Getting 0 ")
getValByDate (IRateCurve dps) d
  = case find (\(TsPoint _d _) -> ( d > _d )) (reverse dps)  of
      Just (TsPoint _d v) -> toRational v  -- `debug` ("Getting rate "++show(_d)++show(v))
      Nothing -> 0              -- `debug` ("Getting 0 ")

splitTsByDate :: Ts -> T.Day -> (Ts, Ts)
splitTsByDate (AmountCurve ds) d
  = case (findIndex (\(TsPoint _d _) -> _d >= d ) ds) of
      Nothing -> (AmountCurve ds, AmountCurve [])
      Just idx -> (AmountCurve l, AmountCurve r)
                  where
                   (l,r) = splitAt idx ds

subTsBetweenDates :: Ts -> Maybe T.Day -> Maybe T.Day -> Ts
subTsBetweenDates (AmountCurve vs) (Just sd) (Just ed)
  =  AmountCurve $ filter(\(TsPoint x _) -> (x > sd) && (x < ed) ) vs
subTsBetweenDates (AmountCurve vs) Nothing (Just ed)
  =  AmountCurve $ filter(\(TsPoint x _) ->  x < ed ) vs
subTsBetweenDates (AmountCurve vs) (Just sd) Nothing
  =  AmountCurve $ filter(\(TsPoint x _) ->  x > sd ) vs

sumValTs :: Ts -> Amount
sumValTs (AmountCurve ds) = foldr (\(TsPoint _ v) acc -> acc+v ) 0 ds

getValByDates :: Ts -> [T.Day] -> [Rational]
getValByDates rc ds = map (getValByDate rc) ds

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


$(deriveJSON defaultOptions ''Ts)
$(deriveJSON defaultOptions ''TsPoint)
$(deriveJSON defaultOptions ''Index)



zipWith8 :: (a->b->c->d->e->f->g->h->i) -> [a]->[b]->[c]->[d]->[e]->[f]->[g]->[h]->[i]
zipWith8 z (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs) (g:gs) (h:hs)
                   =  z a b c d e f g h : zipWith8 z as bs cs ds es fs gs hs
zipWith8 _ _ _ _ _ _ _ _ _ = []

zipWith9 :: (a->b->c->d->e->f->g->h->i->j) -> [a]->[b]->[c]->[d]->[e]->[f]->[g]->[h]->[i]->[j]
zipWith9 z (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs) (g:gs) (h:hs) (j:js)
                   =  z a b c d e f g h j : zipWith9 z as bs cs ds es fs gs hs js
zipWith9 _ _ _ _ _ _ _ _ _ _ = []

pv2 :: IRate -> Date -> Date -> Amount -> Amount
pv2 discount_rate today d amt =
    mulBI amt $ 1/denominator
  where
    denominator = (1+discount_rate) ^^ (fromInteger (div distance 365))
    distance =  daysBetween d today

floatToFixed :: HasResolution a => Float -> Fixed a
floatToFixed x = y where
  y = MkFixed (round (fromInteger (resolution y) * x))

weightedBy :: [Centi] -> [Rational] -> Rational
weightedBy ws vs =  sum $ zipWith (*) vs $ map toRational ws

daysBetween :: Date -> Date -> Integer
daysBetween sd ed = (fromIntegral (T.diffDays sd ed))

data DealStatus = EventOfAccelerate (Maybe T.Day)
                | EventOfDefault (Maybe T.Day)
                | Current
                | Revolving
                | Ended
                deriving (Show)

$(deriveJSON defaultOptions ''DealStatus)
$(deriveJSON defaultOptions ''DealStats)

data Pre = And Pre Pre
         | Or Pre Pre
         | IfZero DealStats
         | IfGT DealStats Centi
         | IfGET DealStats Centi
         | IfLT DealStats Centi
         | IfLET DealStats Centi
         | IfDealStatus DealStatus
         deriving (Show)

$(deriveJSON defaultOptions ''Pre)
