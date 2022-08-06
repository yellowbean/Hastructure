{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lib
    (Rate,Dates,Period(..),calcInt,calcIntRate,Balance,DayCount(..)
    ,genDates,StartDate,EndDate,LastIntPayDate
    ,Spread,Index(..)
    ,paySeqLiabilities,prorataFactors,periodToYear
    ,afterNPeriod,DealStats(..),Ts(..),periodsBetween
    ,Txn(..),combineTxn,Statement(..)
    ,appendStmt,periodRateFromAnnualRate
    ,queryStmtAmt,previousDate,inSamePeriod
    ,Floor,Cap,TsPoint(..),RateAssumption(..)
    ,getValByDate,getValOnByDate
    ,extractTxns,groupTxns,getTxns
    ,getTxnDate,getTxnAmt,toDate,getTxnPrincipal,getTxnAsOf,getTxnBalance
    ,paySeqLiabilitiesAmt,getIntervalDays
    ,zipWith8,zipWith9, pv2, monthsOfPeriod
    ,weightedBy, getValByDates, mkTs
    ) where

import qualified Data.Time as T
import qualified Data.Time.Format as TF
import Data.List
import qualified Data.Map as M
import Language.Haskell.TH
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Aeson hiding (json)
import Text.Regex.TDFA

import Debug.Trace
debug = flip trace

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
type Principal = Float
type Interest = Float
type Cash = Float
type Cap = Float

data Period = Daily 
              | Weekly 
              | Monthly 
              | Quarterly 
              | SemiAnnually 
              | Annually
              deriving (Show,Eq)

data DealStats =  CurrentBondBalance
              | CurrentPoolBalance
              | OriginalBondBalance
              | OriginalPoolBalance
              | BondFactor
              | PoolFactor
              | PoolCollectionInt  -- a redirect map to `CurrentPoolCollectionInt T.Day`
              | AllAccBalance
              | CumulativeDefaultBalance T.Day
              | FutureCurrentPoolBalance T.Day
              | FutureCurrentPoolDefaultBalance T.Day
              | FutureCurrentBondBalance T.Day
              | FutureCurrentBondFactor T.Day
              | FutureCurrentPoolFactor T.Day
              | FutureOriginalPoolBalance
              | CurrentPoolCollectionInt T.Day
              | CurrentBondBalanceOf [String]
              | Max DealStats DealStats
              | Min DealStats DealStats
              | Sum [DealStats]
              deriving (Show,Eq)

data DealFlags = Flags Bool -- dummy , this data intends to provide boolean flags regards to a deal

$(deriveJSON defaultOptions ''DealStats)
$(deriveJSON defaultOptions ''Period)

data Index = LPR5Y
            | LPR1Y
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

getIntervalDays :: [T.Day] -> [Int]
getIntervalDays ds
  = map (\(x,y)-> (fromIntegral (T.diffDays y x))) $ zip (init ds) (tail ds)

getIntervalFactors :: [T.Day] -> [Float]
getIntervalFactors ds
  = map (\x ->  (fromIntegral x)/365) (getIntervalDays ds)

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


prorataFactors :: [Float] -> Float -> [Float]
prorataFactors bals amt =
  case s of 
    0.0 -> bals
    _ -> map (\y -> y * amtToPay) (map (\x -> x / s) bals)
  where
    s = foldl (+) 0 bals
    amtToPay = min s amt

paySeqLiabilities :: Float -> [Float] -> [(Float,Float)]
paySeqLiabilities startAmt liabilities =
  tail $ reverse $ foldl pay [(startAmt, 0)] liabilities
  where pay accum@((amt, _):xs) target = 
                         if amt >= target then
                            (amt-target, 0):accum
                         else
                            (0, target-amt):accum

paySeqLiabilitiesAmt :: Float -> [Float] -> [Float]
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
    Monthly -> _diff
    Annually -> div _diff 12
    Quarterly -> div _diff 4
  where
    _diff = T.cdMonths $ T.diffGregorianDurationClip t1 t2

data Txn = BondTxn T.Day Balance Interest Principal Rate Cash Comment
          | AccTxn T.Day Balance Amount Comment
          | ExpTxn T.Day Balance Amount Balance Comment
        deriving (Show)

getTxnComment :: Txn -> String
getTxnComment (BondTxn _ _ _ _ _ _ t ) = t
getTxnComment (AccTxn _ _ _ t ) = t
getTxnComment (ExpTxn _ _ _ _ t ) = t

getTxnDate :: Txn -> T.Day 
getTxnDate (BondTxn t _ _ _ _ _ _ ) = t
getTxnDate (AccTxn t _ _ _ ) = t
getTxnDate (ExpTxn t _ _ _ _ ) = t

getTxnBalance :: Txn -> Balance
getTxnBalance (BondTxn _ t _ _ _ _ _ ) = t
getTxnBalance (AccTxn _ t _ _ ) = t
getTxnBalance (ExpTxn _ t _ _ _ ) = t

getTxnPrincipal :: Txn -> Float
getTxnPrincipal (BondTxn _ _ _ t _ _ _ ) = t

getTxnAmt :: Txn -> Float
getTxnAmt (BondTxn _ _ _ _ _ t _ ) = t
getTxnAmt (AccTxn _ _ t _ ) = t
getTxnAmt (ExpTxn _ _ t _ _ ) = t

getTxnAsOf :: [Txn] -> T.Day -> Maybe Txn
getTxnAsOf txns d = find (\x -> (getTxnDate x) <= d) $ reverse txns


emptyTxn :: Txn -> T.Day -> Txn 
emptyTxn (BondTxn _ _ _ _ _ _ _ ) d = (BondTxn d 0 0 0 0 0 "" )
emptyTxn (AccTxn _ _ _ _  ) d = (AccTxn d 0 0 "" )
emptyTxn (ExpTxn _ _ _ _ _ ) d = (ExpTxn d 0 0 0 "" )

getTxnByDate :: [Txn] -> T.Day -> Maybe Txn
getTxnByDate ts d = find (\x -> (d == (getTxnDate x))) ts

queryStmtAmt :: Maybe Statement -> String -> Float
queryStmtAmt (Just (Statement txns)) q =
  let
    resultTxns = filter (\txn -> (getTxnComment txn) =~ q)  txns
  in
    abs $ foldr (\x a -> (getTxnAmt x) + a) 0 resultTxns

queryStmtAmt Nothing _ = 0

data Statement = Statement [Txn]
        deriving (Show,Eq)

appendStmt :: Maybe Statement -> Txn -> Statement
appendStmt (Just stmt@(Statement txns)) txn = Statement (txns++[txn])
appendStmt Nothing txn = Statement [txn]

extractTxns :: [Txn] -> [Statement] -> [Txn]
extractTxns rs ((Statement _txns):stmts) = extractTxns (rs++_txns) stmts 
extractTxns rs [] = rs

getTxns :: Maybe Statement -> [Txn]
getTxns Nothing = []
getTxns (Just (Statement txn)) = txn

groupTxns :: Maybe Statement -> M.Map T.Day [Txn]
groupTxns (Just (Statement txns))
  = M.fromAscListWith (++) $ [(getTxnDate txn,[txn]) | txn <- txns]
-- groupTxns Nothing = mempty

combineTxn :: Txn -> Txn -> Txn
combineTxn (BondTxn d1 b1 i1 p1 r1 c1 m1) (BondTxn d2 b2 i2 p2 r2 c2 m2)
    = BondTxn d1 (min b1 b2) (i1 + i2) (p1 + p2) (r1+r2) (c1+c2) ""

instance Ord Txn where
  compare (BondTxn d1 _ _ _ _ _ _ ) (BondTxn d2 _ _ _ _ _ _ )
    = compare d1 d2

instance Eq Txn where 
  (BondTxn d1 _ _ _ _ _ _ ) == (BondTxn d2 _ _ _ _ _ _ )
    = d1 == d2

data TsPoint a = TsPoint T.Day a
                deriving (Show)

data Ts = FloatCurve [(TsPoint Float)]
         |BoolCurve [(TsPoint Bool)]
         |AmountCurve [(TsPoint Float)]
         deriving (Show)

data RateAssumption = RateCurve Index Ts
                    | RateFlat Index Float
                    deriving (Show)

mkTs :: [(T.Day,Float)] -> Ts
mkTs ps = FloatCurve [ (TsPoint d v)  | (d,v) <- ps]

getValOnByDate :: Ts -> T.Day -> Float
getValOnByDate (AmountCurve dps) d 
  = case find (\(TsPoint _d _) -> ( d >= _d )) (reverse dps)  of 
      Just (TsPoint _d v) -> v
      Nothing -> 0

getValByDate :: Ts -> T.Day -> Float
getValByDate (AmountCurve dps) d 
  = case find (\(TsPoint _d _) -> ( d > _d )) (reverse dps)  of 
      Just (TsPoint _d v) -> v
      Nothing -> 0

getValByDate (FloatCurve dps) d 
  = case find (\(TsPoint _d _) -> ( d > _d )) (reverse dps)  of 
      Just (TsPoint _d v) -> v  -- `debug` ("Getting rate "++show(_d)++show(v))
      Nothing -> 0              -- `debug` ("Getting 0 ")

getValByDates :: Ts -> [T.Day] -> [Float]
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


$(deriveJSON defaultOptions ''Txn)
$(deriveJSON defaultOptions ''Ts)
$(deriveJSON defaultOptions ''TsPoint)
$(deriveJSON defaultOptions ''Index)
$(deriveJSON defaultOptions ''Statement)



zipWith8 :: (a->b->c->d->e->f->g->h->i) -> [a]->[b]->[c]->[d]->[e]->[f]->[g]->[h]->[i]
zipWith8 z (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs) (g:gs) (h:hs)
                   =  z a b c d e f g h : zipWith8 z as bs cs ds es fs gs hs
zipWith8 _ _ _ _ _ _ _ _ _ = []

zipWith9 :: (a->b->c->d->e->f->g->h->i->j) -> [a]->[b]->[c]->[d]->[e]->[f]->[g]->[h]->[i]->[j]
zipWith9 z (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs) (g:gs) (h:hs) (j:js)
                   =  z a b c d e f g h j : zipWith9 z as bs cs ds es fs gs hs js
zipWith9 _ _ _ _ _ _ _ _ _ _ = []

pv2 :: Float -> T.Day -> T.Day -> Float -> Float
pv2 discount_rate today d amt =
    amt / (1+discount_rate)**((fromIntegral distance)/365)
  where
    distance = (T.diffDays d today)

weightedBy :: [Float] -> [Float] -> Float
weightedBy ws vs =  sum $ zipWith (*) ws vs

