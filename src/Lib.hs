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
    ,queryStmtAmt,previousDate
    ,Floor,Cap,TsPoint(..),RateAssumption(..)
    ,getValByDate,getValOnByDate
    ,extractTxns,groupTxns,getTxns
    ,getTxnDate,getTxnAmt,toDate
    ,paySeqLiabilitiesAmt
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
              deriving (Show)

data DealStats =  CurrentBondBalance
              | CurrentPoolBalance
              | OriginalBondBalance
              | OriginalPoolBalance
              | BondFactor
              | PoolFactor
              | PoolCollectionInt  -- a redirect map to `CurrentPoolCollectionInt T.Day`
              | CumulativeDefaultBalance T.Day
              | FutureCurrentPoolBalance T.Day
              | FutureCurrentBondBalance T.Day
              | FutureCurrentBondFactor T.Day
              | FutureCurrentPoolFactor T.Day
              | FutureOriginalPoolBalance
              | CurrentPoolCollectionInt T.Day
              | CurrentBondBalanceOf [String]
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

getTxnAmt :: Txn -> Float
getTxnAmt (BondTxn _ _ _ _ _ t _ ) = t
getTxnAmt (AccTxn _ _ t _ ) = t
getTxnAmt (ExpTxn _ _ t _ _ ) = t

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
    foldr (\x a -> (getTxnAmt x) + a) 0 resultTxns

queryStmtAmt Nothing _ = 0

data Statement = Statement [Txn]
        deriving (Show)

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
      Just (TsPoint _d v) -> v
      Nothing -> 0

getValByDates :: Ts -> [T.Day] -> [Float]
getValByDates rc ds = map (getValByDate rc) ds

toDate :: String -> T.Day
toDate s = TF.parseTimeOrError True TF.defaultTimeLocale "%Y%m%d" s


$(deriveJSON defaultOptions ''Txn)
$(deriveJSON defaultOptions ''Ts)
$(deriveJSON defaultOptions ''TsPoint)
$(deriveJSON defaultOptions ''Index)
$(deriveJSON defaultOptions ''Statement)
