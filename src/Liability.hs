{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Liability
  (Bond(..),BondType(..),OriginalInfo(..),SinkFundSchedule(..)
  ,payInt,payPrin,consolTxn,consolStmt,backoutDueIntByYield
  ,priceBond,PriceResult(..),pv,InterestInfo(..),RateReset(..)
  ,weightAverageBalance)
  where

import Language.Haskell.TH
import Data.Aeson       hiding (json)
import Data.Aeson.TH
import Data.Fixed

import qualified Data.Time as T
import Lib (Date,Balance,Rate,Spread,Index(..),Dates
           ,Period(..),Ts(..)
           ,TsPoint(..)
           ,toDate,pv2,daysBetween,Amount
           ,Period,Floor,Cap,IRate,mulBI
           ,getIntervalFactors)

import Util
import Types
import Data.Ratio 

import Stmt (Txn(..),combineTxn,Statement(..),appendStmt,getTxnDate
           ,getTxnAmt,getTxnPrincipal,getTxnAsOf,getTxnBalance,getTxnDate,sliceStmt
           ,getTxnBegBalance)

import Data.List (findIndex,zip6,find)
import qualified Cashflow as CF

import Debug.Trace
debug = flip trace

data RateReset = ByInterval Period (Maybe Date) -- period, maybe a start day
               | MonthOfYear  Int  -- month index, 0 => Janaury
               deriving (Show)

data InterestInfo = 
          Floater Index Spread RateReset DayCount (Maybe Floor) (Maybe Cap)
          | Fix IRate DayCount 
          | InterestByYield IRate
          deriving (Show)

data OriginalInfo = OriginalInfo {
  originBalance::Balance
  ,originDate::Date
  ,originRate::Rate
} deriving (Show)

type SinkFundSchedule = Ts
type PlannedAmorSchedule = Ts

data BondType = Sequential
              | SinkFund SinkFundSchedule
              | PAC PlannedAmorSchedule
              | PAC_Anchor PlannedAmorSchedule [String]
              | Lockout Date
              | Z
              | Equity
              deriving (Show)

data Bond = Bond {
  bndName :: String
  ,bndType :: BondType
  ,bndOriginInfo :: OriginalInfo
  ,bndInterestInfo :: InterestInfo
  ,bndBalance :: Balance
  ,bndRate :: IRate
  ,bndDuePrin :: Balance
  ,bndDueInt :: Balance
  ,bndLastIntPay :: Maybe Date
  ,bndLastPrinPay :: Maybe Date
  ,bndStmt :: Maybe Statement
} deriving (Show)

consolTxn :: [Txn] -> Txn -> [Txn]
consolTxn (txn:txns) txn0
  = if txn==txn0 then 
      (combineTxn txn txn0):txns
    else
      txn0:txn:txns 
consolTxn [] txn = [txn]

consolStmt :: Bond -> Bond
consolStmt b@Bond{bndStmt = Just (Statement (txn:txns))}
  =  b {bndStmt = Just (Statement (reverse (foldl consolTxn [txn] txns)))} -- `debug` ("Consoling stmt for "++ show (bndName b))

consolStmt b@Bond{bndStmt = Nothing} =  b {bndStmt = Nothing}

payInt :: Date -> Amount -> Bond -> Bond
payInt d amt bnd@(Bond bn Equity oi iinfo bal r duePrin dueInt lpayInt lpayPrin stmt)
  = Bond bn Equity oi iinfo bal r duePrin dueInt (Just d) lpayPrin (Just new_stmt)
  where
    new_stmt = appendStmt stmt (BondTxn d bal amt 0 r amt "INT PAY")

payInt d amt bnd@(Bond bn bt oi iinfo bal r duePrin dueInt lpayInt lpayPrin stmt) =
  Bond bn bt oi iinfo bal r duePrin new_due (Just d) lpayPrin (Just new_stmt)
  where
    new_due = dueInt - amt -- `debug` (">>pay INT to "++ show bn ++ ">>" ++ show amt)
    new_stmt = appendStmt stmt (BondTxn d bal amt 0 r amt ("INT PAY:Due "++show new_due))

payPrin :: Date -> Amount -> Bond -> Bond
payPrin d amt bnd@(Bond bn bt oi iinfo bal r duePrin dueInt lpayInt lpayPrin stmt) =
  Bond bn bt oi iinfo new_bal r new_due dueInt lpayInt (Just d) (Just new_stmt) -- `debug` ("New bal"++show(new_bal)++"AMT"++show(amt))
  where
    new_bal = bal - amt
    new_due = duePrin - amt
    new_stmt = appendStmt stmt (BondTxn d new_bal 0 amt 0 amt ("PRIN PAY:Due "++show new_due))

type Valuation = Micro
type PerFace = Micro
type WAL = Centi
type Duration = Micro
type Yield = Micro
type AccruedInterest = Centi
type IRR = Rational

data YieldResult = Yield

data PriceResult = PriceResult Valuation PerFace WAL Duration AccruedInterest -- valuation,wal,accu,duration
                   deriving (Show,Eq)


pv :: Ts -> Date -> Date -> Amount -> Rational
pv pc@(PricingCurve _) today d amt = 
   toRational (amt) * (1 / discount_factor) `debug` ("DF:"++show discount_factor)
  where
   distance = daysBetween today d
   discount_rate = fromRational $ getValByDate pc d `debug` ("Get val by ts"++show pc ++">>d"++ show d)
   discount_factor = (1+discount_rate) ^^ (div distance 365)   `debug` ("discount_rate"++show(discount_rate) ++" dist days=>"++show(distance))
   -- discount_factor = (1+discount_rate) ** (fromRational $ (yearCountFraction DC_ACT_ACT today d))

fv2 :: IRate -> Date -> Date -> Amount -> Amount
fv2 discount_rate today futureDay amt =
    mulBI amt ((1+discount_rate) ^^ (fromInteger (div distance 365)))
  where
    distance = daysBetween today futureDay

priceBond :: Date -> Ts -> Bond -> PriceResult
priceBond d rc b@(Bond _ _ (OriginalInfo obal od _) _ bal cr _ _ lastIntPayDay _ (Just (Statement txns)))
  = PriceResult
     presentValue
     (fromRational (100*(toRational presentValue)/(toRational obal)))
     ((foldr (\x acc ->
               (acc + ((fromIntegral (T.diffDays (getTxnDate x) d))*(getTxnPrincipal x)/365)))
             0
             futureCf)  / cutoffBalance)
     (fromRational (foldr (\x acc ->
               (((fromIntegral (T.diffDays (getTxnDate x) d))/365) * ((pv rc d (getTxnDate x)  (getTxnAmt x)) / (toRational presentValue))) + acc)
            0
            futureCf))  -- `debug` ("Cutoff balance"++show(cutoffBalance))
     accruedInt
     where
       futureCf = filter (\x -> (getTxnDate x) > d) txns
       presentValue = fromRational $ foldr (\x acc -> acc + (pv rc d (getTxnDate x) (getTxnAmt x)) ) 0 futureCf
       cutoffBalance = case (getTxnAsOf txns d) of
                          Nothing ->  (getTxnBalance fstTxn) + (getTxnPrincipal fstTxn) --  `debug` (show(getTxnBalance fstTxn))
                                     where
                                      fstTxn = head txns
                          Just _txn -> getTxnBalance _txn  --`debug` ("Found"++show(_txn))
       accruedInt = case _t of
                      Nothing -> (fromIntegral (max 0 (T.diffDays d leftPayDay))/365) * (mulBI leftBal cr)
                      Just _ -> 0  -- `debug` ("all txn"++show(_t))-- `debug` ("l day, right"++show(leftPayDay)++show(d)++show(T.diffDays leftPayDay d))
                    where
                      _t = find (\x -> (getTxnDate x) == d) txns
                      leftTxns = takeWhile (\txn -> (getTxnDate txn) < d) txns
                      (leftPayDay,leftBal) =
                        case leftTxns of
                          [] -> case lastIntPayDay of
                                 Nothing ->  (od,bal)
                                 Just _d -> (_d,bal)
                          _ -> let
                                leftTxn = last leftTxns
                              in
                                (getTxnDate leftTxn,getTxnBalance leftTxn)

priceBond d rc b@(Bond _ _ _ _ _ _ _ _ _ _ Nothing ) = PriceResult 0 0 0 0 0


_calcIRR :: Balance -> IRR -> Date -> Ts -> IRR
_calcIRR amt initIrr today (AmountCurve cashflows)
   = if ((abs(diff) < 0.005) || (abs(nextIrr-initIrr)<0.0001)) then
       initIrr
     else
       _calcIRR amt nextIrr today (AmountCurve cashflows)  -- `debug` ("NextIRR -> "++show(nextIrr))
     where
       discount (TsPoint _d _a) _r =  (toRational _a) / ((1+_r)^(div (fromIntegral (T.diffDays _d today)) 365))
       pv = foldr (\_ts acc -> (discount _ts initIrr) + acc) 0 cashflows -- `debug` ("")
       diff = pv - (toRational amt)  -- `debug` ("pv->"++show(pv))
       nextIrr = if diff > 0 then
                   initIrr * 1.01
                 else
                   initIrr * 0.99

calcBondYield :: Date -> Balance ->  Bond -> Rate
calcBondYield d cost b@(Bond _ _ _ _ _ _ _ _ _ _ (Just (Statement txns)))
 =  _calcIRR cost 0.05 d (AmountCurve cashflows)
   where
     cashflows = [ TsPoint (getTxnDate txn) (getTxnAmt txn)  | txn <- txns ]

calcBondYield _ _ (Bond _ _ _ _ _ _ _ _ _ _ Nothing) = 0

--backoutDueIntByYield2 :: Date -> Bond -> Float -> Float
--backoutDueIntByYield2 d
--                      b@(Bond _ _ (OriginalInfo obal odate _)
--                        (InterestByYield y) currentBalance _ _ _ _ _ stmt)
--                      initAmt
--  = if abs(diff_irr) < 0.0001 then
--        (initAmt - obal)  -- `debug` ("Return->"++show(initAmt - obal))
--    else
--        backoutDueIntByYield2 d b nextAmount  -- `debug` ("NextAmt=>"++show(nextAmount)++show(b)++show(d))
--    where
--     nextAmount = if diff_irr > 0 then
--                       initAmt * 1.02
--                  else
--                       initAmt * 0.98
--     diff_irr = y - _irr   `debug`  ("Found _irr=> "++show(_irr)++show(d))
--     _irr = _calcIRR obal y odate (AmountCurve (cashflows++[(TsPoint d initAmt)]))
--     cashflows = case stmt of
--                   Just (Statement txns) -> [ TsPoint (getTxnDate txn) (getTxnAmt txn)  | txn <- txns ]
--                   Nothing -> []

backoutDueIntByYield :: Date -> Bond -> Balance
backoutDueIntByYield d b@(Bond _ _ (OriginalInfo obal odate _) (InterestByYield y) currentBalance _ _ _ _ _ stmt)
  = (fv2 y odate d pv0) - obal
    where
     pv0 = obal - pvs
     pvs = sum $ [ pv2 y odate (fst cf) (snd cf)  | cf <- cashflows ]
     cashflows = case stmt of
                   Just (Statement txns) -> [ ((getTxnDate txn),(getTxnAmt txn))  | txn <- txns ]
                   Nothing -> []

weightAverageBalance :: Date -> Date -> Bond -> Balance
weightAverageBalance sd ed b@(Bond _ _ _ _ currentBalance _ _ _ _ _ stmt)
  = sum $ zipWith mulBR _bals _dfs -- `debug` ("dfs"++show(sd)++show(ed)++show(_ds)++show(_bals)++show(_dfs))  -- `debug` (">> stmt"++show(sliceStmt (bndStmt _b) sd ed))
    where
     _dfs =  getIntervalFactors $ [sd]++ _ds ++ [ed]
     _bals = [currentBalance] ++ map getTxnBegBalance txns -- `debug` ("txn"++show(txns))
     _ds = map getTxnDate txns -- `debug` ("Slice"++show((sliceStmt (bndStmt _b) sd ed)))
     _b = consolStmt b   
     txns =  case (sliceStmt (bndStmt _b) sd ed) of
                Nothing -> []
                Just (Statement _txns) -> _txns-- map getTxnBalance _txns


$(deriveJSON defaultOptions ''InterestInfo)
$(deriveJSON defaultOptions ''OriginalInfo)
$(deriveJSON defaultOptions ''BondType)
$(deriveJSON defaultOptions ''Bond)
$(deriveJSON defaultOptions ''RateReset)
$(deriveJSON defaultOptions ''PriceResult)
