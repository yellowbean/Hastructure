{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Liability
  (Bond(..),BondType(..),OriginalInfo(..),SinkFundSchedule(..)
  ,payInt,payPrin,consolTxn,consolStmt,backoutDueIntByYield
  ,priceBond,PriceResult(..),pv,InterestInfo(..),RateReset(..))
  where

import Language.Haskell.TH
import Data.Aeson       hiding (json)
import Data.Aeson.TH
import Lib (Period,Floor,Cap,getValByDate)

import qualified Data.Time as T
import Lib (Balance,Rate,Spread,Index(..),Dates,calcInt,DayCount(..)
           ,Txn(..),combineTxn,Statement(..),appendStmt,Period(..),Ts(..)
           ,TsPoint(..),getTxnDate,getTxnAmt,getTxnPrincipal,getTxnAsOf,getTxnBalance
           ,toDate,pv2,)
import Data.List (findIndex,zip6)

import Debug.Trace
debug = flip trace

data RateReset = ByInterval Period (Maybe T.Day) -- period, maybe a start day
               | MonthOfYear     Int  -- month index, 0 => Janaury
               deriving (Show)

data InterestInfo = 
          Floater Index Spread RateReset (Maybe Floor) (Maybe Cap)
          | Fix Rate
          | InterestByYield Float
          deriving (Show)

data OriginalInfo = OriginalInfo {
  originBalance::Float
  ,originDate::T.Day
  ,originRate::Float
} deriving (Show)

type SinkFundSchedule = Ts
type PlannedAmorSchedule = Ts

data BondType = Sequential
                | SinkFund SinkFundSchedule
                | PAC PlannedAmorSchedule
                | PAC_Anchor PlannedAmorSchedule [String]
                | Lockout T.Day
                | Z
                | Equity
                deriving (Show)

data Bond = Bond {
  bndName :: String
  ,bndType :: BondType
  ,bndOriginInfo :: OriginalInfo
  ,bndInterestInfo :: InterestInfo
  ,bndBalance :: Balance
  ,bndRate :: Float
  ,bndDuePrin :: Float
  ,bndDueInt :: Float
  ,bndLastIntPay :: Maybe T.Day
  ,bndLastPrinPay :: Maybe T.Day
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
  =  b {bndStmt = Just (Statement (reverse (foldl consolTxn [txn] txns)  ))}

consolStmt b@Bond{bndStmt = Nothing} =  b {bndStmt = Nothing}

payInt :: T.Day -> Float -> Bond -> Bond
payInt d amt bnd@(Bond bn Equity oi iinfo bal r duePrin dueInt lpayInt lpayPrin stmt)
  = Bond bn Equity oi iinfo bal r duePrin dueInt (Just d) lpayPrin (Just new_stmt)
  where
    new_stmt = appendStmt stmt (BondTxn d bal amt 0 r amt "INT PAY")

payInt d amt bnd@(Bond bn bt oi iinfo bal r duePrin dueInt lpayInt lpayPrin stmt) =
  Bond bn bt oi iinfo bal r duePrin new_due (Just d) lpayPrin (Just new_stmt)
  where
    new_due = dueInt - amt
    new_stmt = appendStmt stmt (BondTxn d bal amt 0 r amt "INT PAY")

payPrin :: T.Day -> Float -> Bond -> Bond
payPrin d amt bnd@(Bond bn bt oi iinfo bal r duePrin dueInt lpayInt lpayPrin stmt) =
  Bond bn bt oi iinfo new_bal r new_due dueInt lpayInt (Just d) (Just new_stmt) -- `debug` ("New bal"++show(new_bal)++"AMT"++show(amt))
  where
    new_bal = bal - amt
    new_due = duePrin - amt
    new_stmt = appendStmt stmt (BondTxn d new_bal 0 amt 0 amt "PRIN PAY")

type Valuation = Float
type PerFace = Float
type WAL = Float
type Duration = Float
type Yield = Float
type AccruedInterest = Float

data PriceResult 
  = PriceResult Valuation PerFace WAL Duration -- valuation,wal,accu,duration
    deriving (Show,Eq)
data YieldResult = Yield

pv :: Ts -> T.Day -> T.Day -> Float -> Float
pv rc today d amt = 
    amt / (1+discount_rate)**((fromIntegral distance)/365)  -- `debug` ("discount_rate"++show(discount_rate)++" dist"++show(distance))
    where
        discount_rate = getValByDate rc d
        distance = (T.diffDays d today)

fv2 :: Float -> T.Day -> T.Day -> Float -> Float
fv2 discount_rate today futureDay amt =
    amt * (1+discount_rate)**((fromIntegral distance)/365)
  where
    distance = (T.diffDays futureDay today)

priceBond :: T.Day -> Ts -> Bond -> PriceResult
priceBond d rc b@(Bond _ _ (OriginalInfo obal _ _) _ bal _ _ _ _ _ (Just (Statement txns)))
  = PriceResult
     presentValue
     (100*presentValue/obal)
     ((foldr (\x acc ->  (acc + ((fromIntegral (T.diffDays (getTxnDate x) d))*(getTxnPrincipal x)))) 0 futureCf) / 365 / bal)
     (foldr (\x acc ->
               (((fromIntegral (T.diffDays (getTxnDate x) d))/365) * ((pv rc d (getTxnDate x)  (getTxnAmt x)) / presentValue)) + acc)
            0
            futureCf)
     where
       futureCf = filter (\x -> (getTxnDate x) > d) txns
       presentValue = foldr (\x acc -> acc + (pv rc d (getTxnDate x) (getTxnAmt x)) ) 0 futureCf
       cutoffBalance = case (getTxnAsOf txns d) of
                          Nothing -> bal    -- TODO edge case not covered
                          Just _txn -> getTxnBalance _txn

priceBond d rc b@(Bond _ _ _ _ _ _ _ _ _ _ Nothing ) = PriceResult 0 0 0 0

type IRR = Float
type InitBalance = Float

_calcIRR :: InitBalance -> IRR -> T.Day -> Ts -> IRR
_calcIRR amt initIrr today (AmountCurve cashflows)
   = if ((abs(diff) < 0.005) || (abs(nextIrr-initIrr)<0.0001)) then
       initIrr
     else
       _calcIRR amt nextIrr today (AmountCurve cashflows)  -- `debug` ("NextIRR -> "++show(nextIrr))
     where
       discount (TsPoint _d _a) _r =  _a / ((1+_r)^(div (fromIntegral (T.diffDays _d today)) 365))
       pv = foldr (\_ts acc -> (discount _ts initIrr) + acc) 0 cashflows -- `debug` ("")
       diff = pv - amt  -- `debug` ("pv->"++show(pv))
       nextIrr = if diff > 0 then
                   initIrr * 1.01
                 else
                   initIrr * 0.99

calcBondYield :: T.Day -> Float ->  Bond -> Float
calcBondYield d cost b@(Bond _ _ _ _ _ _ _ _ _ _ (Just (Statement txns)))
 =  _calcIRR cost 0.05 d (AmountCurve cashflows)
   where
     cashflows = [ TsPoint (getTxnDate txn) (getTxnAmt txn)  | txn <- txns ]

calcBondYield _ _ (Bond _ _ _ _ _ _ _ _ _ _ Nothing) = 0

backoutDueIntByYield2 :: T.Day -> Bond -> Float -> Float
backoutDueIntByYield2 d b@(Bond _ _ (OriginalInfo obal odate _)
                           (InterestByYield y) currentBalance _ _ _ _ _ stmt)
                       initAmt
  = if abs(diff_irr) < 0.0001 then
        (initAmt - obal)   `debug` ("Return->"++show(initAmt - obal))
    else
        backoutDueIntByYield2 d b nextAmount  `debug` ("NextAmt=>"++show(nextAmount)++show(b)++show(d))
    where
     nextAmount = if diff_irr > 0 then
                       initAmt * 1.02
                  else
                       initAmt * 0.98
     diff_irr = y - _irr   `debug`  ("Found _irr=> "++show(_irr)++show(d))
     _irr = _calcIRR obal y odate (AmountCurve (cashflows++[(TsPoint d initAmt)]))
     cashflows = case stmt of
                   Just (Statement txns) -> [ TsPoint (getTxnDate txn) (getTxnAmt txn)  | txn <- txns ]
                   Nothing -> []

backoutDueIntByYield :: T.Day -> Bond -> Float
backoutDueIntByYield d b@(Bond _ _ (OriginalInfo obal odate _)
                           (InterestByYield y) currentBalance _ _ _ _ _ stmt)
  = (fv2 y odate d pv0) - obal
    where
     pv0 = obal - pvs
     pvs = sum $ [ pv2 y odate (fst cf) (snd cf)  | cf <- cashflows ]
     cashflows = case stmt of
                   Just (Statement txns) -> [ ((getTxnDate txn),(getTxnAmt txn))  | txn <- txns ]
                   Nothing -> []

$(deriveJSON defaultOptions ''InterestInfo)
$(deriveJSON defaultOptions ''OriginalInfo)
$(deriveJSON defaultOptions ''BondType)
$(deriveJSON defaultOptions ''Bond)
$(deriveJSON defaultOptions ''RateReset)
$(deriveJSON defaultOptions ''PriceResult)
