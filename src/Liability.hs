{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Liability
  (Bond(..),BondType(..),OriginalInfo(..),SinkFundSchedule(..)
  ,InterestInfo(..),payInt,payPrin,consolTxn,consolStmt )
  where

import Language.Haskell.TH
import           Data.Aeson       hiding (json)
import           Data.Aeson.TH
import Lib (Period,Floor,Cap,getValByDate)

import qualified Data.Time as T
import Lib (Balance,Rate,Spread,Index(..),Dates,calcInt,DayCount(..)
           ,Txn(..),combineTxn,Statement(..),appendStmt,Period(..),Ts(..)
           ,TsPoint(..),getTxnDate,getTxnAmt)
import Data.List (findIndex,zip6)

data InterestInfo = 
          Floater Index Spread Rate Period (Maybe Floor) (Maybe Cap)
          | Fix Rate
          deriving (Show)

data OriginalInfo = OriginalInfo {
  originBalance::Float
  ,originDate::T.Day
  ,originRate::Float
} deriving (Show)

type SinkFundSchedule = Ts
type PlannedAmorSchedule = Ts


data BondType = Passthrough
                | SinkFund SinkFundSchedule
                | PAC PlannedAmorSchedule
                | Lockout T.Day
                | Z
                deriving (Show)

_pac = PAC $ AmountCurve [TsPoint (T.fromGregorian 2022 3 1) 100.0
                        ,TsPoint (T.fromGregorian 2022 6 1) 100.0]

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
payInt d amt bnd@(Bond bn bt oi iinfo bal r duePrin dueInt lpayInt lpayPrin stmt) =
  Bond bn bt oi iinfo bal r duePrin new_due (Just d) lpayPrin (Just new_stmt)
  where
    new_due = dueInt - amt
    new_stmt = appendStmt stmt (BondTxn d bal amt 0 r amt "INT PAY")

payPrin :: T.Day -> Float -> Bond -> Bond
payPrin d amt bnd@(Bond bn bt oi iinfo bal r duePrin dueInt lpayInt lpayPrin stmt) =
  Bond bn bt oi iinfo new_bal r new_due dueInt lpayInt (Just d) (Just new_stmt)
  where
    new_bal = bal - amt
    new_due = duePrin - amt
    new_stmt = appendStmt stmt (BondTxn d new_bal 0 amt 0 amt "PRIN PAY")

data PriceResult = PriceResult Float -- valuation,wal,accu,duration

pv :: Ts -> T.Day -> T.Day -> Float -> Float
pv rc today d amt = 
    amt / (1+discount_rate)^(div distance 365)
    where
        discount_rate = getValByDate rc d
        distance = fromIntegral (T.diffDays d today)


priceBond :: T.Day -> Ts -> Bond -> PriceResult
priceBond d rc b = 
  case (bndStmt b) of 
    Nothing -> PriceResult 0 
    (Just (Statement txns)) -> PriceResult $ sum $ map (\x -> (pv rc d (getTxnDate x) (getTxnAmt x))) txns
    
    

$(deriveJSON defaultOptions ''InterestInfo)
$(deriveJSON defaultOptions ''OriginalInfo)
$(deriveJSON defaultOptions ''BondType)
$(deriveJSON defaultOptions ''Bond)
