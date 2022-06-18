{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Liability
  (Bond(..),BondType(..),OriginalInfo(..),SinkFundSchedule(..)
  ,InterestInfo(..),payInt,payPrin,consolTxn,consolStmt

  )
  where

import Language.Haskell.TH
import           Data.Aeson       hiding (json)
import           Data.Aeson.TH
import Lib (Period,Floor,Cap)

import qualified Data.Time as T
import Lib (Balance,Rate,Spread,Index(..),Dates,calcInt,DayCount(..)
           ,Txn(..),combineTxn,Statement(..),appendStmt,Period(..),Ts(..))
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

data BondType = Passthrough
                | SinkFund SinkFundSchedule
                | Lockout T.Day
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

--calcBond :: Bond -> T.Day -> (Float, Float) -- due principal, due interest
--calcBond bnd@(Bond bn Passthrough (OriginalInfo _ od or _) iinfo bal _ duePrin dueInt _ _ (Just stmt)) calc_date =
--  case (lastIntPay stmt) of
--    Just (d,bond_bal,arrears) ->
--      (bal, new_int+new_arrears)
--      where
--        new_int = calcInt bond_bal d calc_date 0.08 ACT_365
--        new_arrears = calcInt arrears d calc_date 0.08 ACT_365
--    Nothing ->
--      (bal, new_int)
--      where
--        new_int = calcInt bal od calc_date or ACT_365

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
payInt d amt bnd@(Bond bn Passthrough oi
                                    iinfo bal r duePrin dueInt lpayInt lpayPrin stmt) =
  Bond bn Passthrough oi iinfo new_bal r duePrin new_due (Just d) lpayPrin (Just new_stmt)
  where
    new_bal = bal - amt
    new_due = dueInt - amt
    new_stmt = appendStmt stmt (BondTxn d bal amt 0 r "INT PAY")


payPrin :: T.Day -> Float -> Bond -> Bond
payPrin d amt bnd@(Bond bn Passthrough oi
                   iinfo bal r duePrin dueInt lpayInt lpayPrin stmt) =
  Bond bn Passthrough oi iinfo new_bal r new_due dueInt lpayInt (Just d) (Just new_stmt)
  where
    new_bal = bal - amt
    new_due = duePrin - amt
    new_stmt = appendStmt stmt (BondTxn d new_bal 0 amt 0 "PRIN PAY")


$(deriveJSON defaultOptions ''InterestInfo)
$(deriveJSON defaultOptions ''OriginalInfo)
-- $(deriveJSON defaultOptions ''SinkFundSchedule)
$(deriveJSON defaultOptions ''BondType)
$(deriveJSON defaultOptions ''Bond)


