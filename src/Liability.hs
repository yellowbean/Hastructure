{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Liability
  (Bond(..),BondType(..),OriginalInfo(..),SinkFundSchedule(..)
  ,InterestInfo(..), Statement(..))
  where

import Language.Haskell.TH
import           Data.Aeson       hiding (json)
import           Data.Aeson.TH


import qualified Data.Time as T
import Lib (Balance,Rate,Spread,Index,Dates,calcInt,DayCount(..))
import Data.List (findIndex)

data InterestInfo = Float Index Spread
          | Fix Rate
          | None
          deriving (Show)

data OriginalInfo = OriginalInfo {
  originBalance::Float
  ,originDate::T.Day
  ,originRate::Float
  ,originLockoutEnd::(Maybe T.Day)
} deriving (Show)

data SinkFundSchedule = SinkFundSchedule {
  sk_balance::Float
  ,sk_date::T.Day
} deriving (Show)

data Statement = Statement {
    stmtDate     ::Dates
    ,stmtEndBalance ::[Balance]
    ,stmtIntPaid     ::[Float]
    ,stmtPrinPaid     ::[Float]
    ,stmtIntArrears     ::[Balance]
    --,stmtPrinBehind     ::[Float]
    ,stmtMemo    ::[String]
} deriving (Show)

lastIntPay :: Statement -> Maybe (T.Day, Balance, Balance)
lastIntPay stmt =
    case idx of
      Nothing -> Nothing
      Just n -> Just((stmtDate stmt)!!n ,(stmtEndBalance stmt)!!n,(stmtIntArrears stmt)!!n)
    where
      idx = findIndex (\x -> x>0)  $ reverse (stmtIntPaid stmt)

data BondType = Passthrough
                | SinkFund SinkFundSchedule
                deriving (Show)

data Bond = Bond {
  bndName :: String
  ,bndType :: BondType
  ,bndOriginInfo :: OriginalInfo
  ,bndInterestInfo :: InterestInfo
  ,bndBalance :: Balance
  ,bndStmt :: Maybe Statement
} deriving (Show)

calcBond :: Bond -> T.Day -> (Float, Float) -- due principal, due interest
calcBond bnd@(Bond bn Passthrough (OriginalInfo _ od or _) iinfo bal (Just stmt)) calc_date =
  case (lastIntPay stmt) of
    Just (d,bond_bal,arrears) ->
      (bal, new_int+new_arrears)
      where
        new_int = calcInt bond_bal d calc_date 0.08 ACT_365
        new_arrears = calcInt arrears d calc_date 0.08 ACT_365
    Nothing ->
      (bal, new_int)
      where
        new_int = calcInt bal od calc_date or ACT_365

appendStmt :: Statement -> T.Day -> Balance -> Float -> Float -> Float -> String -> Statement
appendStmt stmt@(Statement ds bals int_paids prin_paids int_arrears memos) d bal _int _prin _int_arrears memo
  = Statement (ds ++ [d])
              (bals ++ [bal])
              (int_paids ++ [_int])
              (prin_paids ++ [_prin])
              (int_arrears ++ [_int_arrears])
              (memos ++ [memo])

--getStmt :: Bond -> Statement
--getStmt b = (fromJust (bndStmt b))
--
--getBal :: Bond -> Balance
--getBal b =(bndBalance b)
--
--payPrin :: Bond -> T.Day -> Float -> Bond
--payPrin b pay_date amount =
--  b { bndBalance=new_bal, bndStmt=new_stmt}
--  where
--    new_bal = getBal b
--    new_stmt = appendStmt (getStmt b) pay_date new_bal  0 amount 0 "PRIN PAY"
--
--payInt :: Bond -> T.Day -> Float -> Bond
--payInt b pay_date amount =
--  b {  bndStmt=new_stmt}
--  where
--    bal = getBal b
--    new_stmt = appendStmt (getStmt b) pay_date bal amount 0 0 "PRIN INT"


$(deriveJSON defaultOptions ''InterestInfo)
$(deriveJSON defaultOptions ''OriginalInfo)
$(deriveJSON defaultOptions ''SinkFundSchedule)
$(deriveJSON defaultOptions ''Statement)
$(deriveJSON defaultOptions ''BondType)
$(deriveJSON defaultOptions ''Bond)
$(deriveJSON defaultOptions ''Index)


