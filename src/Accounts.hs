{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Accounts (Account(..),AccountType(..),draw,deposit)
    where
import qualified Data.Time as T
import Lib (Period(Monthly),Rate,Balance,Dates,StartDate,EndDate,LastIntPayDate
           ,DayCount(ACT_365),calcInt)

import           Data.Aeson       hiding (json)
import Language.Haskell.TH
import Data.Aeson.TH
import Data.Aeson.Types

data AccountType = Virtual
                 | BankAccount Rate Period
                 deriving (Show)

data Statement = Statement {
    stmtDate     ::Dates
    ,stmtBalance ::[Balance]
    ,stmtAmt     ::[Float]
    ,stmtMemo    ::[String]
} deriving (Show)


data Account = Account {
    accBalance :: Float
    ,accName :: String
    ,accType :: AccountType
    ,accStmt :: Maybe Statement
} deriving (Show)

$(deriveJSON defaultOptions ''AccountType)
$(deriveJSON defaultOptions ''Account)
$(deriveJSON defaultOptions ''Statement)


depositInt :: Account -> StartDate -> EndDate -> Account
depositInt acc@(Account
                bal
                _
                (BankAccount r _)
                stmt)
                sd
                ed =
  acc {accBalance = newBal,accStmt = (Just newStmt)}
  where
    newBal = (accured_int + bal)
    accured_int =  calcInt bal sd ed r ACT_365
    newStmt = appendStmt stmt ed accured_int newBal "Deposit Int"

transfer :: Account -> Float -> T.Day -> Account -> (Account, Account)
transfer source_acc@(Account s_bal _ _ s_stmt)
         amount
         d
         target_acc@(Account t_bal _ _ t_stmt)
  = (source_acc {accBalance = new_s_bal, accStmt = (Just source_newStmt)}
    ,target_acc {accBalance = new_t_bal, accStmt = (Just target_newStmt)})
  where
    new_s_bal = s_bal - amount
    new_t_bal = t_bal + amount
    source_newStmt = appendStmt s_stmt d (- amount) new_s_bal "Transfer out"
    target_newStmt = appendStmt t_stmt d amount new_t_bal "Transfer in"

deposit :: Float -> T.Day -> String -> Account -> Account
deposit amount d source acc@(Account bal _ _ maybeStmt)  =
    acc {accBalance = newBal, accStmt = Just newStmt}
  where
    newBal = bal + amount
    newStmt = appendStmt maybeStmt d amount newBal source

draw :: Float -> T.Day -> String -> Account -> Account
draw amount d source acc = deposit (- amount) d source acc

appendStmt :: Maybe Statement -> T.Day -> Float -> Float -> String -> Statement
appendStmt (Just stmt@(Statement ds bals amts memos)) d amount bal memo
  = Statement (ds ++ [d])
              (bals ++ [bal])
              (amts ++ [amount])
              (memos ++ [memo])

appendStmt Nothing d amount bal memo
  = Statement [d] [bal] [amount] [memo]


getAvailBal :: Account -> Float
getAvailBal a = (accBalance a)