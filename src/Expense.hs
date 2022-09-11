{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Expense (Fee(..),FeeType(..),payFee)
  where

import Lib(DayCount,Period,paySeqLiabilities,Dates,DealStats
           ,appendStmt,Statement,Txn(..),Ts(..),Amount,Balance,Date,Rate)
import Data.Traversable
import Language.Haskell.TH

import qualified Data.Time as T
import qualified Data.Text
import           Data.Aeson       hiding (json)
import           Data.Aeson.TH
import           Data.Aeson.Types

import Data.Fixed

data FeeType = AnnualRateFee DealStats Rate
             | PctFee DealStats Rate
             | FixFee Balance
             | RecurFee Period Balance
             | Custom Ts
             deriving (Show,Eq)

data Fee = Fee {
  feeName :: String
  ,feeType :: FeeType
  ,feeStart :: Date
  ,feeDue :: Balance
  ,feeDueDate :: Maybe Date
  ,feeArrears :: Balance
  ,feeLastPaidDay :: Maybe Date
  ,feeStmt :: Maybe Statement
} deriving (Show,Eq)

payFee :: T.Day -> Amount -> Fee -> Fee
payFee d amt f@(Fee fn ft fs fd fdDay fa flpd fstmt) =
   f {feeLastPaidDay = Just d
     ,feeDue = dueRemain
     ,feeArrears = arrearRemain
     ,feeStmt = Just newStmt}
   where
    [(r0,arrearRemain),(r1,dueRemain)] = paySeqLiabilities amt [fa,fd]
    paid = fa + fd - arrearRemain - dueRemain
    newStmt = appendStmt fstmt (ExpTxn d dueRemain paid arrearRemain "")

$(deriveJSON defaultOptions ''FeeType)
$(deriveJSON defaultOptions ''Fee)
