{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Expense (Fee(..),FeeType(..),payFee)
  where

import Lib(DayCount,Period,paySeqLiabilities,Dates,DealStats
           ,appendStmt,Statement,Txn(..))
import Data.Traversable
import Language.Haskell.TH

import qualified Data.Time as T
import qualified Data.Text
import           Data.Aeson       hiding (json)
import           Data.Aeson.TH
import Data.Aeson.Types
import Control.Monad (mzero)
import qualified Data.ByteString.Lazy.Char8 as L

data FeeType = AnnualRateFee DealStats Float
              |PctFee DealStats Float
              |FixFee Float
              |RecurFee Period Float
              deriving (Show)
--calcFee :: FeeType -> T.Day ->
data Fee = Fee {
  feeName :: String
  ,feeType :: FeeType
  ,feeStart :: T.Day
  ,feeDue :: Float
  ,feeDueDate :: Maybe T.Day
  ,feeArrears :: Float
  ,feeLastPaidDay :: Maybe T.Day
  ,feeStmt :: Maybe Statement
} deriving (Show)

payFee :: T.Day -> Float -> Fee -> Fee
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
