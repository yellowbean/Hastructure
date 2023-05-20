{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Expense (Fee(..),FeeType(..),payFee
               ,buildFeeAccrueAction)
  where

import Lib(Period,paySeqLiabilities,Dates
           ,Amount,Balance,Date,Rate,Ts(..))
import Stmt(appendStmt,Statement,Txn(..),TxnComment(..))
import Data.Traversable
import Language.Haskell.TH

import qualified Data.Text
import           Data.Aeson       hiding (json)
import           Data.Aeson.TH
import           Data.Aeson.Types
import GHC.Generics

import Data.Fixed
import Types
import Util

data FeeType = AnnualRateFee DealStats Rate
             | PctFee DealStats Rate
             | FixFee Balance
             | RecurFee DatePattern Balance
             | NumFee DatePattern DealStats Amount
             | FeeFlow Ts
             deriving (Show,Eq, Generic)

data Fee = Fee {
  feeName :: String
  ,feeType :: FeeType
  ,feeStart :: Date
  ,feeDue :: Balance
  ,feeDueDate :: Maybe Date
  ,feeArrears :: Balance
  ,feeLastPaidDay :: Maybe Date
  ,feeStmt :: Maybe Statement
} deriving (Show,Eq, Generic)

payFee :: Date -> Amount -> Fee -> Fee
payFee d amt f@(Fee fn ft fs fd fdDay fa flpd fstmt) =
   f {feeLastPaidDay = Just d
     ,feeDue = dueRemain
     ,feeArrears = arrearRemain
     ,feeStmt = Just newStmt}
   where
    [(r0,arrearRemain),(r1,dueRemain)] = paySeqLiabilities amt [fa,fd]
    paid = fa + fd - arrearRemain - dueRemain
    newStmt = appendStmt fstmt (ExpTxn d dueRemain paid arrearRemain (PayFee fn))

buildFeeAccrueAction :: [Fee] -> Date -> [(String,Dates)] -> [(String,Dates)]
buildFeeAccrueAction [] ed r = r
buildFeeAccrueAction (fee:fees) ed r = 
  case fee of 
    (Fee fn (RecurFee dp _) fs _ _ _ _ _)
      -> buildFeeAccrueAction fees ed [(fn, projDatesByPattern dp fs ed)]++r    
    (Fee fn (FixFee _) fs _ _ _ _ _)
      -> buildFeeAccrueAction fees ed [(fn, [fs])]++r    
    (Fee fn (FeeFlow _ts) _ _ _ _ _ _)
      -> buildFeeAccrueAction fees ed [(fn, getTsDates _ts)]++r    
    _
      -> buildFeeAccrueAction fees ed r



$(deriveJSON defaultOptions ''FeeType)
$(deriveJSON defaultOptions ''Fee)
