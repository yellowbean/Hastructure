{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Expense (Fee(..),FeeType(..),payFee,payResidualFee
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

import Debug.Trace
debug = flip trace

type FormulaRate = DealStats

data FeeType = AnnualRateFee DealStats FormulaRate
             | PctFee DealStats FormulaRate
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
    [(r0,arrearRemain),(r1,dueRemain)] = paySeqLiabilities amt [fa,fd] -- `debug` ("AMT"++show amt++">> fa"++show fa++"fd"++show fd)
    paid = fa + fd - arrearRemain - dueRemain -- `debug` ("arrear remain "++show arrearRemain++"due remain "++ show dueRemain++"r0 r1"++show r0++show r1)
    newStmt = appendStmt fstmt (ExpTxn d dueRemain paid arrearRemain (PayFee fn)) -- `debug` ("Actual paid to fee"++show paid)

payResidualFee :: Date -> Amount -> Fee -> Fee
payResidualFee d amt f@(Fee fn ft fs fd fdDay fa flpd fstmt) =
   f {feeLastPaidDay = Just d
     ,feeDue = dueRemain
     ,feeArrears = arrearRemain
     ,feeStmt = Just newStmt}
   where
    [(r0,arrearRemain),(r1,dueRemain)] = paySeqLiabilities amt [fa,fd] 
    newStmt = appendStmt fstmt (ExpTxn d dueRemain amt arrearRemain (PayFee fn)) 

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
