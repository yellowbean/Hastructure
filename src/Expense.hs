{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Expense (Fee(..),FeeType(..),payFee,payResidualFee
               ,buildFeeAccrueAction
               ,feeNameLens,feeDueLens,feeTypeLens,feeStmtLens)
  where

import Lib(Period,paySeqLiabilities,Dates
           ,Amount,Balance,Date,Rate,Ts(..))
import Stmt(appendStmt,Statement,TxnComment(..))
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
import DateUtil
import qualified Stmt as S
import qualified InterestRate as IR

import Control.Lens
import Debug.Trace
debug = flip trace

type FormulaRate = DealStats

data FeeType = AnnualRateFee DealStats FormulaRate                       -- ^ annulized fee with a referece
             | PctFee DealStats FormulaRate                              -- ^ fee base on percentage 
             | FixFee Balance                                            -- ^ one-off fee
             | RecurFee DatePattern Balance                              -- ^ fee occur every date pattern
             | NumFee DatePattern DealStats Amount                       -- ^ fee based on an integer number
             | AmtByTbl DatePattern DealStats (Table Balance Balance)    -- ^ lookup query value in a table
             | TargetBalanceFee DealStats DealStats                      -- ^ fee due amount = max( 0, (ds1 - ds2))
             | FeeFlow Ts                                                -- ^ a time series based fee 
             | ByCollectPeriod Amount                                    -- ^ fix amount per collection period
             deriving (Show,Eq, Generic,Ord)

data Fee = Fee {
  feeName :: String              -- ^ fee name
  ,feeType :: FeeType            -- ^ fee type
  ,feeStart :: Date              -- ^ when fee become effective
  ,feeDue :: Balance             -- ^ outstanding due amount fee
  ,feeDueDate :: Maybe Date      -- ^ the date when due amount was calculated
  ,feeArrears :: Balance         -- ^ reserved
  ,feeLastPaidDay :: Maybe Date  -- ^ last paid date
  ,feeStmt :: Maybe Statement    -- ^ transaction history
} deriving (Show,Ord, Eq, Generic)

payFee :: Date   -- ^ When pay action happen
       -> Amount -- ^ Amount paid to fee
       -> Fee    -- ^ Fee before being paid
       -> Fee    -- ^ Fee after paid
payFee d amt f@(Fee fn ft fs fd fdDay fa flpd fstmt) =
   f {feeLastPaidDay = Just d
     ,feeDue = dueRemain
     ,feeArrears = arrearRemain
     ,feeStmt = newStmt}
   where
    [(r0,arrearRemain),(r1,dueRemain)] = paySeqLiabilities amt [fa,fd]
    paid = fa + fd - arrearRemain - dueRemain 
    newStmt = appendStmt (ExpTxn d dueRemain paid arrearRemain (PayFee fn)) fstmt

-- | pay amount of fee regardless the due amount
payResidualFee :: Date -> Amount -> Fee -> Fee
payResidualFee d amt f@(Fee fn ft fs fd fdDay fa flpd fstmt) =
   f {feeLastPaidDay = Just d
     ,feeDue = dueRemain
     ,feeArrears = arrearRemain
     ,feeStmt = newStmt}
   where
    [(r0,arrearRemain),(r1,dueRemain)] = paySeqLiabilities amt [fa,fd] 
    newStmt = appendStmt (ExpTxn d dueRemain amt arrearRemain (PayFee fn)) fstmt  

-- | build accure dates for a fee
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
    (Fee fn (NumFee dp _ _) fs _ _ _ _ _)
      -> buildFeeAccrueAction fees ed [(fn, projDatesByPattern dp fs ed)]++r    
    (Fee fn (AmtByTbl dp _ _) fs _ _ _ _ _)
      -> buildFeeAccrueAction fees ed [(fn, projDatesByPattern dp fs ed)]++r    
    _
      -> buildFeeAccrueAction fees ed r

instance S.QueryByComment Fee where 
    queryStmt Fee{feeStmt = Nothing} tc = []
    queryStmt Fee{feeStmt = Just (S.Statement txns)} tc
      = filter (\x -> S.getTxnComment x == tc) txns

instance Liable Fee where 
  isPaidOff f@Fee{feeDue=bal,feeArrears=fa}
    | bal==0 && fa==0 = True 
    | otherwise = False

instance IR.UseRate Fee where
  isAdjustbleRate x = False
  getIndex x = Nothing 

makeLensesFor [("feeName","feeNameLens"),("feeType","feeTypeLens") ,("feeDue","feeDueLens") ,("feeDueDate","feeDueDateLens") ,("feeStmt","feeStmtLens")] ''Fee

$(deriveJSON defaultOptions ''FeeType)
$(deriveJSON defaultOptions ''Fee)
