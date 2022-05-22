{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Expense (Fee(..),FeeType(..),FeeBase(..),payFee)
  where

import Lib(DayCount,Period,paySeqLiabilities)
import Data.Traversable
import Language.Haskell.TH

import qualified Data.Time as T
import qualified Data.Text
import           Data.Aeson       hiding (json)
import           Data.Aeson.TH
import Data.Aeson.Types
import Control.Monad (mzero)
import qualified Data.ByteString.Lazy.Char8 as L

data FeeBase = PoolInt
              | CurrentBondBalance
              | CurrentPoolBalance
              deriving (Show)


data FeeType = AnnualRateFee FeeBase Float
              |PctFee FeeBase Float
              |FixFee Float
              |RecurFee Period Float
              deriving (Show)


--calcFee :: FeeType -> T.Day ->
data Fee = Fee {
  feeName :: String
  ,feeType :: FeeType
  ,feeStart :: T.Day
  ,feeDue :: Float
  ,feeArrears :: Float
  ,feeLastPaidDay :: Maybe T.Day
} deriving (Show)

payFee :: T.Day -> Float -> Fee -> Fee
payFee d amt f@(Fee fn ft fs fd fa flpd) =
   f {feeLastPaidDay = Just d
     ,feeDue = dueRemain
     ,feeArrears = arrearRemain}
   where
    [(r0,arrearRemain),(r1,dueRemain)] = paySeqLiabilities amt [fa,fd]



$(deriveJSON defaultOptions ''FeeBase)
$(deriveJSON defaultOptions ''FeeType)
$(deriveJSON defaultOptions ''Fee)
