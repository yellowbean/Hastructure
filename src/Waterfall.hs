{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Waterfall
  (PoolSource(..),Action(..),DistributionSeq(..))
  where


import Language.Haskell.TH
import           Data.Aeson       hiding (json)
import           Data.Aeson.TH

import Accounts (Account)
import Asset (Mortgage, Pool)
import Equity
import Expense
import Liability

type FeeName = String
type BondName = String
type AccountName = String

data PoolSource = CollectedInterest
                | CollectedPrincipal
                | CollectedRecoveries
             deriving (Show)

data Action = Transfer AccountName AccountName
             | Collect PoolSource AccountName
             | PayFee AccountName [FeeName]
             | PayInt AccountName [BondName]
             | PayPrin AccountName [BondName]
             deriving (Show)

type DistributionSeq = [Action]

-- $(deriveJSON defaultOptions ''DistSeq)
$(deriveJSON defaultOptions ''PoolSource)
$(deriveJSON defaultOptions ''Action)
