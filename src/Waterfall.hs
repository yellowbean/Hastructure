{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Waterfall
  (PoolSource(..),Action(..),DistributionSeq(..),CollectionRule(..))
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
                | CollectedPrepayment
             deriving (Show)

data Action = Transfer AccountName AccountName
             | PayFee AccountName [FeeName]
             | PayInt AccountName [BondName]
             | PayPrin AccountName [BondName]
             deriving (Show)

type DistributionSeq = [Action]

data CollectionRule = Collect PoolSource AccountName
             deriving (Show)



-- $(deriveJSON defaultOptions ''DistSeq)
$(deriveJSON defaultOptions ''PoolSource)
$(deriveJSON defaultOptions ''Action)
$(deriveJSON defaultOptions ''CollectionRule)
