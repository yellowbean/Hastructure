{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Waterfall
  (PoolSource(..),Action(..),DistributionSeq(..),CollectionRule(..)
  ,KeepReserve(..),Limit(..),Formula(..))
  where


import Language.Haskell.TH
import Data.Aeson       hiding (json)
import Data.Aeson.TH

import Accounts (Account)
import Asset (Mortgage, Pool)
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

data KeepReserve = TillSource 
                 | TillTarget
             deriving (Show)

data Limit = DuePct Float
            | DueCapAmt Float
            deriving (Show)

data Formula = ABCD
            | OtherFormula String
            deriving (Show)

data Pre = And Pre Pre
         | Or Pre Pre
         deriving (Show)

data Action = Transfer AccountName AccountName (Maybe String)
             | TransferBy AccountName AccountName Formula
             | PayFee [AccountName] [FeeName]
             | PayFeeBy Limit [AccountName] [FeeName]
             | PayInt AccountName [BondName]
             | PayPrin AccountName [BondName]
             | PayTillYield AccountName [BondName]
             | PayResidual AccountName BondName
             | TransferReserve KeepReserve AccountName AccountName (Maybe String)
             deriving (Show)

type DistributionSeq = [Action]

data CollectionRule = Collect PoolSource AccountName
             deriving (Show)



-- $(deriveJSON defaultOptions ''DistSeq)
$(deriveJSON defaultOptions ''PoolSource)
$(deriveJSON defaultOptions ''Action)
$(deriveJSON defaultOptions ''Limit)
$(deriveJSON defaultOptions ''KeepReserve)
$(deriveJSON defaultOptions ''CollectionRule)
$(deriveJSON defaultOptions ''Formula)
$(deriveJSON defaultOptions ''Pre)
