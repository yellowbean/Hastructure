{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Waterfall
  (PoolSource(..),Action(..),DistributionSeq(..),CollectionRule(..)
  ,KeepReserve(..),Limit(..),Formula(..),ActionWhen(..))
  where

import GHC.Generics
import Language.Haskell.TH
import Data.Aeson hiding (json)
import qualified Data.Text as T
import Text.Read (readMaybe)
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Hashable

import Accounts (Account)
import Asset (Mortgage, Pool)
import Expense
import Liability
import qualified Lib as L


type FeeName = String
type BondName = String
type AccountName = String

data ActionWhen = EndOfPoolCollection
                | DistributionDay
                | CleanUp
                deriving (Show,Ord,Eq,Generic,Read)

instance ToJSONKey ActionWhen where
  toJSONKey = toJSONKeyText (T.pack . show)

instance FromJSONKey ActionWhen where
  fromJSONKey = FromJSONKeyTextParser $ \t -> case readMaybe (T.unpack t) of
    Just k -> pure k
    Nothing -> fail ("Invalid key: " ++ show t)

$(deriveJSON defaultOptions ''ActionWhen)

data PoolSource = CollectedInterest
                | CollectedPrincipal
                | CollectedRecoveries
                | CollectedPrepayment
                deriving (Show)

data KeepReserve = TillSource 
                 | TillTarget
                 deriving (Show)

data Limit = DuePct Float  -- due fee
            | DueCapAmt Float  -- due fee
            | RemainBalPct L.DealStats Float -- pay till remain balance equals to a percentage of `stats`

            deriving (Show)

data Formula = ABCD
            | Sum L.DealStats
            | OtherFormula String
            deriving (Show)

data Pre = And Pre Pre
         | Or Pre Pre
         | IfZero L.DealStats
         deriving (Show)

data Action = Transfer AccountName AccountName (Maybe String)
             | TransferBy AccountName AccountName Formula
             | PayFee [AccountName] [FeeName]
             | PayFeeBy Limit [AccountName] [FeeName]
             | PayInt AccountName [BondName]
             | PayPrin AccountName [BondName]
             | PayPrinBy Limit AccountName BondName
             | PayTillYield AccountName [BondName]
             | PayResidual AccountName BondName
             | TransferReserve KeepReserve AccountName AccountName (Maybe String)
             deriving (Show)

type DistributionSeq = [Action]

data CollectionRule = Collect PoolSource AccountName
             deriving (Show)


$(deriveJSON defaultOptions ''PoolSource)
$(deriveJSON defaultOptions ''Action)
$(deriveJSON defaultOptions ''Limit)
$(deriveJSON defaultOptions ''KeepReserve)
$(deriveJSON defaultOptions ''CollectionRule)
$(deriveJSON defaultOptions ''Formula)
$(deriveJSON defaultOptions ''Pre)
