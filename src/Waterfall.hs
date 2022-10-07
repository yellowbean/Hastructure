{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Waterfall
  (PoolSource(..),Action(..),DistributionSeq(..),CollectionRule(..)
  ,Satisfy(..),Limit(..),Formula(..),ActionWhen(..))
  where

import GHC.Generics
import Language.Haskell.TH
import Data.Aeson hiding (json)
import qualified Data.Text as T
import Text.Read (readMaybe)
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Hashable
import Data.Fixed

import Accounts (Account)
import Asset (Mortgage, Pool)
import Expense
import Liability
import Types
import qualified Lib as L
import qualified Call as C


data ActionWhen = EndOfPoolCollection
                | DistributionDay DealStatus
                | CleanUp
                deriving (Show,Ord,Eq,Generic,Read)

instance ToJSONKey ActionWhen where
  toJSONKey = toJSONKeyText (T.pack . show)

instance FromJSONKey ActionWhen where
  fromJSONKey = FromJSONKeyTextParser $ \t -> case readMaybe (T.unpack t) of
    Just k -> pure k
    Nothing -> fail ("Invalid key: " ++ show t)


data PoolSource = CollectedInterest
                | CollectedPrincipal
                | CollectedRecoveries
                | CollectedPrepayment
                | CollectedRental
                | CollectedFee
                deriving (Show)

data Satisfy = Source
             | Target
             deriving (Show)

data Limit = DuePct L.Balance  -- due fee
            | DueCapAmt L.Balance  -- due fee
            | RemainBalPct L.Rate -- pay till remain balance equals to a percentage of `stats`
            | KeepBalAmt DealStats -- pay till a certain amount remains in an account
            | Multiple Limit Float -- factor of a limit:w
            deriving (Show)

data Formula = ABCD          -- short cuts to complex arthimics on deal stats
            | Sum DealStats
            | OtherFormula String
            deriving (Show)



data Action = Transfer AccountName AccountName (Maybe String)
             | TransferBy AccountName AccountName Formula
             | CalcFee [FeeName]
             | PayFee [AccountName] [FeeName]
             | PayFeeBy Limit [AccountName] [FeeName]
             | PayFeeResidual (Maybe Limit) AccountName FeeName
             | PayInt AccountName [BondName]
             | PayPrin AccountName [BondName]
             | PayPrinResidual AccountName [BondName]
             | PayPrinBy Limit AccountName BondName
             | PayTillYield AccountName [BondName]
             | PayResidual (Maybe Limit) AccountName BondName
             | TransferReserve Satisfy AccountName AccountName (Maybe String)
             | LiquidatePool C.LiquidationMethod AccountName
             | RunTrigger (Maybe [Trigger])
             deriving (Show)

--type DistributionSeq = [Action]
type DistributionSeq = [(Maybe L.Pre, Action)]

--data DistributionSeq = DistSeq [Action]
--                     | DistSeqPre [(Maybe Pre,Action)]
--                     deriving (Show)

data CollectionRule = Collect PoolSource AccountName
                    deriving (Show)


$(deriveJSON defaultOptions ''PoolSource)
$(deriveJSON defaultOptions ''Action)
$(deriveJSON defaultOptions ''Limit)
$(deriveJSON defaultOptions ''Satisfy)
$(deriveJSON defaultOptions ''CollectionRule)
$(deriveJSON defaultOptions ''Formula)
$(deriveJSON defaultOptions ''ActionWhen)