{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Waterfall
  (PoolSource(..),Action(..),DistributionSeq(..),CollectionRule(..)
  ,Satisfy(..),Limit(..),ActionWhen(..),FormulaType(..))
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
import Asset (Pool)
import AssetClass.Mortgage(Mortgage)
import Expense
import Liability
import Types
import Revolving
import Stmt (TxnComment(..))
import qualified Lib as L
import qualified Call as C
import qualified CreditEnhancement as CE


data ActionWhen = EndOfPoolCollection
                | DistributionDay DealStatus
                | CleanUp
                | OnClosingDay
                deriving (Show,Ord,Eq,Generic,Read)

instance ToJSONKey ActionWhen where
  toJSONKey = toJSONKeyText (T.pack . show)

instance FromJSONKey ActionWhen where
  fromJSONKey = FromJSONKeyTextParser $ \t -> case readMaybe (T.unpack t) of
    Just k -> pure k
    Nothing -> fail ("Invalid key: " ++ show t++">>"++ show (T.unpack t))

data Satisfy = Source
             | Target
             deriving (Show)

data Limit = DuePct L.Balance  --
           | DueCapAmt L.Balance  -- due fee
           | RemainBalPct L.Rate -- pay till remain balance equals to a percentage of `stats`
           | KeepBalAmt DealStats -- pay till a certain amount remains in an account
           | Multiple Limit Float -- factor of a limit:w
           | Formula FormulaType
           | DS DealStats
           deriving (Show)

data Action = Transfer AccountName AccountName 
            | TransferBy Limit AccountName AccountName
            | CalcFee [FeeName]
            | CalcBondInt [BondName]
            | PayFee [AccountName] [FeeName]
            | PayFeeBy Limit [AccountName] [FeeName]
            | PayFeeResidual (Maybe Limit) AccountName FeeName
            | PayInt AccountName [BondName]
            | PayPrin AccountName [BondName]
            | PayPrinResidual AccountName [BondName]
            | PayPrinBy Limit AccountName BondName
            | PayTillYield AccountName [BondName]
            | PayResidual (Maybe Limit) AccountName BondName
            | TransferReserve Satisfy AccountName AccountName 
            | LiquidatePool LiquidationMethod AccountName
            | RunTrigger (Maybe [Trigger])
            | BuyAsset (Maybe Limit) LiquidationMethod AccountName
            | LiqSupport (Maybe Limit) CE.LiquidityProviderName AccountName
            | LiqPayFee (Maybe Limit) CE.LiquidityProviderName FeeName
            | LiqPayBond (Maybe Limit) CE.LiquidityProviderName BondName
            | LiqRepay (Maybe Limit) AccountName CE.LiquidityProviderName 
            | LiqYield (Maybe Limit) AccountName CE.LiquidityProviderName 
            | LiqAccrue CE.LiquidityProviderName 
            deriving (Show)

type DistributionSeq = [(Maybe L.Pre, Action)]

data CollectionRule = Collect PoolSource AccountName
                     deriving (Show)


$(deriveJSON defaultOptions ''Action)
$(deriveJSON defaultOptions ''Limit)
$(deriveJSON defaultOptions ''Satisfy)
$(deriveJSON defaultOptions ''CollectionRule)
$(deriveJSON defaultOptions ''ActionWhen)
