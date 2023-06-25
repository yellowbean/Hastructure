{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Waterfall
  (PoolSource(..),Action(..),DistributionSeq(..),CollectionRule(..)
  ,Satisfy(..),Limit(..),ActionWhen(..))
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
import GHC.Generics

import Accounts (Account)
import Asset (Pool)
-- import AssetClass.Mortgage(Mortgage)
import Expense
import Liability
import Types
import Revolving
import Triggers
import Stmt (TxnComment(..))
import qualified Lib as L
import qualified Call as C
import qualified CreditEnhancement as CE


data ActionWhen = EndOfPoolCollection
                | DistributionDay DealStatus
                | CleanUp
                | OnClosingDay
                | DefaultDistribution
                deriving (Show,Ord,Eq,Generic,Read)

instance ToJSONKey ActionWhen where
  toJSONKey = toJSONKeyText (T.pack . show)

instance FromJSONKey ActionWhen where
  fromJSONKey = FromJSONKeyTextParser $ \t -> case readMaybe (T.unpack t) of
    Just k -> pure k
    Nothing -> fail ("Invalid key: " ++ show t++">>"++ show (T.unpack t))

data Satisfy = Source
             | Target
             deriving (Show,Generic)

data Limit = DuePct L.Balance  --
           | DueCapAmt L.Balance  -- due fee
           | RemainBalPct L.Rate -- pay till remain balance equals to a percentage of `stats`
           | KeepBalAmt DealStats -- pay till a certain amount remains in an account
           | Multiple Limit Float -- factor of a limit:w
           | Formula FormulaType
           | DS DealStats
           deriving (Show,Generic)

data Action = Transfer AccountName AccountName 
            | TransferBy Limit AccountName AccountName
            | CalcFee [FeeName]
            | CalcBondInt [BondName]
            | PayFee [AccountName] [FeeName]
            | PayFeeBy Limit [AccountName] [FeeName]
            | PayFeeResidual (Maybe Limit) AccountName FeeName
            | AccrueAndPayInt AccountName [BondName]
            | PayInt AccountName [BondName]
            | PayPrin AccountName [BondName]
            | PayPrinResidual AccountName [BondName]
            | PayPrinBy Limit AccountName BondName
            | PayTillYield AccountName [BondName]
            | PayResidual (Maybe Limit) AccountName BondName
            | TransferReserve Satisfy AccountName AccountName 
            | LiquidatePool PricingMethod AccountName
            -- | RunTrigger (Maybe [Trigger])
            | LiqSupport (Maybe Limit) CE.LiquidityProviderName AccountName
            | LiqPayFee (Maybe Limit) CE.LiquidityProviderName FeeName
            | LiqPayBond (Maybe Limit) CE.LiquidityProviderName BondName
            | LiqRepay (Maybe Limit) CE.LiqRepayType AccountName CE.LiquidityProviderName 
            | LiqYield (Maybe Limit) AccountName CE.LiquidityProviderName 
            | LiqAccrue CE.LiquidityProviderName 
            | SwapAccrue CeName
            | SwapReceive AccountName CeName
            | SwapPay AccountName CeName
            | BuyAsset (Maybe Limit) PricingMethod AccountName
            | ActionWithPre L.Pre [Action] 
            | ActionWithPre2 L.Pre [Action] [Action]
            | RunTrigger DealCycle Int
            deriving (Show,Generic)

type DistributionSeq = [Action]

data CollectionRule = Collect PoolSource AccountName
                    | CollectByPct PoolSource [(Rate,AccountName)]
                    deriving (Show,Generic)


$(deriveJSON defaultOptions ''Action)
$(deriveJSON defaultOptions ''Limit)
$(deriveJSON defaultOptions ''Satisfy)
$(deriveJSON defaultOptions ''CollectionRule)
$(deriveJSON defaultOptions ''ActionWhen)