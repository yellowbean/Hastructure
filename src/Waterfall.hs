{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Waterfall
  (PoolSource(..),Action(..),DistributionSeq(..),CollectionRule(..)
  ,ActionWhen(..),BookType(..),ExtraSupport(..))
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
import Ledger
import Stmt (TxnComment(..))
import qualified Lib as L
import qualified Call as C
import qualified CreditEnhancement as CE
import CreditEnhancement (LiquidityProviderName)
import Ledger (Ledger)


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


data BookType = PDL DealStats [(LedgerName,DealStats)] --Reverse PDL Debit reference, [(name,cap reference)]
              | ByAccountDraw LedgerName
              | ByDS          LedgerName DealStats 
              deriving (Show,Generic)

data ExtraSupport = SupportAccount AccountName (Maybe BookType)
                  | SupportLiqFacility LiquidityProviderName
                  | MultiSupport [ExtraSupport]
                  deriving (Show,Generic)

data Action = Transfer (Maybe Limit) AccountName AccountName (Maybe TxnComment)
            -- Fee
            | CalcFee [FeeName]
            | PayFee (Maybe Limit) AccountName [FeeName] (Maybe ExtraSupport)
            | CalcAndPayFee (Maybe Limit) AccountName [FeeName] (Maybe ExtraSupport)
            | PayFeeResidual (Maybe Limit) AccountName FeeName 
            -- Bond - Interest
            | CalcBondInt [BondName]
            | PayInt (Maybe Limit) AccountName [BondName] (Maybe ExtraSupport)
            | AccrueAndPayInt (Maybe Limit) AccountName [BondName] (Maybe ExtraSupport)
            | PayIntResidual (Maybe Limit) AccountName BondName
            -- | PayTillYield AccountName [BondName]
            -- Bond - Principal
            | PayPrin (Maybe Limit) AccountName [BondName] (Maybe ExtraSupport) 
            | PayPrinResidual AccountName [BondName]
            -- | PayPrinBy Limit AccountName BondName
            -- Pool/Asset change
            | BuyAsset (Maybe Limit) PricingMethod AccountName
            | LiquidatePool PricingMethod AccountName
            -- Liquidation support
            | LiqSupport (Maybe Limit) CE.LiquidityProviderName AccountName
            | LiqPayFee (Maybe Limit) CE.LiquidityProviderName FeeName
            | LiqPayBond (Maybe Limit) CE.LiquidityProviderName BondName
            | LiqRepay (Maybe Limit) CE.LiqRepayType AccountName CE.LiquidityProviderName 
            | LiqYield (Maybe Limit) AccountName CE.LiquidityProviderName 
            | LiqAccrue CE.LiquidityProviderName 
            -- Swap
            | SwapAccrue CeName                 -- ^ calculate the net amount of swap
            | SwapReceive AccountName CeName    -- ^ receive amount from net amount of swap and deposit to account
            | SwapPay AccountName CeName        -- ^ pay out net amount from account 
            | SwapSettle AccountName CeName     -- ^ pay & receive net amount of swap with account
            -- Record booking
            | BookBy BookType
            -- Pre
            | ActionWithPre L.Pre [Action]            -- ^ execute actions if <pre> is true 
            | ActionWithPre2 L.Pre [Action] [Action]  -- ^ execute action1 if <pre> is true ,else execute action2 
            -- Trigger
            | RunTrigger DealCycle Int
            -- Debug
            | WatchVal (Maybe String) [DealStats]
            deriving (Show,Generic)

type DistributionSeq = [Action]

data CollectionRule = Collect PoolSource AccountName
                    | CollectByPct PoolSource [(Rate,AccountName)]
                    deriving (Show,Generic)


$(deriveJSON defaultOptions ''Action)
$(deriveJSON defaultOptions ''CollectionRule)
$(deriveJSON defaultOptions ''ActionWhen)
$(deriveJSON defaultOptions ''BookType)
$(deriveJSON defaultOptions ''ExtraSupport)
