{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Waterfall
  (PoolSource(..),Action(..),DistributionSeq(..),CollectionRule(..)
  ,ActionWhen(..),BookType(..),ExtraSupport(..))
  where

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
import CreditEnhancement (LiquidityProviderName)
import Ledger (Ledger,LedgerName)


data ActionWhen = EndOfPoolCollection             -- ^ waterfall executed at the end of pool collection
                | DistributionDay DealStatus      -- ^ waterfall executed depends on deal status
                | CleanUp                         -- ^ waterfall exectued upon a clean up call
                | OnClosingDay                    -- ^ waterfall executed on deal closing day
                | DefaultDistribution             -- ^ default waterfall executed
                | RampUp                          -- ^ ramp up
                deriving (Show,Ord,Eq,Generic,Read)

instance ToJSONKey ActionWhen where
  toJSONKey = toJSONKeyText (T.pack . show)

instance FromJSONKey ActionWhen where
  fromJSONKey = FromJSONKeyTextParser $ \t -> case readMaybe (T.unpack t) of
    Just k -> pure k
    Nothing -> fail ("Invalid key: " ++ show t++">>"++ show (T.unpack t))


data BookType = PDL DealStats [(LedgerName,DealStats)] -- Reverse PDL Debit reference, [(name,cap reference)]
              | ByAccountDraw LedgerName               -- Book amount equal to account draw amount
              | ByDS          LedgerName Direction DealStats     -- Book amount equal to a formula/deal stats
              deriving (Show,Generic)

data ExtraSupport = SupportAccount AccountName (Maybe BookType)  -- ^ if there is deficit, draw another account to pay the shortfall
                  | SupportLiqFacility LiquidityProviderName     -- ^ if there is deficit, draw facility's available credit to pay the shortfall
                  | MultiSupport [ExtraSupport]                  -- ^ if there is deficit, draw multiple supports (by sequence in the list) to pay the shortfall
                  | WithCondition Pre ExtraSupport               -- ^ support only available if Pre is true
                  deriving (Show,Generic)

data Action = Transfer (Maybe Limit) AccountName AccountName (Maybe TxnComment)
            -- Fee
            | CalcFee [FeeName]                                                            -- ^ calculate fee due amount in the fee names
            | PayFee (Maybe Limit) AccountName [FeeName] (Maybe ExtraSupport)              -- ^ pay fee with cash from account with optional limit or extra support
            | CalcAndPayFee (Maybe Limit) AccountName [FeeName] (Maybe ExtraSupport)       -- ^ combination of CalcFee and PayFee
            | PayFeeResidual (Maybe Limit) AccountName FeeName                             -- ^ pay fee regardless fee due amount
            -- Bond - Interest
            | CalcBondInt [BondName]                                                       -- ^ calculate interest due amount in the bond names
            | PayInt (Maybe Limit) AccountName [BondName] (Maybe ExtraSupport)             -- ^ pay interest with cash from the account with optional limit or extra support
            | PayIntBySeq (Maybe Limit) AccountName [BondName] (Maybe ExtraSupport)        -- ^ with sequence
            | AccrueAndPayInt (Maybe Limit) AccountName [BondName] (Maybe ExtraSupport)    -- ^ combination of CalcInt and PayInt
            | AccrueAndPayIntBySeq (Maybe Limit) AccountName [BondName] (Maybe ExtraSupport) -- ^ with sequence
            | PayIntResidual (Maybe Limit) AccountName BondName                            -- ^ pay interest to bond regardless interest due
            -- | PayTillYield AccountName [BondName]
            -- Bond - Principal
            | PayPrin (Maybe Limit) AccountName [BondName] (Maybe ExtraSupport)             -- ^ pay principal to bond via pro-rata
            | PayPrinBySeq (Maybe Limit) AccountName [BondName] (Maybe ExtraSupport)        -- ^ pay principal to bond via sequence
            | PayPrinResidual AccountName [BondName]                                        -- ^ pay principal regardless predefined balance schedule
            | PayIntPrinBySeq (Maybe Limit) AccountName [BondName] (Maybe ExtraSupport)     -- ^ pay int & prin to bonds sequentially
            | AccrueAndPayIntPrinBySeq (Maybe Limit) AccountName [BondName] (Maybe ExtraSupport) 
            -- Pool/Asset change
            | BuyAsset (Maybe Limit) PricingMethod AccountName                              -- ^ buy asset from revolving assumptions using funds from account
            | LiquidatePool PricingMethod AccountName                                       -- ^ sell all assets and deposit proceeds to account
            -- Liquidation support
            | LiqSupport (Maybe Limit) CE.LiquidityProviderName CE.LiqDrawType AccountName  -- ^ draw credit and deposit to account/fee/bond interest/principal
            | LiqRepay (Maybe Limit) CE.LiqRepayType AccountName CE.LiquidityProviderName   -- ^ repay liquidity facility
            | LiqYield (Maybe Limit) AccountName CE.LiquidityProviderName                   -- ^ repay compensation to liquidity facility
            | LiqAccrue CE.LiquidityProviderName                                            -- ^ accure premium/due interest of liquidity facility
            -- Swap
            | SwapAccrue CeName                 -- ^ calculate the net amount of swap
            | SwapReceive AccountName CeName    -- ^ receive amount from net amount of swap and deposit to account
            | SwapPay AccountName CeName        -- ^ pay out net amount from account 
            | SwapSettle AccountName CeName     -- ^ pay & receive net amount of swap with account
            -- RateCap 
            | CollectRateCap AccountName CeName  -- ^ collect cash from rate cap and deposit to account
            -- Record booking
            | BookBy BookType                         -- ^ book an ledger with book types
            -- Pre
            | ActionWithPre L.Pre [Action]            -- ^ execute actions if <pre> is true 
            | ActionWithPre2 L.Pre [Action] [Action]  -- ^ execute action1 if <pre> is true ,else execute action2 
            -- Trigger
            | RunTrigger DealCycle TriggerName        -- ^ update the trigger status during the waterfall execution
            -- Debug
            | WatchVal (Maybe String) [DealStats]     -- ^ inspect vals during the waterfall execution
            deriving (Show,Generic)

type DistributionSeq = [Action]

data CollectionRule = Collect PoolSource AccountName                   -- ^ collect a pool source from pool collection and deposit to an account
                    | CollectByPct PoolSource [(Rate,AccountName)]     -- ^ collect a pool source from pool collection and deposit to multiple accounts with percentages
                    deriving (Show,Generic)


$(deriveJSON defaultOptions ''Action)
$(deriveJSON defaultOptions ''CollectionRule)
$(deriveJSON defaultOptions ''ActionWhen)
$(deriveJSON defaultOptions ''BookType)
$(deriveJSON defaultOptions ''ExtraSupport)
