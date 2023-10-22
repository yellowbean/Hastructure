{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Hedge
  (RateSwap(..)
  ,RateSwapType(..),RateSwapBase(..)
  ,accrueIRS,payoutIRS,receiveIRS
  ,CurrencySwap(..),updateRefBalance
  )
  where

import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Data.Map as Map
import GHC.Generics
import Language.Haskell.TH
import Data.Aeson hiding (json)
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Fixed
import Data.Maybe
import Types
import Util
import Stmt
import DateUtil

import qualified InterestRate as IR

import Debug.Trace
debug = flip trace

type SettleDates = DatePattern       -- ^ dates when rates/ex-rates are reseted
type ReceiveAmount = Balance         -- ^ cash to be collect in instrutment
type PayoutAmount = Balance          -- ^ cash to be paid in instrutment

data RateSwapBase = Fixed Balance    -- ^ a fixed balance as notional base 
                  | Base DealStats   -- ^ a referece as notional base
                  | Schedule Ts      -- ^ a predfiend schedule of notional balance
                  deriving(Show,Generic)

data RateSwap = RateSwap {rsType :: RateSwapType         -- ^ swap type
                         ,rsSettleDates :: SettleDates   -- ^ define settle dates
                         ,rsNotional :: RateSwapBase     -- ^ define notional balance
                         ,rsStartDate :: StartDate       -- ^ swap start date
                         ,rsPayingRate :: IRate          -- ^ collect rate
                         ,rsReceivingRate :: IRate       -- ^ paying rate
                         ,rsRefBalance :: Balance        -- ^ notional balance in use
                         ,rsLastStlDate :: Maybe Date    -- ^ last  settle date
                         ,rsNetCash :: Balance           -- ^ amount to pay/collect
                         ,rsStmt :: Maybe Statement      -- ^ transaction history
                         }
                         deriving(Show,Generic)

updateRefBalance :: Balance -> RateSwap -> RateSwap
updateRefBalance bal rs = rs { rsRefBalance = bal}

-- | The `accrueIRS` will calculate the `Net` amount 
-- ( payble with negative, positve with receivable) of Rate Swap      
accrueIRS :: Date -> RateSwap -> RateSwap
accrueIRS d rs@RateSwap{rsRefBalance = face               
                      , rsPayingRate = payRate            
                      , rsReceivingRate = receiveRate     
                      , rsNetCash = netCash               
                      , rsStmt = stmt}                    
  =  rs {rsNetCash = newNet , rsLastStlDate = Just d, rsStmt = newStmt }
      where 
          accureStartDate =  case rsLastStlDate rs of 
                               Nothing ->  rsStartDate rs 
                               Just lsd -> lsd
          rateDiff =  receiveRate - payRate 
          yearFactor = fromRational $ yearCountFraction DC_ACT_365F accureStartDate d
          newNetAmount = mulBIR (face * yearFactor) rateDiff  -- `debug` ("Diff rate"++ show rateDiff)
          newNet = netCash + newNetAmount
          newTxn = IrsTxn d face newNetAmount payRate receiveRate newNet SwapAccure
          newStmt = appendStmt stmt newTxn

-- | set rate swap to state of receive all cash from counterparty
receiveIRS :: Date -> RateSwap -> RateSwap 
receiveIRS d rs@RateSwap{rsNetCash = receiveAmt, rsStmt = stmt} 
  | receiveAmt > 0 = rs { rsNetCash = 0 ,rsStmt = newStmt}
  | otherwise = rs
     where 
       newStmt = appendStmt stmt (IrsTxn d 0 receiveAmt 0 0 0 SwapInSettle)

-- | set rate swap to state of payout all possible cash to counterparty
payoutIRS :: Date -> Amount -> RateSwap -> RateSwap 
payoutIRS d amt rs@RateSwap{rsNetCash = payoutAmt, rsStmt = stmt} 
  | payoutAmt < 0  =  rs { rsNetCash = outstanding, rsStmt = newStmt }
  | otherwise = rs
     where 
       actualAmt = min amt (negate payoutAmt)  --TODO need to add a check here
       outstanding = payoutAmt + actualAmt
       newTxn = IrsTxn d 0 actualAmt 0 0 0 SwapOutSettle
       newStmt = appendStmt stmt newTxn

instance QueryByComment RateSwap where 
    queryStmt RateSwap{rsStmt = Nothing} tc = []
    queryStmt RateSwap{rsStmt = Just (Statement txns)} tc
      = filter (\x -> getTxnComment x == tc) txns

instance Liable RateSwap where 
  isPaidOff rs@RateSwap{rsNetCash=bal}
    | bal == 0 = True
    | otherwise = False

data RateSwapType = FloatingToFloating Floater Floater    -- ^ Paying Floating rate and receiving Floating Rate
                  | FloatingToFixed  Floater IRate        -- ^ Paying Floating Rate and receiving Fixed Rate
                  | FixedToFloating  IRate Floater        -- ^ Paying Fixed Rate and receiving Floating rate
                  deriving(Show,Generic)

data CurrencySwap = CurrencySwap {
                    csBalance :: Balance
                    } deriving (Show,Generic)

instance IR.UseRate RateSwap where 
  getIndexes rs@RateSwap{rsType = rstype}
    = case rstype of
        FloatingToFloating (idx1,_) (idx2,_) -> Just [idx1,idx2]
        FloatingToFixed (idx1,_) _ -> Just [idx1]
        FixedToFloating _ (idx1,_) -> Just [idx1]
        _ -> Nothing


$(deriveJSON defaultOptions ''RateSwap)
$(deriveJSON defaultOptions ''RateSwapType)
$(deriveJSON defaultOptions ''RateSwapBase)
$(deriveJSON defaultOptions ''CurrencySwap)