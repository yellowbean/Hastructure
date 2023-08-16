{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Hedge
  (RateSwap(..),CurrencySwap(..)
  ,RateSwapType(..),RateSwapBase(..)
  ,accrueIRS,payoutIRS,receiveIRS
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

data RateSwapBase = Fixed Balance
                  | Base DealStats
                  | Schedule Ts
                  deriving(Show,Generic)

data RateSwap = RateSwap {rsType :: RateSwapType
                         ,rsSettleDates :: SettleDates
                         ,rsNotional :: RateSwapBase
                         ,rsStartDate :: StartDate
                         ,rsPayingRate :: IRate
                         ,rsReceivingRate :: IRate
                         ,rsRefBalance :: Balance
                         ,rsLastStlDate :: Maybe Date
                         ,rsNetCash :: Balance
                         ,rsStmt :: Maybe Statement}
                         deriving(Show,Generic)
              
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
          yearFactor = fromRational $ yearCountFraction DC_ACT_365 accureStartDate d
          newNetAmount = mulBIR (face * yearFactor) rateDiff 
          newNet = netCash + newNetAmount
          newTxn = IrsTxn d face newNetAmount payRate receiveRate newNet SwapAccure
          newStmt = case stmt of 
                      Nothing -> Just $ Statement [newTxn]
                      Just (Statement txns) -> Just $ Statement $ txns++[newTxn]

receiveIRS :: Date -> RateSwap -> RateSwap 
receiveIRS d rs@RateSwap{rsNetCash = receiveAmt, rsStmt = stmt} 
  | receiveAmt > 0 = rs { rsNetCash = 0 }
  | otherwise = rs
     where 
       newTxn = IrsTxn d 0 receiveAmt 0 0 0 SwapInSettle

payoutIRS :: Date -> Amount -> RateSwap -> RateSwap 
payoutIRS d amt rs@RateSwap{rsNetCash = payoutAmt, rsStmt = stmt} 
  | payoutAmt < 0  =  rs { rsNetCash = outstanding }
  | otherwise = rs
     where 
       actualAmt = min amt (negate payoutAmt)  --TODO need to add a check here
       outstanding = negate payoutAmt - amt
       newTxn = IrsTxn d 0 actualAmt 0 0 0 SwapOutSettle

data CurrencySwap = CurrencySwap Rate Balance
                  | Dummy3
                  deriving(Show,Generic)
              
instance QueryByComment RateSwap where 
    queryStmt RateSwap{rsStmt = Nothing} tc = []
    queryStmt RateSwap{rsStmt = Just (Statement txns)} tc
      = filter (\x -> getTxnComment x == tc) txns

type SettleDates = DatePattern
type Notional = Balance

data RateSwapType = FloatingToFloating Floater Floater   -- Paying Floating rate and receiving Floating Rate
                  | FloatingToFixed  Floater IRate        -- Paying Floating Rate and receiving Fixed Rate
                  | FixedToFloating  IRate Floater        -- Paying Fixed Rate and receiving Floating rate
                  deriving(Show,Generic)

type ReceiveAmount = Balance
type PayoutAmount = Balance


$(deriveJSON defaultOptions ''RateSwap)
$(deriveJSON defaultOptions ''RateSwapType)
$(deriveJSON defaultOptions ''RateSwapBase)
$(deriveJSON defaultOptions ''CurrencySwap)