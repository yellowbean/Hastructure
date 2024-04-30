{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Hedge
  (RateSwap(..),RateCap(..)
  ,RateSwapType(..),RateSwapBase(..)
  ,accrueIRS,payoutIRS,receiveIRS,receiveRC
  ,CurrencySwap(..),rsRefBalLens,SRT(..)
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

import qualified Assumptions as A
import qualified InterestRate as IR
import Control.Lens hiding (Index)

import Debug.Trace
import InterestRate (calcInt)
debug = flip trace

type SettleDates = DatePattern       -- ^ dates when rates/ex-rates are reseted
type ReceiveAmount = Balance         -- ^ cash to be collect in instrutment
type PayoutAmount = Balance          -- ^ cash to be paid in instrutment

data RateSwapBase = Fixed Balance    -- ^ a fixed balance as notional base 
                  | Base DealStats   -- ^ a referece as notional base
                  | Schedule Ts      -- ^ a predfiend schedule of notional balance
                  deriving(Show,Generic,Eq,Ord)

data RateSwapType = FloatingToFloating Floater Floater    -- ^ Paying Floating rate and receiving Floating Rate
                  | FloatingToFixed  Floater IRate        -- ^ Paying Floating Rate and receiving Fixed Rate
                  | FixedToFloating  IRate Floater        -- ^ Paying Fixed Rate and receiving Floating rate
                  deriving(Show,Generic,Eq,Ord)

data RateSwap = RateSwap {rsType :: RateSwapType         -- ^ swap type
                         ,rsSettleDates :: SettleDates   -- ^ define settle dates
                         ,rsNotional :: RateSwapBase     -- ^ define notional balance
                         ,rsStartDate :: StartDate       -- ^ swap start date
                         ,rsPayingRate :: IRate          -- ^ collect rate
                         ,rsReceivingRate :: IRate       -- ^ paying rate
                         ,rsRefBalance :: Balance        -- ^ notional balance in use
                         ,rsLastStlDate :: Maybe Date    -- ^ last settle date
                         ,rsNetCash :: Balance           -- ^ amount to pay/collect
                         ,rsStmt :: Maybe Statement      -- ^ transaction history
                         }
                         deriving(Show,Generic,Eq,Ord)

-- updateRefBalance :: Balance -> RateSwap -> RateSwap
-- updateRefBalance bal rs = rs { rsRefBalance = bal}

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
          newTxn = IrsTxn d face newNetAmount payRate receiveRate newNet SwapAccrue
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
       newStmt = appendStmt stmt $ IrsTxn d 0 actualAmt 0 0 0 SwapOutSettle

instance QueryByComment RateSwap where 
    queryStmt RateSwap{rsStmt = Nothing} tc = []
    queryStmt RateSwap{rsStmt = Just (Statement txns)} tc
      = filter (\x -> getTxnComment x == tc) txns

instance Liable RateSwap where 
  isPaidOff rs@RateSwap{rsNetCash=bal}
    | bal == 0 = True
    | otherwise = False

data RateCap = RateCap {
                rcIndex :: Index
                ,rcStrikeRate :: Ts
                ,rcNotional :: RateSwapBase
                ,rcStartDate :: Date
                ,rcSettleDates :: DatePattern
                ,rcEndDate :: Date
                ,rcReceivingRate :: IRate       -- ^ receiving rate
                ,rcLastStlDate :: Maybe Date    -- ^ last settle date
                ,rcNetCash :: Balance           -- ^ amount to collect
                ,rcStmt :: Maybe Statement      -- ^ transaction history                
              }
              deriving(Show,Generic,Eq,Ord)


receiveRC :: Date -> RateCap -> RateCap
receiveRC d rc@RateCap{rcNetCash = receiveAmt, rcStmt = stmt} 
  | receiveAmt > 0 = rc { rcNetCash = 0 ,rcStmt = newStmt}
  | otherwise = rc
     where 
       newStmt = appendStmt stmt (IrsTxn d 0 receiveAmt 0 0 0 SwapInSettle)

instance IR.UseRate RateCap where 
  getIndexes rc@RateCap{rcIndex = idx} = Just [idx]

instance QueryByComment RateCap where 
    queryStmt RateCap{rcStmt = Nothing} tc = []
    queryStmt RateCap{rcStmt = Just (Statement txns)} tc
      = filter (\x -> getTxnComment x == tc) txns


data CurrencySwap = CurrencySwap {
                    csBalance :: Balance
                    } deriving (Show,Generic,Ord,Eq)

instance IR.UseRate RateSwap where 
  getIndexes rs@RateSwap{rsType = rstype}
    = case rstype of
        FloatingToFloating (idx1,_) (idx2,_) -> Just [idx1,idx2]
        FloatingToFixed (idx1,_) _ -> Just [idx1]
        FixedToFloating _ (idx1,_) -> Just [idx1]
        _ -> Nothing

makeLensesFor [("rsType","rsTypeLens"),("rsRefBalance","rsRefBalLens")] ''RateSwap

data SrtType = SrtByEndDay DealStats DatePattern  -- ^ autu accrue by end of day
              deriving(Show,Generic,Eq,Ord)


data SRT = SRT {
    srtName :: String 
    ,srtType :: SrtType 
    ,srtPremiumType :: IR.RateType              -- ^ define how/when to update the balance
    
    ,srtRefBalance :: Balance                   -- ^ balance to calc premium
    ,srtPremiumRate :: IRate                    -- ^ current interest rated on oustanding balance

    ,srtOpenBalance :: Balance                  -- ^ total open balance
    
    ,srtDuePremiumDate :: Maybe Date            -- ^ last day of interest/premium calculated
    ,srtDuePremium :: Balance                   -- ^ oustanding due on premium
    
    ,srtStart :: Date                           -- ^ when liquidiy provider came into effective
    ,srtEnds :: Maybe Date                      -- ^ when liquidiy provider came into expired
    ,srtStmt :: Maybe Statement                 -- ^ transaction history
} deriving (Show,Generic,Eq,Ord)

instance Liable SRT where 
  isPaidOff srt@SRT{srtOpenBalance=bal,srtDuePremium=duePremium}
    | bal==0 && duePremium==0 = True
    | otherwise = False

instance IR.UseRate SRT where 
  getIndexes srt@SRT{srtPremiumType = rt} 
    = case rt of 
        (IR.Floater _ idx _ _ _ _ _ _ ) -> Just [idx]
        _ -> Nothing
  
  getResetDates srt@SRT{srtPremiumRate = rt , srtStart = sd, srtEnds = ed} 
    = case rt of 
        (IR.Floater _ _ _ _ dp _ _ _ ) -> genSerialDatesTill2 EI sd dp ed
        _ -> []

-- | update the reset events of liquidity provider
buildSrtAccrueAction :: [SRT] -> Date -> [(String, Dates)] -> [(String, Dates)]
buildSrtAccrueAction [] ed r = r
buildSrtAccrueAction (srt:srts) ed r = 
  case srt of 
    (SRT srtName (SrtByEndDay _ dp ) _ _ _ _ _ _ ss _ _ )
      -> buildSrtAccrueAction
           srts
           ed
           [(srtName, projDatesByPattern dp ss ed)]++r
    _ -> buildSrtAccrueAction srts ed r

buildSrtResetAction :: [SRT] -> Date -> [(String, Dates)] -> [(String, Dates)]
buildSrtResetAction [] ed r = r
buildSrtResetAction (srt:srts) ed r = 
  case srt of 
    srt@SRT{srtPremiumType = rt, srtName = ln , srtStart = sd} -> 
       buildSrtResetAction 
        srts 
        ed 
        [(ln,IR.getRateResetDates sd ed rt)]++r
    _ -> buildSrtResetAction srts ed r




$(concat <$> traverse (deriveJSON defaultOptions) [''RateSwap, ''RateCap, ''RateSwapType, ''RateSwapBase, ''CurrencySwap])

-- $(deriveJSON defaultOptions ''RateSwap)
-- $(deriveJSON defaultOptions ''RateCap)
-- $(deriveJSON defaultOptions ''RateSwapType)
-- $(deriveJSON defaultOptions ''RateSwapBase)
-- $(deriveJSON defaultOptions ''CurrencySwap)