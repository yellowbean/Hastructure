{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module CreditEnhancement
  (LiqFacility(..),LiqSupportType(..),buildLiqResetAction
  ,LiquidityProviderName,draw,repay,LiqSupportRate(..)
  ,RateSwap(..),LiqRepayType(..),CurrencySwap(..)
  ,RateSwapType(..)
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

type LiquidityProviderName = String

data LiqSupportType = ReplenishSupport DatePattern Balance
                    | FixSupport
                    | ByPct DatePattern DealStats Rate
                    | UnLimit
                    deriving(Show)

type LastAccDate =  Date 
data LiqSupportRate = FixRate DatePattern Rate (Maybe LastAccDate)
                    | Dummy 
                    deriving(Show)

data LiqFacility = LiqFacility {
    liqName :: String 
    ,liqType :: LiqSupportType 
    ,liqBalance :: Maybe Balance  -- available balance to support. Nothing -> unlimit 
    ,liqCredit :: Balance  -- total support balance supported
    ,liqStart :: Date
    ,liqDueInt :: Maybe Balance
    ,liqDuePremium :: Maybe Balance
    ,liqRate :: Maybe LiqSupportRate
    ,liqPremium :: Maybe LiqSupportRate
    ,liqStmt :: Maybe Statement
} deriving (Show)


buildLiqResetAction :: [LiqFacility] -> Date -> [(String, Dates)] -> [(String, Dates)]
buildLiqResetAction [] ed r = r
buildLiqResetAction (liqProvider:liqProviders) ed r = 
  case liqProvider of 
    (LiqFacility lqName (ReplenishSupport dp bal) _ _ ss _ _ _ _ stmt)
      -> buildLiqResetAction
           liqProviders
           ed
           [(lqName, projDatesByPattern dp ss ed)]++r
    (LiqFacility lqName (ByPct dp ds pct) _ _ ss _ _ _ _ stmt)
      -> buildLiqResetAction
           liqProviders
           ed
           [(lqName, projDatesByPattern dp ss ed)]++r
    _ -> buildLiqResetAction liqProviders ed r

draw :: Balance -> Date -> LiqFacility -> LiqFacility
draw  amt d liq@LiqFacility{ liqBalance = liqBal
                            ,liqStmt = mStmt
                            ,liqCredit = accCredit
                            ,liqDueInt = dueInt 
                            ,liqDuePremium = duePremium} 
  = liq { liqBalance = newBal,liqCredit = newCredit,liqStmt = Just newStmt}
    where 
        newBal = case liqBal of 
                   Just availBal -> Just (availBal - amt)
                   Nothing -> Nothing
        newCredit = accCredit + amt
        newStmt = appendStmt 
                    mStmt $
                    SupportTxn d newBal amt newCredit dueInt duePremium LiquidationDraw

data LiqRepayType = LiqBal 
                  | LiqPremium 
                  | LiqInt 
                  | LiqRepayTypes [LiqRepayType] --TODO not implemented
                  deriving (Show)

repay :: Amount -> Date -> LiqRepayType -> LiqFacility -> LiqFacility
repay bal d pt liq@LiqFacility{liqBalance = liqBal
                              ,liqStmt = mStmt 
                              ,liqCredit = credit
                              ,liqDueInt = liqDueInt
                              ,liqDuePremium = liqDuePremium
                              ,liqType = lt} 
  = liq {liqBalance = newBal
         ,liqCredit = newCredit
         ,liqDueInt = newIntDue
         ,liqDuePremium = newDuePremium
         ,liqStmt = Just newStmt}
    where 
      (newBal,newCredit,newIntDue,newDuePremium) = 
        case pt of 
          LiqBal -> ( Just (bal + (fromMaybe 0 liqBal)), credit - bal,liqDueInt,liqDuePremium )
          LiqPremium -> ( liqBal, credit , liqDueInt,  (Just (fromMaybe 0 liqDuePremium  - bal )))
          LiqInt -> (liqBal, credit , Just ((fromMaybe 0 liqDueInt) - bal) , liqDuePremium )

      newStmt = appendStmt mStmt $ 
                           SupportTxn d liqBal bal newCredit newIntDue newDuePremium LiquidationRepay


type SettleDates = DatePattern
type Notional = Balance

data RateSwapType = FloatingToFloating Floater Floater   -- Paying Floating rate and receiving Floating Rate
                  | FloatingToFixed  Floater IRate        -- Paying Floating Rate and receiving Fixed Rate
                  | FixedToFloating  IRate Floater        -- Paying Fixed Rate and receiving Floating rate
                  deriving(Show)

type ReceiveAmount = Balance
type PayoutAmount = Balance
type NetCash = Either ReceiveAmount PayoutAmount

data RateSwapBase = Fixed Balance
                  | Base DealStats
                  deriving(Show)

data RateSwap = RateSwap {rsType :: RateSwapType
                         ,settleDates :: SettleDates
                         ,notional :: RateSwapBase
                         ,startDate :: StartDate
                         ,payingRate :: IRate
                         ,receivingRate :: IRate
                         ,refBalance :: Balance
                         ,lastStlDate :: Maybe Date
                         ,netCash :: NetCash
                         ,stmt :: Maybe Statement}
                         deriving(Show)
              
accrueIRS :: Date -> RateSwap -> RateSwap
accrueIRS d rs@RateSwap{refBalance = face
                      , payingRate=payRate
                      , receivingRate =receiveRate
                      , netCash = netCash
                      , stmt = stmt}
  =  rs {netCash = newNet , lastStlDate = Just d, stmt = newStmt }
      where 
          accureStartDate =  case lastStlDate rs of 
                               Nothing ->  startDate rs 
                               Just lsd -> lsd
          rateDiff =  receiveRate - payRate 
          yearFactor = fromRational $ (yearCountFraction DC_ACT_365 accureStartDate d)
          newNetAmount = mulBIR (face * yearFactor) $ rateDiff 
          currentNetAmount = case netCash of 
                              Left l -> l 
                              Right r -> negate r 
          cumNetAmt = currentNetAmount + newNetAmount
          newNet = if cumNetAmt > 0 then
                     Left cumNetAmt
                   else 
                     Right cumNetAmt
          newTxn = IrsTxn d face newNetAmount payRate receiveRate cumNetAmt SwapAccure
          newStmt = case stmt of 
                      Nothing -> Just $ Statement [newTxn]
                      Just (Statement txns) -> Just $ Statement $ txns++[newTxn]

receiveIRS :: Date -> RateSwap -> RateSwap 
receiveIRS d rs@RateSwap{netCash = Left receiveAmt, stmt = stmt} 
  =  rs { netCash = Left 0 }
    where 
      newTxn = IrsTxn d 0 receiveAmt 0 0 0 SwapSettle

payoutIRS :: Date -> Amount -> RateSwap -> RateSwap 
payoutIRS d amt rs@RateSwap{netCash = Right payoutAmt, stmt = stmt} 
  =  rs { netCash = Right outstanding }
    where 
      actualAmt = min amt payoutAmt  --TODO need to add a check here
      outstanding = payoutAmt - amt
      newTxn = IrsTxn d 0 actualAmt 0 0 0 SwapSettle


data CurrencySwap = CurrencySwap Rate Balance
                  | Dummy3
                  deriving(Show)
              

$(deriveJSON defaultOptions ''RateSwap)
$(deriveJSON defaultOptions ''RateSwapType)
$(deriveJSON defaultOptions ''RateSwapBase)
$(deriveJSON defaultOptions ''CurrencySwap)
$(deriveJSON defaultOptions ''LiqRepayType)
$(deriveJSON defaultOptions ''LiqSupportType)
$(deriveJSON defaultOptions ''LiqSupportRate)
$(deriveJSON defaultOptions ''LiqFacility)
