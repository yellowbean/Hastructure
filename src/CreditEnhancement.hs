{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module CreditEnhancement
  (LiqFacility(..),LiqSupportType(..),buildLiqResetAction,buildLiqRateResetAction
  ,LiquidityProviderName,draw,repay,accrueLiqProvider
  ,LiqRepayType(..),LiqDrawType(..)
  )
  where

import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Data.Map as Map
import qualified InterestRate as IR
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

data LiqSupportType = ReplenishSupport DatePattern Balance    -- ^ credit will be refresh by an interval
                    | FixSupport                              -- ^ fixed credit amount
                    | ByPct DealStats Rate                    -- ^ By a pct of formula
                    | UnLimit                                 -- ^ Unlimit credit support, like insurance company
                    deriving(Show,Generic)

data LiqFacility = LiqFacility {
    liqName :: String 
    ,liqType :: LiqSupportType 
    ,liqBalance :: Balance                   -- ^ total support balance supported
    ,liqCredit :: Maybe Balance              -- ^ available balance to support. Nothing -> unlimit 
    ,liqRateType :: Maybe IR.RateType        -- ^ interest rate type 
    ,liqPremiumRateType :: Maybe IR.RateType -- ^ premium rate type
    
    ,liqRate :: Maybe IRate                  -- ^ current interest rated on oustanding balance
    ,liqPremiumRate :: Maybe IRate           -- ^ current premium rate used on credit un-used, a.k. commitment fee
    
    ,liqDueIntDate :: Maybe Date             -- ^ last day of interest/premium calculated
    
    ,liqDueInt :: Balance                    -- ^ oustanding due on interest
    ,liqDuePremium :: Balance                -- ^ oustanding due on premium
    
    ,liqStart :: Date                        -- ^ when liquidiy provider came into effective
    ,liqEnds :: Maybe Date                   -- ^ when liquidiy provider came into expired
    ,liqStmt :: Maybe Statement              -- ^ transaction history
} deriving (Show,Generic)


buildLiqResetAction :: [LiqFacility] -> Date -> [(String, Dates)] -> [(String, Dates)]
buildLiqResetAction [] ed r = r
buildLiqResetAction (liqProvider:liqProviders) ed r = 
  case liqProvider of 
    (LiqFacility lqName (ReplenishSupport dp bal) _ _ _ _ _ _ _ _ _ ss _ stmt)
      -> buildLiqResetAction
           liqProviders
           ed
           [(lqName, projDatesByPattern dp ss ed)]++r
    _ -> buildLiqResetAction liqProviders ed r

buildLiqRateResetAction  :: [LiqFacility] -> Date -> [(String, Dates)] -> [(String, Dates)]
buildLiqRateResetAction [] ed r = r
buildLiqRateResetAction (liq:liqProviders) ed r = 
  case liq of 
    liq@LiqFacility{liqRateType = rt, liqPremiumRateType = prt, liqName =ln , liqStart = sd} -> 
       buildLiqRateResetAction 
        liqProviders 
        ed 
        [(ln,IR.getRateResetDates sd ed rt ++ IR.getRateResetDates sd ed prt)]++r
    _ -> buildLiqRateResetAction liqProviders ed r

draw :: Amount -> Date -> LiqFacility -> LiqFacility
draw  amt d liq@LiqFacility{ liqBalance = liqBal
                            ,liqStmt = mStmt
                            ,liqCredit = mCredit
                            ,liqDueInt = dueInt 
                            ,liqDuePremium = duePremium} 
  = liq { liqBalance = newBal,liqCredit = newCredit,liqStmt = newStmt}
    where 
        newCredit = (\x -> x - amt) <$> mCredit
        newBal = liqBal + amt
        newStmt = appendStmt 
                    mStmt $
                    SupportTxn d newCredit amt newBal dueInt duePremium LiquidationDraw

data LiqDrawType = LiqToAcc        -- ^ draw credit and deposit cash to account
                 | LiqToBondInt    -- ^ draw credit and pay to bond interest if any shortfall
                 | LiqToBondPrin   -- ^ draw credit and pay to bond principal if any shortfall
                 | LiqToFee        -- ^ draw credit and pay to a fee if there is a shortfall
                 deriving (Show,Generic)

data LiqRepayType = LiqBal         -- ^ repay oustanding balance of liquidation provider
                  | LiqPremium     -- ^ repay oustanding premium fee of lp
                  | LiqInt         -- ^ repay oustanding interest of lp
                  | LiqRepayTypes [LiqRepayType] --TODO not implemented
                  deriving (Show,Generic)

repay :: Amount -> Date -> LiqRepayType -> LiqFacility -> LiqFacility
repay amt d pt liq@LiqFacility{liqBalance = liqBal
                              ,liqStmt = mStmt 
                              ,liqCredit = mCredit
                              ,liqDueInt = liqDueInt
                              ,liqDuePremium = liqDuePremium
                              ,liqType = lt} 
  = liq {liqBalance = newBal
         ,liqCredit = newCredit
         ,liqDueInt = newIntDue
         ,liqDuePremium = newDuePremium
         ,liqStmt = newStmt}
    where 
      (newBal,newCredit,newIntDue,newDuePremium) = 
        case pt of 
          LiqBal -> ( liqBal - amt, (amt +) <$> mCredit,liqDueInt, liqDuePremium )
          LiqPremium -> ( liqBal, mCredit , liqDueInt,   liqDuePremium  - amt)
          LiqInt -> (liqBal, mCredit , liqDueInt - amt , liqDuePremium )

      newStmt = appendStmt mStmt $ 
                           SupportTxn d newCredit amt newBal newIntDue newDuePremium LiquidationRepay

accrueLiqProvider ::  Date -> LiqFacility -> LiqFacility
accrueLiqProvider d liq@(LiqFacility _ _ curBal mCredit mRateType mPRateType rate prate dueDate dueInt duePremium sd mEd Nothing)
  = accrueLiqProvider d $ liq{liqStmt = Just defaultStmt} 
    where 
      defaultStmt = Statement [SupportTxn sd mCredit 0 curBal dueInt duePremium Empty]

accrueLiqProvider d liq@(LiqFacility _ _ curBal mCredit mRateType mPRateType rate prate dueDate dueInt duePremium sd mEd mStmt)
  = liq { liqCredit = newCredit
         ,liqStmt = newStmt
         ,liqDueInt = newDueInt
         ,liqDuePremium = newDueFee }
    where 
      accureInt = case rate of 
                    Nothing -> 0
                    Just r -> 
                      let 
                        lastAccDate = fromMaybe sd dueDate
                        bals = weightAvgBalanceByDates [lastAccDate,d] $ getTxns mStmt
                      in 
                        sum $ flip mulBIR r <$> bals
      accureFee = case prate of
                    Nothing -> 0 
                    Just r -> 
                      let 
                        lastAccDate = fromMaybe sd dueDate
                        (_,_unAccTxns) = splitByDate (getTxns mStmt) lastAccDate EqToLeftKeepOne
                        accBals = getUnusedBal <$> _unAccTxns 
                        _ds = lastAccDate : tail (getDate <$> _unAccTxns)
                        _avgBal = calcWeigthBalanceByDates accBals (_ds++[d])
                      in 
                        mulBIR _avgBal r
                        
      getUnusedBal (SupportTxn _ b _ _ _ _ _ ) = fromMaybe 0 b 
      
      newDueFee = accureFee + duePremium
      newDueInt = accureInt + dueInt
      newCredit = (\x-> x - accureInt - accureFee) <$> mCredit 
      newStmt = appendStmt mStmt $ SupportTxn d 
                                             newCredit
                                             (accureInt + accureFee) 
                                             curBal
                                             newDueInt 
                                             newDueFee 
                                             (LiquidationSupportInt accureInt accureFee)



instance QueryByComment LiqFacility where 
    queryStmt liq@LiqFacility{liqStmt = Nothing} tc = []
    queryStmt liq@LiqFacility{liqStmt = (Just (Statement txns))} tc
      = filter (\x -> getTxnComment x == tc) txns


$(deriveJSON defaultOptions ''LiqRepayType)
$(deriveJSON defaultOptions ''LiqDrawType)
$(deriveJSON defaultOptions ''LiqSupportType)
$(deriveJSON defaultOptions ''LiqFacility)
