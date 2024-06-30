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
import GHC.Generics
import Language.Haskell.TH
import Data.Aeson hiding (json)
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Fixed
import Data.Maybe
import Types
import Util
import DateUtil
import Stmt
import qualified InterestRate as IR

import Debug.Trace
debug = flip trace

type LiquidityProviderName = String

data LiqSupportType = ReplenishSupport DatePattern Balance    -- ^ credit will be refresh by an interval
                    | FixSupport Balance                      -- ^ fixed credit amount
                    | ByPct DealStats Rate                    -- ^ By a pct of formula
                    | UnLimit                                 -- ^ Unlimit credit support, like insurance company
                    deriving(Show,Generic,Eq,Ord)

data LiqFacility = LiqFacility {
    liqName :: String 
    ,liqType :: LiqSupportType 
    ,liqBalance :: Balance                   -- ^ total support balance supported
    ,liqCredit :: Maybe Balance              -- ^ available balance to support. Nothing -> unlimit 
    ,liqRateType :: Maybe IR.RateType        -- ^ interest rate type 
    ,liqPremiumRateType :: Maybe IR.RateType -- ^ premium rate type
    
    ,liqRate :: Maybe IRate                  -- ^ current interest rated on oustanding balance
    ,liqPremiumRate :: Maybe IRate           -- ^ current premium rate used on unused credit, a.k. commitment fee
    
    ,liqDueIntDate :: Maybe Date             -- ^ last day of interest/premium calculated
    
    ,liqDueInt :: Balance                    -- ^ oustanding due on interest
    ,liqDuePremium :: Balance                -- ^ oustanding due on premium
    
    ,liqStart :: Date                        -- ^ when liquidiy provider came into effective
    ,liqEnds :: Maybe Date                   -- ^ when liquidiy provider came into expired
    ,liqStmt :: Maybe Statement              -- ^ transaction history
} deriving (Show,Generic,Eq,Ord)

-- | update the reset events of liquidity provider
buildLiqResetAction :: [LiqFacility] -> Date -> [(String, Dates)] -> [(String, Dates)]
buildLiqResetAction [] ed r = r
buildLiqResetAction (liqProvider:liqProviders) ed r = 
  case liqProvider of 
    (LiqFacility lqName (ReplenishSupport dp bal) _ _ _ _ _ _ _ _ _ ss _ stmt) -- update the support credit of liquidity provider
      -> buildLiqResetAction
           liqProviders
           ed
           [(lqName, projDatesByPattern dp ss ed)]++r
    _ -> buildLiqResetAction liqProviders ed r

-- | update the rate reset events of liquidity provider
buildLiqRateResetAction  :: [LiqFacility] -> Date -> [(String, Dates)] -> [(String, Dates)]
buildLiqRateResetAction [] ed r = r
buildLiqRateResetAction (liq:liqProviders) ed r = 
  case liq of 
    liq@LiqFacility{liqRateType = rt, liqPremiumRateType = prt, liqName = ln , liqStart = sd} -> 
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
                 deriving (Show,Generic,Ord,Eq)

data LiqRepayType = LiqBal         -- ^ repay oustanding balance of liquidation provider
                  | LiqPremium     -- ^ repay oustanding premium fee of lp
                  | LiqInt         -- ^ repay oustanding interest of lp
                  | LiqRepayTypes [LiqRepayType] --TODO not implemented
                  deriving (Show,Generic,Ord,Eq)

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


-- | accure fee and interest of a liquidity provider
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
      lastAccDate = fromMaybe sd dueDate
      accureInt = case rate of 
                    Nothing -> 0
                    Just r -> 
                      let 
                        bals = weightAvgBalanceByDates [lastAccDate,d] $ getTxns mStmt
                      in 
                        sum $ flip mulBIR r <$> bals -- `debug` ("Gettnig bal of liq"++ show bals)
      accureFee = case prate of
                    Nothing -> 0 
                    Just r -> 
                      let 
                        (_,_unAccTxns) = splitByDate (getTxns mStmt) lastAccDate EqToLeftKeepOne
                        accBals = getUnusedBal <$> _unAccTxns 
                        _ds = lastAccDate : tail (getDate <$> _unAccTxns)
                        _avgBal = calcWeightBalanceByDates DC_ACT_365F accBals (_ds++[d])
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


instance Liable LiqFacility where 
  isPaidOff liq@LiqFacility{liqBalance=bal,liqDueInt=dueInt,liqDuePremium=duePremium}
    | bal==0 && dueInt==0 && duePremium==0 = True
    | otherwise = False


instance IR.UseRate LiqFacility where 
  getIndexes liq@LiqFacility{liqRateType = mRt,liqPremiumRateType = mPrt} 
    = case (mRt,mPrt) of 
        (Nothing, Nothing) -> Nothing
        (Just (IR.Floater _ idx _ _ _ _ _ _), Nothing ) -> Just [idx]
        (Nothing, Just (IR.Floater _ idx _ _ _ _ _ _)) -> Just [idx]
        (Just (IR.Floater _ idx1 _ _ _ _ _ _), Just (IR.Floater _ idx2 _ _ _ _ _ _)) -> Just [idx1,idx2]
        _ -> Nothing



$(deriveJSON defaultOptions ''LiqRepayType)
$(deriveJSON defaultOptions ''LiqDrawType)
$(deriveJSON defaultOptions ''LiqSupportType)
$(deriveJSON defaultOptions ''LiqFacility)
