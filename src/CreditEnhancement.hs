{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module CreditEnhancement
  (LiqFacility(..),LiqSupportType(..),buildLiqResetAction
  ,LiquidityProviderName,draw,repay,LiqSupportRate(..)
  ,RateSwap(..),LiqRepayType(..)
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
data LiqSupportRate = FixRate DatePattern Rate LastAccDate
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
                    mStmt
                    (SupportTxn d newBal amt newCredit dueInt duePremium Empty)


data LiqRepayType = LiqBal 
                  | LiqPremium 
                  | LiqInt 
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
          LiqBal -> ( (+ bal) <$> liqBal, credit - bal,liqDueInt,liqDuePremium )
          LiqPremium -> ( liqBal, credit , liqDueInt, (\x->x-bal)  <$> liqDuePremium )
          LiqInt -> (liqBal, credit , (\x->x-bal)  <$> liqDueInt , liqDuePremium )

      newStmt = appendStmt mStmt (SupportTxn d liqBal bal newCredit newIntDue newDuePremium Empty)


data RateSwap = RateSwap Rate Balance
              | Dummy2 
              deriving(Show)

$(deriveJSON defaultOptions ''RateSwap)
$(deriveJSON defaultOptions ''LiqRepayType)


$(deriveJSON defaultOptions ''LiqSupportType)
$(deriveJSON defaultOptions ''LiqSupportRate)
$(deriveJSON defaultOptions ''LiqFacility)
