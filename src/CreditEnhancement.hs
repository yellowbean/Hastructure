{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module CreditEnhancement
  (LiqFacility(..),LiqSupportType(..),buildLiqResetAction
  ,LiquidityProviderName,draw,repay,LiqSupportRate(..)
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
    ,liqCredit :: Balance  -- total support balance 
    ,liqStart :: Date
    ,liqRate :: Maybe LiqSupportRate
    ,liqStmt :: Maybe Statement
} deriving (Show)


buildLiqResetAction :: [LiqFacility] -> Date -> [(String, Dates)] -> [(String, Dates)]
buildLiqResetAction [] ed r = r
buildLiqResetAction (liqProvider:liqProviders) ed r = 
  case liqProvider of 
    (LiqFacility lqName (ReplenishSupport dp bal) _ _ ss _ stmt)
      -> buildLiqResetAction
           liqProviders
           ed
           [(lqName, projDatesByPattern dp ss ed)]++r
    (LiqFacility lqName (ByPct dp ds pct) _ _ ss _ stmt)
      -> buildLiqResetAction
           liqProviders
           ed
           [(lqName, projDatesByPattern dp ss ed)]++r
    _ -> buildLiqResetAction liqProviders ed r

draw :: Balance -> Date -> LiqFacility -> LiqFacility
draw  amt d liq@LiqFacility{ liqBalance = liqBal
                            ,liqStmt = mStmt
                            ,liqCredit = accCredit} 
  = liq { liqBalance = newBal,liqCredit = newCredit,liqStmt = Just newStmt}
    where 
        newBal = case liqBal of 
                   Just availBal -> Just (availBal - amt)
                   Nothing -> Nothing
        newCredit = accCredit+amt
        newStmt = appendStmt 
                    mStmt
                    (SupportTxn d newBal amt newCredit Empty)

repay :: Balance -> Date -> LiqFacility -> LiqFacility
repay bal d liq@LiqFacility{liqBalance = liqBal, liqStmt = mStmt ,liqCredit = accCredit} 
  = liq { liqCredit = newCredit,liqStmt = Just newStmt}
    where 
        newCredit = accCredit - bal
        newStmt = appendStmt mStmt (SupportTxn d liqBal (negate bal) newCredit Empty)


$(deriveJSON defaultOptions ''LiqSupportType)
$(deriveJSON defaultOptions ''LiqSupportRate)
$(deriveJSON defaultOptions ''LiqFacility)
