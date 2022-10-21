{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module CreditEnhancement
  (LiqFacility(..),LiqSupportType(..),buildLiqResetAction
  ,LiquidityProviderName,draw
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
                    deriving(Show)

data LiqFacility = LiqFacility {
    liqName :: String 
    ,liqType :: LiqSupportType 
    ,liqBalance :: Balance
    ,liqCredit :: Balance
    ,liqStart :: Date
    ,liqStmt :: Maybe Statement
} deriving (Show)


buildLiqResetAction :: [LiqFacility] -> Date -> [(String, Dates)] -> [(String, Dates)]
buildLiqResetAction [] ed r = r
buildLiqResetAction (liqProvider:liqProviders) ed r = 
  case liqProvider of 
    (LiqFacility lqName (ReplenishSupport dp bal) _ _ ss stmt)
      -> buildLiqResetAction
           liqProviders
           ed
           [(lqName, projDatesByPattern dp ss ed)]++r
    _ -> buildLiqResetAction liqProviders ed r

draw :: Balance -> Date -> LiqFacility -> LiqFacility
draw  bal d liq@LiqFacility{ liqBalance = availBal 
                            ,liqStmt = mStmt
                            ,liqCredit = accCredit} 
  = liq { liqBalance = newBal,liqCredit = newCredit,liqStmt = Just newStmt}
    where 
        newBal = availBal - bal
        newCredit = accCredit+bal
        newStmt = appendStmt 
                    mStmt
                    (SupportTxn d newBal bal newCredit Empty)
                                    

$(deriveJSON defaultOptions ''LiqSupportType)
$(deriveJSON defaultOptions ''LiqFacility)
