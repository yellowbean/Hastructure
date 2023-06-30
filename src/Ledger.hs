{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Ledger (Ledger(..),entryLog)
    where
import qualified Data.Time as T
import Stmt 
import Types
import Lib
import Util
import Data.Aeson hiding (json)
import Language.Haskell.TH
import Data.Aeson.TH
import Data.Aeson.Types
import GHC.Generics

import Debug.Trace
debug = flip trace

data Ledger = Ledger {
    ledgName :: String
    ,ledgBalance :: Balance
    ,ledgStmt :: Maybe Statement
} deriving (Show, Generic)

entryLog :: Amount -> Date -> TxnComment -> Ledger -> Ledger
entryLog amt d cmt ledg@Ledger{ledgStmt = mStmt, ledgBalance = bal} 
  = ledg { ledgStmt = Just (appendStmt mStmt txn) ,ledgBalance = newBal }
    where 
      newBal = bal + amt
      txn = EntryTxn d newBal amt cmt

$(deriveJSON defaultOptions ''Ledger)