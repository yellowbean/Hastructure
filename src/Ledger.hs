{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Ledger (Ledger(..),entryLog,LedgerName)
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

type LedgerName = String

data Ledger = Ledger {
    ledgName :: String              -- ^ ledger account name
    ,ledgBalance :: Balance         -- ^ current balance of ledger
    ,ledgStmt :: Maybe Statement    -- ^ ledger transaction history
} deriving (Show, Generic)


entryLog :: Amount -> Date -> TxnComment -> Ledger -> Ledger
entryLog amt d cmt ledg@Ledger{ledgStmt = mStmt, ledgBalance = bal} 
  = ledg { ledgStmt = appendStmt mStmt txn ,ledgBalance = newBal }
    where 
      newBal = bal + amt
      txn = EntryTxn d newBal amt cmt

instance QueryByComment Ledger where 
    queryStmt (Ledger _ _ Nothing) tc = []
    queryStmt (Ledger _ _ (Just (Statement txns))) tc
      = filter (\x -> getTxnComment x == tc) txns

    queryTxnAmt a tc
      = sum $ map getTxnAmt $ queryStmt a tc

$(deriveJSON defaultOptions ''Ledger)