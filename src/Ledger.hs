{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Ledger (Ledger(..),entryLog,LedgerName,queryGap,entryDebit,entryCredit,clearLedgersBySeq
              ,queryDirection,entryLogByDr)
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

import Control.Lens hiding (element)

import Control.Lens.TH
import Debug.Trace
debug = flip trace


type LedgerName = String

data Ledger = Ledger {
    ledgName :: String                              -- ^ ledger account name
    ,ledgBalance :: Balance                         -- ^ current balance of ledger
    ,ledgStmt :: Maybe Statement                    -- ^ ledger transaction history
} deriving (Show, Generic,Ord, Eq)

-- | Book an entry with date,amount and transaction to a ledger
entryLog :: Amount -> Date -> TxnComment -> Ledger -> Ledger
entryLog amt d cmt ledg@Ledger{ledgStmt = mStmt, ledgBalance = bal} 
  | isTxnDirection Credit cmt  = let 
                                   newBal = bal - amt
                                   txn = EntryTxn d newBal amt cmt
                                 in 
                                   ledg { ledgStmt = appendStmt mStmt txn ,ledgBalance = newBal }
  | otherwise = let 
                  newBal = bal + amt
                  txn = EntryTxn d newBal amt cmt
                in 
                  ledg { ledgStmt = appendStmt mStmt txn ,ledgBalance = newBal }

entryLogByDr :: BookDirection -> Amount -> Date -> TxnComment -> Ledger -> Ledger
entryLogByDr Credit amt = entryLog (negate amt) 
entryLogByDr Debit amt = entryLog amt 

isTxnDirection :: BookDirection -> TxnComment -> Bool 
isTxnDirection Credit (TxnDirection Credit) = True
isTxnDirection Debit (TxnDirection Debit) = True
isTxnDirection Credit (TxnComments txns) = any (isTxnDirection Credit) txns
isTxnDirection Debit (TxnComments txns) = any (isTxnDirection Debit) txns
isTxnDirection _ _ = False

-- ^ credit is negative amount
entryCredit :: Amount -> Date -> TxnComment -> Ledger -> Ledger 
entryCredit amt d txn lg@Ledger{ledgName = ln}
  | isTxnDirection Credit txn = entryLog (negate amt) d txn lg
  | otherwise = undefined $ "Failed to write credit txn to ledger "++ ln ++ " with txn"++ show txn

entryDebit :: Amount -> Date -> TxnComment -> Ledger -> Ledger 
entryDebit amt d txn lg@Ledger{ledgName = ln}
  | isTxnDirection Debit txn = entryLog amt d txn lg
  | otherwise = undefined $ "Failed to write debit txn to ledger "++ ln ++ " with txn"++ show txn

queryDirection :: Ledger -> (BookDirection ,Balance) 
queryDirection (Ledger _ bal _)
  |  bal >= 0 = (Debit, bal)
  |  bal < 0 = (Credit, negate bal)

-- ^ return ledger's bookable amount (for netting off to zero ) with direction input
queryGap :: BookDirection -> Ledger -> Balance
queryGap dr Ledger{ledgBalance = bal}  
  = case (bal > 0, dr) of 
      (True, Debit) -> 0
      (True, Credit) -> bal
      (False, Debit) -> negate bal 
      (False, Credit) -> 0

clearLedgersBySeq :: BookDirection -> Date -> Amount -> [Ledger] -> [Ledger] -> ([Ledger],Amount)
clearLedgersBySeq dr d 0 rs unAllocLedgers = (rs++unAllocLedgers,0)
clearLedgersBySeq dr d amtToAlloc rs [] = (rs,amtToAlloc)
clearLedgersBySeq dr d amtToAlloc rs (ledger@Ledger{ledgBalance = bal}:ledgers)  
  = let 
      deductAmt = queryGap dr ledger
      allocAmt = min deductAmt amtToAlloc
      remainAmt = amtToAlloc - allocAmt
      newLedger = entryLog allocAmt d (TxnDirection dr) ledger
    in 
      clearLedgersBySeq dr d remainAmt (newLedger:rs) ledgers

instance QueryByComment Ledger where 
    queryStmt (Ledger _ _ Nothing) tc = []
    queryStmt (Ledger _ _ (Just (Statement txns))) tc
      = filter (\x -> getTxnComment x == tc) txns

    queryTxnAmt a tc = sum $ map getTxnAmt $ queryStmt a tc

makeLensesFor [("ledgName","ledgNameLens"),("ledgBalance","ledgBalLens"),("ledgStmt","ledgStmtLens")] ''Ledger


$(deriveJSON defaultOptions ''Ledger)
