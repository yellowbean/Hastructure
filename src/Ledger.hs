{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Ledger (Ledger(..),entryLog,LedgerName,queryGap,clearLedgersBySeq
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
                                   ledg { ledgStmt = appendStmt txn mStmt,ledgBalance = newBal }
  | otherwise = let 
                  newBal = bal + amt
                  txn = EntryTxn d newBal amt cmt
                in 
                  ledg { ledgStmt = appendStmt txn mStmt ,ledgBalance = newBal }

-- TODO-- need to ensure there is no direction in input
entryLogByDr :: BookDirection -> Amount -> Date -> Maybe TxnComment -> Ledger -> Ledger
entryLogByDr dr amt d Nothing = entryLog amt d (TxnDirection dr)
entryLogByDr dr amt d (Just cmt) 
  | not (hasTxnDirection cmt) = entryLog amt d (TxnComments [TxnDirection dr,cmt])
  | isTxnDirection dr cmt = entryLog amt d  cmt
  | otherwise = error $ "Suppose direction"++ show dr++"but got from comment"++ show cmt

entryLogByDr Credit amt d (Just (TxnComments cms)) = entryLog amt d (TxnComments ((TxnDirection Credit):cms))
entryLogByDr Debit amt d (Just (TxnComments cms)) = entryLog amt d (TxnComments ((TxnDirection Debit):cms))


hasTxnDirection :: TxnComment -> Bool
hasTxnDirection (TxnDirection _) = True
hasTxnDirection (TxnComments txns) = any (hasTxnDirection) txns
hasTxnDirection _ = False


isTxnDirection :: BookDirection -> TxnComment -> Bool 
isTxnDirection Credit (TxnDirection Credit) = True
isTxnDirection Debit (TxnDirection Debit) = True
isTxnDirection Credit (TxnComments txns) = any (isTxnDirection Credit) txns
isTxnDirection Debit (TxnComments txns) = any (isTxnDirection Debit) txns
isTxnDirection _ _ = False

-- ^ credit is negative amount
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
