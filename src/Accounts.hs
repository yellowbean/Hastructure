{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Accounts (Account(..),ReserveAmount(..),draw,deposit,supportPay
                ,transfer,depositInt
                ,InterestInfo(..),buildEarnIntAction)
    where
import qualified Data.Time as T
import Lib (Period(Monthly),Rate,Date,Amount,Balance,Dates,StartDate,EndDate,LastIntPayDate
           ,Balance
           ,paySeqLiabilitiesAmt,IRate,mulBI
           ,getIntervalFactors)
import Stmt (Statement(..),appendStmt,Txn(..),getTxnBegBalance,sliceTxns,getTxnDate
            ,TxnComment(..))
import Types
import Util
-- import IntrestRate
import Data.Aeson hiding (json)
import Language.Haskell.TH
import Data.Aeson.TH
import Data.Aeson.Types

import Debug.Trace
debug = flip trace

data InterestInfo = BankAccount IRate Date DatePattern
                 -- | InvestmentAccount IRate Date Period
                  deriving (Show)

data ReserveAmount = PctReserve DealStats Rate
                   | FixReserve Balance
                   | Max ReserveAmount ReserveAmount
                   | Min ReserveAmount ReserveAmount
                   deriving (Show)

data Account = Account {
    accBalance :: Balance
    ,accName :: String
    ,accInterest :: Maybe InterestInfo
    ,accType :: Maybe ReserveAmount
    ,accStmt :: Maybe Statement
} deriving (Show)

$(deriveJSON defaultOptions ''InterestInfo)
$(deriveJSON defaultOptions ''ReserveAmount)
$(deriveJSON defaultOptions ''Account)

buildEarnIntAction :: [Account] -> Date -> [(String,Dates)] -> [(String,Dates)]
buildEarnIntAction [] ed r = r
buildEarnIntAction (acc:accs) ed r = 
  case acc of 
    (Account _ _ Nothing _ _) 
      -> buildEarnIntAction accs ed r
    (Account _ an (Just (BankAccount _ lastAccDate dp)) _ _)
      -> buildEarnIntAction accs ed [(an, projDatesByPattern dp lastAccDate ed)]++r    


depositInt :: Account -> Date -> Account
depositInt a@(Account _ _ Nothing _ _) _ = a
depositInt a@(Account 
                bal 
                _ 
                (Just (BankAccount r lastCollectDate dp)) 
                _
                stmt) ed 
          = a {accBalance = newBal
              ,accStmt= Just new_stmt
              ,accInterest = Just (BankAccount r ed dp)}
          where 
            accrued_int = case stmt of 
                            Nothing -> mulBR 
                                         (mulBI bal r) 
                                         (yearCountFraction DC_30E_360 lastCollectDate ed) -- `debug` (">>"++show lastCollectDate++">>"++show ed)
                            Just (Statement _txns) ->
                              let 
                                _accrue_txns = sliceTxns _txns lastCollectDate ed
                                _bals = (map getTxnBegBalance _accrue_txns) ++ [bal] -- `debug` ("ACCU TXN"++show _accrue_txns)
                                _ds = map getTxnDate _accrue_txns
                                _dfs = getIntervalFactors $ [lastCollectDate] ++ _ds ++ [ed]
                              in
                                mulBI (sum $ zipWith mulBR _bals _dfs) r  
                                -- `debug` (">>>"++show _bals ++">>>"++show ([lastCollectDate] ++ _ds ++ [ed]) ++">>>"++show _dfs)


            newBal = accrued_int + bal  -- `debug` ("INT ACC->"++ show accrued_int)
            new_txn = (AccTxn ed newBal accrued_int BankInt)
            new_stmt = appendStmt stmt new_txn


transfer :: Account -> Amount -> T.Day -> Account -> (Account, Account)
transfer source_acc@(Account s_bal san _ _ s_stmt)
         amount
         d
         target_acc@(Account t_bal tan _ _ t_stmt)
  = (source_acc {accBalance = new_s_bal, accStmt = (Just source_newStmt)}
    ,target_acc {accBalance = new_t_bal, accStmt = (Just target_newStmt)})
  where
    new_s_bal = s_bal - amount
    new_t_bal = t_bal + amount
    source_newStmt = appendStmt s_stmt (AccTxn d new_s_bal (- amount) (Transfer san tan))
    target_newStmt = appendStmt t_stmt (AccTxn d new_t_bal amount (Transfer san tan) )

deposit :: Amount -> Date -> TxnComment -> Account -> Account
deposit amount d source acc@(Account bal _ _ _ maybeStmt)  =
    acc {accBalance = newBal, accStmt = Just newStmt}
  where
    newBal = bal + amount
    newStmt = appendStmt maybeStmt (AccTxn d newBal amount source)

draw :: Amount -> Date -> TxnComment -> Account -> Account
draw amount = deposit (- amount) 


supportPay :: [Account] -> Date -> Amount -> (TxnComment, TxnComment) -> [Account]
supportPay all_accs@(acc:accs) d amt (m1,m2) = 
    (draw payOutAmt d m1 acc): (map (\(_acc,amt) -> draw amt d m2 _acc) supportPayByAcc)
  where 
      availBals = map accBalance all_accs
      accNames = map accName all_accs
      payOutAmt:payOutAmts = paySeqLiabilitiesAmt amt availBals
      supportPayByAcc = filter (\(_acc,_amt_out) -> _amt_out > 0)   $ zip accs payOutAmts
