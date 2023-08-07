{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Accounts (Account(..),ReserveAmount(..),draw,deposit,supportPay
                ,transfer,depositInt,depositIntByCurve
                ,InterestInfo(..),buildEarnIntAction,updateReserveBalance)
    where
import qualified Data.Time as T
import Stmt (Statement(..),appendStmt,Txn(..),getTxnBegBalance,sliceTxns,getDate
            ,TxnComment(..),QueryByComment(..),getTxnComment,getTxnAmt,weightAvgBalanceByDates)
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

data InterestInfo = BankAccount IRate Date DatePattern
                  | InvestmentAccount Index Spread Date DatePattern
                  deriving (Show, Generic)

data ReserveAmount = PctReserve DealStats Rate
                   | FixReserve Balance
                   | Either Pre ReserveAmount ReserveAmount
                   | Max ReserveAmount ReserveAmount
                   | Min ReserveAmount ReserveAmount
                   deriving (Show, Eq, Generic)

data Account = Account {
    accBalance :: Balance
    ,accName :: String
    ,accInterest :: Maybe InterestInfo
    ,accType :: Maybe ReserveAmount
    ,accStmt :: Maybe Statement
} deriving (Show, Generic)


buildEarnIntAction :: [Account] -> Date -> [(String,Dates)] -> [(String,Dates)]
buildEarnIntAction [] ed r = r
buildEarnIntAction (acc:accs) ed r = 
  case acc of 
    (Account _ _ Nothing _ _) 
      -> buildEarnIntAction accs ed r
    (Account _ an (Just (BankAccount _ lastAccDate dp)) _ _)
      -> buildEarnIntAction accs ed [(an, genSerialDatesTill2 EE lastAccDate dp ed)]++r    
    (Account _ an (Just (InvestmentAccount _ _ lastAccDate dp)) _ _)
      -> buildEarnIntAction accs ed [(an, genSerialDatesTill2 EE lastAccDate dp ed)]++r    


depositInt :: Account -> Date -> Account
depositInt a@(Account _ _ Nothing _ _) _ = a
depositInt a@(Account bal _ (Just (BankAccount r lastCollectDate dp)) _ stmt) ed 
          = a {accBalance = newBal ,accStmt= Just new_stmt ,accInterest = Just (BankAccount r ed dp)}
          where 
            accrued_int = case stmt of 
                            Nothing -> mulBR 
                                         (mulBI bal r) 
                                         (yearCountFraction DC_30E_360 lastCollectDate ed) -- `debug` (">>"++show lastCollectDate++">>"++show ed)
                            Just (Statement _txns) ->
                              let 
                                _accrue_txns = sliceTxns _txns lastCollectDate ed
                                _bals = map getTxnBegBalance _accrue_txns ++ [bal] -- `debug` ("ACCU TXN"++show _accrue_txns)
                                _ds = getDates _accrue_txns
                                _dfs = getIntervalFactors $ [lastCollectDate] ++ _ds ++ [ed]
                              in
                                mulBI (sum $ zipWith mulBR _bals _dfs) r  

            newBal = accrued_int + bal  -- `debug` ("INT ACC->"++ show accrued_int)
            new_txn = AccTxn ed newBal accrued_int BankInt
            new_stmt = appendStmt stmt new_txn

depositIntByCurve :: Account -> Ts -> Date -> Account
depositIntByCurve a@(Account bal _ (Just (InvestmentAccount idx spd lastCollectDate dp)) _ stmt)
                  rc
                  ed 
          = a {accBalance = newBal 
              ,accStmt= Just new_stmt 
              ,accInterest = Just (InvestmentAccount idx spd ed dp)}
          where 
            accrued_int = case stmt of 
                            Nothing -> 
                                let  
                                  curve_ds = [lastCollectDate] ++ subDates EE lastCollectDate ed (getTsDates rc) ++ [ed]
                                  curve_vs = map 
                                               (\x -> toRational (getValByDate rc Exc x) + toRational spd)
                                               (init curve_ds)
                                  ds_factor = getIntervalFactors curve_ds
                                  weightInt = sum $ zipWith (*) curve_vs ds_factor --  `debug` ("ds"++show curve_ds++"vs"++show curve_vs++"factors"++show ds_factor)
                                in 
                                  mulBR bal weightInt
                            Just (Statement _txns) ->
                              let 
                                curve_ds = [lastCollectDate] ++ subDates EE lastCollectDate ed (getTsDates rc) ++ [ed]
                                curve_vs = map 
                                             (\x -> toRational (getValByDate rc Exc x) + toRational spd)
                                             (init curve_ds)
                                bals = weightAvgBalanceByDates curve_ds _txns
                              in
                                sum $ zipWith mulBR bals curve_vs -- `debug` ("cds"++show curve_ds++"vs"++ show curve_vs++"bs"++show bals)

            newBal = accrued_int + bal  -- `debug` ("INT ACC->"++ show accrued_int)
            new_txn = AccTxn ed newBal accrued_int BankInt
            new_stmt = appendStmt stmt new_txn



transfer :: Account -> Amount -> T.Day -> Account -> (Account, Account)
transfer source_acc@(Account s_bal san _ _ s_stmt)
         amount
         d
         target_acc@(Account t_bal tan _ _ t_stmt)
  = (source_acc {accBalance = new_s_bal, accStmt = Just source_newStmt}
    ,target_acc {accBalance = new_t_bal, accStmt = Just target_newStmt})
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

updateReserveBalance :: ReserveAmount -> Account -> Account 
updateReserveBalance ra acc = acc {accType = Just ra}

supportPay :: [Account] -> Date -> Amount -> (TxnComment, TxnComment) -> [Account]
supportPay all_accs@(acc:accs) d amt (m1,m2) = 
    (draw payOutAmt d m1 acc): (map (\(_acc,amt) -> draw amt d m2 _acc) supportPayByAcc)
  where 
      availBals = map accBalance all_accs
      accNames = map accName all_accs
      payOutAmt:payOutAmts = paySeqLiabilitiesAmt amt availBals
      supportPayByAcc = filter (\(_acc,_amt_out) -> _amt_out > 0)   $ zip accs payOutAmts

instance QueryByComment Account where 
    queryStmt (Account _ _ _ _ Nothing) tc = []
    queryStmt (Account _ _ _ _ (Just (Statement txns))) tc
      = filter (\x -> getTxnComment x == tc) txns

$(deriveJSON defaultOptions ''InterestInfo)
$(deriveJSON defaultOptions ''ReserveAmount)
$(deriveJSON defaultOptions ''Account)
