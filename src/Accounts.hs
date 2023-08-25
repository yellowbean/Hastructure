{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Accounts (Account(..),ReserveAmount(..),draw,deposit
                ,transfer,depositInt,depositIntByCurve
                ,InterestInfo(..),buildEarnIntAction,updateReserveBalance)
    where
import qualified Data.Time as T
import Stmt (Statement(..),appendStmt,Txn(..),getTxnBegBalance,getDate
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

data InterestInfo = BankAccount IRate Date DatePattern                -- ^ fix reinvest return rate
                  | InvestmentAccount Index Spread Date DatePattern   -- ^ float reinvest return rate 
                  deriving (Show, Generic)

data ReserveAmount = PctReserve DealStats Rate               -- ^ target amount with reference to % of formula
                   | FixReserve Balance                      -- ^ target amount with fixed balance amount    
                   | Either Pre ReserveAmount ReserveAmount  -- ^ target amount depends on a test, if true, then use first one ,otherwise use second one
                   | Max [ReserveAmount]                     -- ^ use higher of all reserve formulas
                   | Min [ReserveAmount]                     -- ^ use lower of all reserve formulas
                   deriving (Show, Eq, Generic)

data Account = Account {
    accBalance :: Balance                 -- ^ account current balance
    ,accName :: String                    -- ^ account name
    ,accInterest :: Maybe InterestInfo    -- ^ account reinvestment interest
    ,accType :: Maybe ReserveAmount       -- ^ target info if a reserve account
    ,accStmt :: Maybe Statement           -- ^ transactional history
} deriving (Show, Generic)

-- | build interest earn actions
buildEarnIntAction :: [Account] -> Date -> [(String,Dates)] -> [(String,Dates)]
buildEarnIntAction [] ed r = r
buildEarnIntAction (acc:accs) ed r = 
  case acc of 
    (Account _ _ Nothing _ _) 
      -> buildEarnIntAction accs ed r
    (Account _ an (Just (BankAccount _ lastAccDate dp)) _ _)
      -> buildEarnIntAction accs ed [(an, genSerialDatesTill2 NO_IE lastAccDate dp ed)]++r    
    (Account _ an (Just (InvestmentAccount _ _ lastAccDate dp)) _ _)
      -> buildEarnIntAction accs ed [(an, genSerialDatesTill2 NO_IE lastAccDate dp ed)]++r    

-- | sweep interest/investement income into account
depositInt :: Account -> Date -> Account
depositInt a@(Account _ _ Nothing _ _) _ = a
depositInt a@(Account bal _ (Just (BankAccount r lastCollectDate dp)) _ stmt) ed 
          = a {accBalance = newBal ,accStmt= new_stmt ,accInterest = Just (BankAccount r ed dp)}
          where 
            accrued_int = case stmt of 
                            Nothing -> mulBR 
                                         (mulBI bal r) 
                                         (yearCountFraction DC_30E_360 lastCollectDate ed) -- `debug` (">>"++show lastCollectDate++">>"++show ed)
                            Just (Statement _txns) ->
                              let 
                                _accrue_txns = sliceBy IE lastCollectDate ed _txns
                                _bals = map getTxnBegBalance _accrue_txns ++ [bal] -- `debug` ("ACCU TXN"++show _accrue_txns)
                                _ds = getDates _accrue_txns
                                _dfs = getIntervalFactors $ [lastCollectDate] ++ _ds ++ [ed]
                              in
                                mulBI (sum $ zipWith mulBR _bals _dfs) r  

            newBal = accrued_int + bal  -- `debug` ("INT ACC->"++ show accrued_int)
            new_txn = AccTxn ed newBal accrued_int BankInt
            new_stmt = appendStmt stmt new_txn

-- | sweep interest/investement income into account by a yield curve
depositIntByCurve :: Account -> Ts -> Date -> Account
depositIntByCurve a@(Account bal _ (Just (InvestmentAccount idx spd lastCollectDate dp)) _ stmt)
                  rc
                  ed 
          = a {accBalance = newBal 
              ,accStmt= new_stmt 
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

            newBal = accrued_int + bal 
            new_txn = AccTxn ed newBal accrued_int BankInt
            new_stmt = appendStmt stmt new_txn


-- | move cash from account A to account B
transfer :: Account -> Amount -> Date -> Account -> (Account, Account)
transfer source_acc@(Account s_bal san _ _ s_stmt)
         amount
         d
         target_acc@(Account t_bal tan _ _ t_stmt)
  = (source_acc {accBalance = new_s_bal, accStmt = source_newStmt}
    ,target_acc {accBalance = new_t_bal, accStmt = target_newStmt})
  where
    new_s_bal = s_bal - amount
    new_t_bal = t_bal + amount
    source_newStmt = appendStmt s_stmt (AccTxn d new_s_bal (- amount) (Transfer san tan))
    target_newStmt = appendStmt t_stmt (AccTxn d new_t_bal amount (Transfer san tan) )

-- | deposit cash to account with a comment
deposit :: Amount -> Date -> TxnComment -> Account -> Account
deposit amount d source acc@(Account bal _ _ _ maybeStmt)  =
    acc {accBalance = newBal, accStmt = newStmt}
  where
    newBal = bal + amount
    newStmt = appendStmt maybeStmt (AccTxn d newBal amount source)

-- | draw cash from account with a comment
draw :: Amount -> Date -> TxnComment -> Account -> Account
draw amount = deposit (- amount) 

-- | change reserve target info of account
updateReserveBalance :: ReserveAmount -> Account -> Account 
updateReserveBalance ra acc = acc {accType = Just ra}

-- | query total balance transfer from account a to account b
queryTrasnferBalance :: Account -> Account -> Balance
queryTrasnferBalance Account{accStmt = Nothing } Account{accName = an} = 0
queryTrasnferBalance a@Account{accName = fromAccName, accStmt = Just (Statement txns)} Account{accName = toAccName}
  = sum $ getTxnAmt <$> queryStmt a (Transfer fromAccName toAccName) 

instance QueryByComment Account where 
    queryStmt (Account _ _ _ _ Nothing) tc = []
    queryStmt (Account _ _ _ _ (Just (Statement txns))) tc = filter (\x -> getTxnComment x == tc) txns

$(deriveJSON defaultOptions ''InterestInfo)
$(deriveJSON defaultOptions ''ReserveAmount)
$(deriveJSON defaultOptions ''Account)
