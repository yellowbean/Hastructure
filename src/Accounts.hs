{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Accounts (Account(..),ReserveAmount(..),draw,deposit
                ,transfer,depositInt
                ,InterestInfo(..),buildEarnIntAction,updateReserveBalance
                ,accBalLens,tryDraw,buildRateResetDates,accrueInt)
    where
import qualified Data.Time as T
import Stmt (Statement(..),appendStmt,getTxnBegBalance,getDate
            ,TxnComment(..),QueryByComment(..),getTxnComment,getTxnAmt,weightAvgBalanceByDates)
import Types
import Lib
import Util
import DateUtil
import Data.Aeson hiding (json)
import Language.Haskell.TH
import Data.Aeson.TH
import Data.Aeson.Types
import GHC.Generics

import Control.Lens hiding (Index)

import qualified InterestRate as IR

import Debug.Trace
debug = flip trace

data InterestInfo = BankAccount IRate DatePattern Date                
                    -- ^ fix reinvest return rate
                  | InvestmentAccount Types.Index Spread DatePattern DatePattern Date IRate 
                    -- ^ float type: index, spread, sweep dates, rate reset , last accrue day, last reset rate
                  deriving (Show, Generic,Eq,Ord)


makePrisms ''InterestInfo


data ReserveAmount = PctReserve DealStats Rate               -- ^ target amount with reference to % of formula
                   | FixReserve Balance                      -- ^ target amount with fixed balance amount    
                   | Either Pre ReserveAmount ReserveAmount  -- ^ target amount depends on a test, if true, then use first one ,otherwise use second one
                   | Max [ReserveAmount]                     -- ^ use higher of all reserve formulas
                   | Min [ReserveAmount]                     -- ^ use lower of all reserve formulas
                   deriving (Show, Eq, Generic, Ord)

data Account = Account {
    accBalance :: Balance                 -- ^ account current balance
    ,accName :: String                    -- ^ account name
    ,accInterest :: Maybe InterestInfo    -- ^ account reinvestment interest
    ,accType :: Maybe ReserveAmount       -- ^ target info if a reserve account
    ,accStmt :: Maybe Statement           -- ^ transactional history
} deriving (Show, Generic,Eq, Ord)

-- | build interest earn actions
buildEarnIntAction :: [Account] -> Date -> [(String,Dates)] -> [(String,Dates)]
buildEarnIntAction [] ed r = r
buildEarnIntAction (acc:accs) ed r = 
  case accInterest acc of 
    Nothing -> buildEarnIntAction accs ed r
    Just (BankAccount _ dp lastAccDate ) 
      -> buildEarnIntAction accs ed [(accName acc, genSerialDatesTill2 NO_IE lastAccDate dp ed)]++r    
    Just (InvestmentAccount _ _ dp _ lastAccDate _) 
      -> buildEarnIntAction accs ed [(accName acc, genSerialDatesTill2 NO_IE lastAccDate dp ed)]++r    

accrueInt :: Date -> Account -> Balance
accrueInt _ (Account _ _ Nothing _ _) = 0 
-- ^ bank account type interest 
accrueInt endDate a@(Account bal _ (Just interestType) _ stmt)  
  = case stmt of 
       Nothing -> mulBR (mulBI bal rateToUse) (yearCountFraction defaultDc lastDay endDate) -- `debug` (">>"++show lastCollectDate++">>"++show ed)
       Just (Statement txns) ->
         let 
           accrueTxns = sliceBy IE lastDay endDate txns
           bals = map getTxnBegBalance accrueTxns ++ [bal]
           ds = [lastDay] ++ getDates accrueTxns ++ [endDate]
           avgBal = calcWeightBalanceByDates defaultDc bals ds
         in
           mulBI avgBal rateToUse  
    where 
      defaultDc = DC_30E_360
      (lastDay,rateToUse) = case interestType of 
                              (BankAccount r dp lastCollectDate) -> (lastCollectDate, r)
                              (InvestmentAccount idx spd dp _ lastCollectDate lastRate) -> (lastCollectDate, lastRate)

-- | sweep interest/investement income into account
depositInt :: Date -> Account -> Account
depositInt _ a@(Account _ _ Nothing _ _) = a 
depositInt ed a@(Account bal _ (Just intType) _ stmt)
          = a {accBalance = newBal ,accStmt= newStmt ,accInterest = Just (newIntInfoType intType)}
          where 
            -- accruedInt = accrueInt a (mkTs [(lastCollectDate, toRational r),(ed, toRational r)])  ed
            accruedInt = accrueInt ed a
            newIntInfoType (BankAccount x y _d) = BankAccount x y ed
            newIntInfoType (InvestmentAccount x y z z1 _d z2) = (InvestmentAccount x y z z1 ed z2)
            newBal = accruedInt + bal  -- `debug` ("INT ACC->"++ show accrued_int)
            newTxn = AccTxn ed newBal accruedInt BankInt
            newStmt = appendStmt stmt newTxn

-- | move cash from account A to account B
transfer :: (Account,Account) -> Date -> Amount -> (Account, Account)
transfer (sourceAcc@(Account sBal san _ _ sStmt), targetAcc@(Account tBal tan _ _ tStmt))
         d
         amount
  = (sourceAcc {accBalance = newSBal, accStmt = sourceNewStmt}
    ,targetAcc {accBalance = newTBal, accStmt = targetNewStmt})
  where
    newSBal = sBal - amount
    newTBal = tBal + amount
    sourceNewStmt = appendStmt sStmt (AccTxn d newSBal (- amount) (Transfer san tan))
    targetNewStmt = appendStmt tStmt (AccTxn d newTBal amount (Transfer san tan) )

-- | deposit cash to account with a comment
deposit :: Amount -> Date -> TxnComment -> Account -> Account
deposit amount d source acc@(Account bal _ _ _ maybeStmt)  =
    acc {accBalance = newBal, accStmt = newStmt}
  where
    newBal = bal + amount -- `debug` ("Date:"++show d++ "deposit"++show amount++"from"++show bal)
    newStmt = appendStmt maybeStmt (AccTxn d newBal amount source)

-- | draw cash from account with a comment
draw :: Amount -> Date -> TxnComment -> Account -> Account
draw amount d txn acc@Account{ accBalance = bal ,accName = an} 
  | bal >= amount = deposit (- amount) d txn acc  
  | otherwise = error  $ "Date:"++ show d ++" Failed to draw "++ show amount ++" from account" ++ an

-- | draw cash from account with a comment,return shortfall and acccount 
tryDraw :: Amount -> Date -> TxnComment -> Account -> ((Amount,Amount),Account)
tryDraw amt d tc acc@(Account bal _ _ _ maybeStmt) 
  | amt > bal = ((amt - bal, bal), acc {accBalance = 0})
  | otherwise = ((0, amt), draw amt d tc acc)


-- | change reserve target info of account
updateReserveBalance :: ReserveAmount -> Account -> Account 
updateReserveBalance ra acc = acc {accType = Just ra}

instance QueryByComment Account where 
    queryStmt (Account _ _ _ _ Nothing) tc = []
    queryStmt (Account _ _ _ _ (Just (Statement txns))) tc = filter (\x -> getTxnComment x == tc) txns

-- | query total balance transfer from account a to account b
queryTrasnferBalance :: Account -> Account -> Balance
queryTrasnferBalance Account{accStmt = Nothing } Account{accName = an} = 0
queryTrasnferBalance a@Account{accName = fromAccName, accStmt = Just (Statement txns)} Account{accName = toAccName}
  = sum $ getTxnAmt <$> queryStmt a (Transfer fromAccName toAccName) 


-- InvestmentAccount Types.Index Spread DatePattern DatePattern Date IRate 
buildRateResetDates :: Date -> Account -> Maybe (String,Dates)
buildRateResetDates ed Account{accName = n, accInterest = Just (InvestmentAccount _ _ _ dp sd _) }
  = Just (n, genSerialDatesTill2 NO_IE sd dp ed)
buildRateResetDates _ _ = Nothing


makeLensesFor [("accBalance","accBalLens") ,("accName","accNameLens") 
              ,("accType","accTypeLens") ,("accStmt","accStmtLens")] ''Account


instance IR.UseRate Account where 
  isAdjustbleRate (Account _ an (Just (InvestmentAccount _ _ _ _ _ _)) _ _) = True
  isAdjustbleRate _ = False

  getIndex (Account _ an (Just (InvestmentAccount idx _ _ _ _ _)) _ _) = Just idx
  getIndex _ = Nothing 
  


$(deriveJSON defaultOptions ''InterestInfo)
$(deriveJSON defaultOptions ''ReserveAmount)
$(deriveJSON defaultOptions ''Account)
