{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Accounts (Account(..),ReserveAmount(..),draw,deposit
                ,transfer,depositInt,depositIntByCurve
                ,InterestInfo(..),buildEarnIntAction,updateReserveBalance
                ,accBalLens)
    where
import qualified Data.Time as T
import Stmt (Statement(..),appendStmt,Txn(..),getTxnBegBalance,getDate
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

import Control.Lens 

import qualified InterestRate as IR

import Debug.Trace
debug = flip trace

data InterestInfo = BankAccount IRate Date DatePattern                -- ^ fix reinvest return rate
                  | InvestmentAccount Types.Index Spread Date DatePattern   -- ^ float reinvest return rate (index,spread, dp)
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
  case accInterest acc of 
    Nothing -> buildEarnIntAction accs ed r
    Just (BankAccount _ lastAccDate dp) 
      -> buildEarnIntAction accs ed [(accName acc, genSerialDatesTill2 NO_IE lastAccDate dp ed)]++r    
    Just (InvestmentAccount _ _ lastAccDate dp) 
      -> buildEarnIntAction accs ed [(accName acc, genSerialDatesTill2 NO_IE lastAccDate dp ed)]++r    

-- | sweep interest/investement income into account
depositInt :: Account -> Date -> Account
depositInt a@(Account _ _ Nothing _ _) _ = a
depositInt a@(Account bal _ (Just (BankAccount r lastCollectDate dp)) _ stmt) ed 
          = a {accBalance = newBal ,accStmt= newStmt ,accInterest = Just (BankAccount r ed dp)}
          where 
            accruedInt = case stmt of 
                            Nothing -> mulBR (mulBI bal r) (yearCountFraction DC_30E_360 lastCollectDate ed) -- `debug` (">>"++show lastCollectDate++">>"++show ed)
                            Just (Statement txns) ->
                              let 
                                accrueTxns = sliceBy IE lastCollectDate ed txns
                                bals = map getTxnBegBalance accrueTxns ++ [bal] -- `debug` ("ACCU TXN"++show _accrue_txns)
                                dfs = getIntervalFactors $ [lastCollectDate] ++ (getDates accrueTxns) ++ [ed]
                              in
                                mulBI (sum $ zipWith mulBR bals dfs) r  

            newBal = accruedInt + bal  -- `debug` ("INT ACC->"++ show accrued_int)
            newTxn = AccTxn ed newBal accruedInt BankInt
            newStmt = appendStmt stmt newTxn

-- | sweep interest/investement income into account by a yield curve
depositIntByCurve :: Account -> Ts -> Date -> Account
depositIntByCurve a@(Account bal _ (Just (InvestmentAccount idx spd lastCollectDate dp)) _ stmt)
                  rc
                  ed 
          = a {accBalance = newBal ,accStmt= newStmt ,accInterest = Just (InvestmentAccount idx spd ed dp)}
          where 
            accruedInt = let 
                           curveDs = [lastCollectDate] ++ subDates EE lastCollectDate ed (getTsDates rc) ++ [ed]
                           curveVs = (\x -> toRational (getValByDate rc Exc x) + toRational spd) <$> init curveDs
                         in
                           case stmt of 
                             Nothing -> 
                               let  
                                 dsFactor = getIntervalFactors curveDs
                                 weightInt = sum $ zipWith (*) curveVs dsFactor --  `debug` ("ds"++show curve_ds++"vs"++show curve_vs++"factors"++show ds_factor)
                               in 
                                 mulBR bal weightInt
                             Just (Statement txns) ->
                               let 
                                 bals = weightAvgBalanceByDates curveDs txns
                               in
                                 sum $ zipWith mulBR bals curveVs -- `debug` ("cds"++show curve_ds++"vs"++ show curve_vs++"bs"++show bals)

            newBal = accruedInt + bal 
            newTxn = AccTxn ed newBal accruedInt BankInt
            newStmt = appendStmt stmt newTxn


-- | move cash from account A to account B
transfer :: Account -> Amount -> Date -> Account -> (Account, Account)
transfer sourceAcc@(Account sBal san _ _ sStmt)
         amount
         d
         targetAcc@(Account tBal tan _ _ tStmt)
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

makeLensesFor [("accBalance","accBalLens") ,("accName","accNameLens") ,("accType","accTypeLens") ,("accStmt","accStmtLens")] ''Account


instance QueryByComment Account where 
    queryStmt (Account _ _ _ _ Nothing) tc = []
    queryStmt (Account _ _ _ _ (Just (Statement txns))) tc = filter (\x -> getTxnComment x == tc) txns

instance IR.UseRate Account where 
  isAdjustbleRate (Account _ an (Just (InvestmentAccount _ _ lastAccDate dp)) _ _) = True
  isAdjustbleRate _ = False

  getIndex (Account _ an (Just (InvestmentAccount idx _ _ _)) _ _) = Just idx
  getIndex _ = Nothing 
  


$(deriveJSON defaultOptions ''InterestInfo)
$(deriveJSON defaultOptions ''ReserveAmount)
$(deriveJSON defaultOptions ''Account)
