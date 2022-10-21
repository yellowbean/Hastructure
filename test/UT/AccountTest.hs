module UT.AccountTest(intTests,reserveAccTest)
where

import Test.Tasty
import Test.Tasty.HUnit
import Accounts 
import Lib
import Stmt
import Util
import Types
import Deal
import qualified Cashflow as CF

import qualified Data.Time as T
import qualified Data.Map as Map
import UT.DealTest (td)

intTests =
  let 
    acc1 = Account 200 "A1" (Just (BankAccount 0.03 (toDate "20221001") QuarterEnd)) Nothing Nothing
    acc2 = Account 150 "A1" (Just (BankAccount 0.03 (toDate "20220301") MonthEnd)) Nothing 
          (Just (Statement [ AccTxn (toDate "20220715") 120 10 Empty
                          ,AccTxn (toDate "20220915") 150 30 Empty ]))
  in 
    testGroup "Interest on Bank Account Test"
     [
      testCase "Build EarnIntAction" $
        assertEqual "QuarterEnd" 
          [("A1",(genSerialDates QuarterEnd (toDate "20221001") 5))] $ 
          buildEarnIntAction [acc1] (toDate "20231231") []
      ,testCase "Build EarnIntAction Same Year" $
        assertEqual "QuarterEnd Same Year" 
          [("A1",(genSerialDates QuarterEnd (toDate "20221001") 1))] $ 
          buildEarnIntAction [acc1] (toDate "20221231") []
      ,testCase "Validate Interest Calculation 1" $
        assertEqual "MonthEnd with No txn"
        200.5
        (accBalance (depositInt acc1 (toDate "20221101")))
      ,testCase "Validate Interest Calculation 2" $
        assertEqual "MonthEnd with txns"
        152.42
        (accBalance (depositInt acc2 (toDate "20221101")))
     ]

reserveAccTest = 
  let 
    acc1 = Account 200 "A1" Nothing (Just (PctReserve CurrentPoolBalance 0.01)) Nothing
    acc2 = Account 150 "A2" Nothing (Just (FixReserve 210)) Nothing
    accMap = Map.fromList [("A1",acc1),("A2",acc2)]
    testCFs = CF.CashFlowFrame $
               [CF.MortgageFlow (toDate "20220601") 150 20 10 0 0 0 0 0
               ,CF.MortgageFlow (toDate "20220701") 130 20 10 0 0 0 0 0
               ,CF.MortgageFlow (toDate "20220801") 110 20 10 0 0 0 0 0
               ,CF.MortgageFlow (toDate "20220901") 90 20 10 0 0 0 0 0
               ,CF.MortgageFlow (toDate "20221001") 70 20 10 0 0 0 0 0]
    ttd = (setFutureCF td testCFs) {accounts = accMap}
  in 
    testGroup "Test On Reserve Acc"
     [
      testCase "Test on Pct Reserve" $
        assertEqual "shall be " 
          1.1
          (calcTargetAmount ttd (toDate "20220826") acc1)
     ,testCase "Test on fix Reserve" $
        assertEqual "shall be " 
          210
          (calcTargetAmount ttd (toDate "20220801") acc2)
     ,testCase "test on reserve account gap" $
        assertEqual "pct reserve gap "
        0
        (queryDeal ttd (ReserveAccGapAt (toDate "20220826") ["A1"]))
     ,testCase "test on reserve account gap" $
        assertEqual "fix reserve gap "
        60
        (queryDeal ttd (ReserveAccGapAt (toDate "20220801") ["A2"]))
     ]


