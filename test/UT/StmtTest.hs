module UT.StmtTest(txnTest)
where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Time as T
import Lib
import Util
import Stmt
import Data.Ratio
import Types

txnTest = 
    testGroup "Txn test" 
    [
       testCase "Weight Average Balance " $ 
           assertEqual "Weight Average Balacne " 
           7.44 $
           weightAvgBalance 
             (toDate "20221101") 
             (toDate "20221201")
             [(AccTxn (toDate "20221115") 100 20 Empty)]
      ,testCase "Weight Average Balance " $ 
           assertEqual "Weight Average Balacne "
           7.26 $
           weightAvgBalance 
             (toDate "20221101") 
             (toDate "20221201")
             [(AccTxn (toDate "20221115") 100 20 Empty)
             ,(AccTxn (toDate "20221125") 90 (negate 10) Empty)
             ]
      ,testCase "Weight Average Balance " $ 
           assertEqual "Weight Average Balacne by dates"
           [ 7.26, 5.64 ] $
           weightAvgBalanceByDates 
             [(toDate "20221101"),(toDate "20221201"),(toDate "20221225")]
             [(AccTxn (toDate "20221115") 100 20 Empty)
             ,(AccTxn (toDate "20221125") 90 (negate 10) Empty)
             ,(AccTxn (toDate "20221215") 80 (negate 10) Empty)]
    ]

