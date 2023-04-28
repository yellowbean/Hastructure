module UT.CashflowTest(cfTests,tsSplitTests)
where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Time as T
import qualified Lib as L
import qualified Asset as P
import qualified Assumptions as A
import qualified Cashflow as CF
import Types
import Util

import Debug.Trace
debug = flip trace
trs = [CF.MortgageFlow (L.toDate "20220101") 100 10 10 0 0 0 0 0 Nothing
      , CF.MortgageFlow (L.toDate "20220201") 90 10 10 0 0 0 0 0 Nothing
      , CF.MortgageFlow (L.toDate "20220211") 80 10 10 0 0 0 0 0 Nothing
      , CF.MortgageFlow (L.toDate "20220301") 70 10 10 0 0 0 0 0 Nothing]

cf = CF.CashFlowFrame trs

aggTs1 = CF.aggTsByDates trs [L.toDate "20220110"]
aggTs2 = CF.aggTsByDates trs [L.toDate "20220210"]
aggTs3 = CF.aggTsByDates trs [L.toDate "20220101",L.toDate "20220208"]
aggTs4 = CF.aggTsByDates trs [L.toDate "20220101",L.toDate "20220218"]

findLatestCf1 = CF.getTxnLatestAsOf cf (L.toDate "20220215")
findLatestCf2 = CF.getTxnLatestAsOf cf (L.toDate "20220315")
findLatestCf3 = CF.getTxnLatestAsOf cf (L.toDate "20210315")

cfTests = testGroup "Cashflow Utils"
  [
    testCase "Cashflow Aggregation only one return" $
     assertEqual "only one ts" 1 (length aggTs1)
   ,testCase "Cashflow Aggregation agg correct amount" $
     assertEqual "which bal is 100"
       (CF.MortgageFlow (L.toDate "20220110") 100 10 10 0 0 0 0 0 Nothing)
       (head aggTs1)
   ,testCase "Cashflow Aggregation Sum up" $
     assertEqual "Test Sum up" 1 (length aggTs2)
   ,testCase "Cashflow Aggregation agg correct amount" $
     assertEqual "which bal is 90"
       (CF.MortgageFlow (L.toDate "20220210") 90 20 20 0 0 0 0 0 Nothing)
       (head aggTs2)

   ,testCase "Cashflow Aggregation with two dates" $
     assertEqual "Test Sum up" 2 (length aggTs3)
   ,testCase "Cashflow Aggregation agg correct amount" $
     assertEqual "which bal is 90"
        [CF.MortgageFlow (L.toDate "20220101") 100 10 10 0 0 0 0 0 Nothing
        ,CF.MortgageFlow (L.toDate "20220208") 90 10 10 0 0 0 0 0  Nothing]
        aggTs3

   ,testCase "Cashflow Aggregation with two flows at second cutoff" $
     assertEqual "include two cf in one cutoff date"
       [CF.MortgageFlow (L.toDate "20220101") 100 10 10 0 0 0 0 0 Nothing
       ,CF.MortgageFlow (L.toDate "20220218") 80 20 20 0 0 0 0 0  Nothing]
       aggTs4

   ,testCase "Get Latest Cashflow 1" $
     assertEqual "Found one"
       (Just $ CF.MortgageFlow (L.toDate "20220211") 80 10 10 0 0 0 0 0 Nothing)
       --(Just $ CF.MortgageFlow (L.toDate "20220211") 80 10 10 0 0 0)
       findLatestCf1
   ,testCase "Get Latest Cashflow 2" $
     assertEqual "Found one"
       (Just (CF.MortgageFlow (L.toDate "20220301") 70 10 10 0 0 0 0 0 Nothing))
       findLatestCf2
   ,testCase "Get Latest Cashflow 3" $
     assertEqual "Nothing found"
       Nothing
       findLatestCf3
    ]


tsSplitTests = 
    let 
      cf1 = CF.CashFlow (L.toDate "20230101") 100
      cf2 = CF.CashFlow (L.toDate "20230201") 100
      cf3 = CF.CashFlow (L.toDate "20230301") 100
      cf4 = CF.CashFlow (L.toDate "20230401") 100
      ts1 = [cf1,cf2,cf3,cf4]
      ts2 = [cf1,cf2,cf2,cf3,cf4]
      cff = CF.CashFlowFrame [cf1,cf2,cf3,cf4]
    in 
      testGroup "Slice Time Series" 
      [ testCase "Cashflow" $
          assertEqual "by middle left"
          ([cf1,cf2],[cf3,cf4]) $
          splitByDate ts1 (L.toDate "20230215") EqToLeft
        ,testCase "Cashflow" $
          assertEqual "on left" 
          ([cf1,cf2,cf3],[cf4]) $
          splitByDate ts1 (L.toDate "20230301") EqToLeft
        ,testCase "Cashflow" $
          assertEqual "on right"
          ([cf1,cf2],[cf3,cf4]) $
          splitByDate ts1 (L.toDate "20230301") EqToRight
        ,testCase "Cashflow" $
          assertEqual "by middle right"
          ([cf1],[cf2, cf3,cf4]) $
          splitByDate ts1 (L.toDate "20230110") EqToRight
        ,testCase "Cashflow" $
          assertEqual "Keep previous one"
          ([cf1],[cf2, cf3,cf4]) $
          splitByDate ts1 (L.toDate "20230210") EqToLeftKeepOne
        ,testCase "Cashflow" $
          assertEqual "Keep previous one"
          ([],[cf1,cf2, cf3,cf4]) $
          splitByDate ts1 (L.toDate "20230201") EqToLeftKeepOne
        ,testCase "CashflowFrame" $ 
          assertEqual "Slice on Cashflow Frame"
          (CF.CashFlowFrame [cf1,cf2],CF.CashFlowFrame [cf3,cf4]) $
          CF.splitCashFlowFrameByDate cff (L.toDate "20230215") EqToLeft
        ,testCase "CashflowFrame" $ 
          assertEqual "Slice on Cashflow Frame"
          (CF.CashFlowFrame [cf1,cf2,cf3],CF.CashFlowFrame [cf4]) $
          CF.splitCashFlowFrameByDate cff (L.toDate "20230301") EqToLeft
        ,testCase "Range of Ts" $
          assertEqual "get subset of Ts between two dates"
          [cf2, cf3,cf4] $
          rangeBy ts1 (L.toDate "20230201") (L.toDate "20230401") II
        ,testCase "Range of Ts" $
          assertEqual "get subset of Ts between two dates"
          [cf3,cf4] $
          rangeBy ts1 (L.toDate "20230201") (L.toDate "20230401") EI
        ,testCase "Range of Ts" $
          assertEqual "get subset of Ts between two dates"
          [cf2, cf3] $
          rangeBy ts1 (L.toDate "20230201") (L.toDate "20230401") IE
        ,testCase "Range of Ts" $
          assertEqual "get subset of Ts between two dates"
          [cf3] $
          rangeBy ts1 (L.toDate "20230201") (L.toDate "20230401") EE
       -- ,testCase "Cashflow" $
       --   assertEqual "Keep previous ones"
       --   ([cf1],[cf2, cf2, cf3,cf4]) $
       --   splitByDate ts2 (L.toDate "20230210") EqToLeftKeepOnes
      ]

--cashAggTest = [
--    testGroup "Test on Combine Cashflow Frame"  $ [
--        testCase "" $ 
--            assertEqual "A"
--            1
--            1 
--        ,testCase "1"
--            assertEqual "B" 
--            1
--            1 
--        ]          
--    ]
