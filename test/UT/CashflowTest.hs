module UT.CashflowTest(cfTests)
where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Time as T
import qualified Lib as L
import qualified Asset as P
import qualified Assumptions as A
import qualified Cashflow as CF

import Debug.Trace
debug = flip trace
trs = [CF.MortgageFlow (L.toDate "20220101") 100 10 10 0 0 0
      , CF.MortgageFlow (L.toDate "20220201") 90 10 10 0 0 0
      , CF.MortgageFlow (L.toDate "20220211") 80 10 10 0 0 0
      , CF.MortgageFlow (L.toDate "20220301") 70 10 10 0 0 0]

aggTs1 = CF.aggTsByDates trs [L.toDate "20220110"]
aggTs2 = CF.aggTsByDates trs [L.toDate "20220210"]
aggTs3 = CF.aggTsByDates trs [L.toDate "20220101",L.toDate "20220208"]
aggTs4 = CF.aggTsByDates trs [L.toDate "20220101",L.toDate "20220218"]

cfTests = testGroup "Cashflow Utils"
  [
    testCase "Cashflow Aggregation only one return" $
     assertEqual "only one ts" 1 (length aggTs1)
   ,testCase "Cashflow Aggregation agg correct amount" $
     assertEqual "which bal is 100"
       (CF.MortgageFlow (L.toDate "20220110") 100 10 10 0 0 0)
       (head aggTs1)

   ,testCase "Cashflow Aggregation Sum up" $
     assertEqual "Test Sum up" 1 (length aggTs2)
   ,testCase "Cashflow Aggregation agg correct amount" $
     assertEqual "which bal is 90"
       (CF.MortgageFlow (L.toDate "20220210") 90 20 20 0 0 0)
       (head aggTs2)

   ,testCase "Cashflow Aggregation with two dates" $
     assertEqual "Test Sum up" 2 (length aggTs3)
   ,testCase "Cashflow Aggregation agg correct amount" $
      assertEqual "which bal is 90"
        [CF.MortgageFlow (L.toDate "20220101") 100 10 10 0 0 0
        ,CF.MortgageFlow (L.toDate "20220208") 90 10 10 0 0 0]
        aggTs3

   ,testCase "Cashflow Aggregation with two flows at second cutoff" $
       assertEqual "include two cf in one cutoff date"
         [CF.MortgageFlow (L.toDate "20220101") 100 10 10 0 0 0
         ,CF.MortgageFlow (L.toDate "20220218") 80 20 20 0 0 0]
         aggTs4
    ]