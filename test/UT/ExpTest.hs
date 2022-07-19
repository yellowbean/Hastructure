module UT.ExpTest(expTests)
where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Time as T
import qualified Lib as L
import qualified Asset as P
import qualified Deal as D
import qualified UT.DealTest as DT
import Expense
import qualified Cashflow as CF

import Debug.Trace
debug = flip trace


expTests =  testGroup "Expense Tests"
  [
    let
     f1 = Fee "FeeName1" (RecurFee L.Monthly 50) (L.toDate "20220101") 0 Nothing 0 Nothing Nothing
     f1_ = Fee "FeeName1" (RecurFee L.Monthly 50) (L.toDate "20220101") 100 (Just (L.toDate "20220301")) 0 Nothing Nothing
     f1_3m = Fee "FeeName1" (RecurFee L.Monthly 50) (L.toDate "20220101") 150 (Just (L.toDate "20220415")) 0 Nothing Nothing
     f2 = Fee "FeeNameAccum" (RecurFee L.Monthly 50) (L.toDate "20220101") 60 (Just (L.toDate "20220301")) 0 Nothing Nothing
     f2_ = Fee "FeeNameAccum" (RecurFee L.Monthly 50) (L.toDate "20220101") 110 (Just (L.toDate "20220415")) 0 Nothing Nothing
     _calcDate = (L.toDate "20220301")
     _calcDate2 = (L.toDate "20220315")
     _calcDate3 = (L.toDate "20220415")
    in
      testCase "calc on diff/same period for recur fee" $
      assertEqual
        "test dates"
        [f1_
        ,f1_
        ,f2_
        ,f1_3m]
        [(D.calcDueFee DT.td _calcDate f1)
         ,(D.calcDueFee DT.td _calcDate2 f1_)
         ,(D.calcDueFee DT.td _calcDate3 f2)
         ,(D.calcDueFee DT.td _calcDate3 f1)
         ]
  ]