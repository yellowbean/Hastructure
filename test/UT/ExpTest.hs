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
    ,
    let
     tsPoints = [(L.TsPoint (L.toDate "20220101") 10.0)
                 ,(L.TsPoint (L.toDate "20220301") 15.0)
                 ,(L.TsPoint (L.toDate "20220601") 20.0)]
     f1 = Fee "FeeName1" (Custom (L.AmountCurve tsPoints)) (L.toDate "20210101") 0 Nothing 0 Nothing Nothing
     _calcDate = (L.toDate "20220321")
     _calcDate2 = (L.toDate "20220621")
     _calcDate3 = (L.toDate "20211221")
     f1_ = Fee "FeeName1" (Custom (L.AmountCurve [(L.TsPoint (L.toDate "20220601") 20.0)])) (L.toDate "20210101") 25 (Just (L.toDate "20220321")) 0 Nothing Nothing
     f2_ = f1 {feeDue = 45.0, feeDueDate = Just _calcDate2, feeType = Custom (L.AmountCurve [])}
     f3_ = f1 {feeDue = 0, feeDueDate = Just _calcDate3}

     f1WithDue = Fee "FeeName1" (Custom (L.AmountCurve tsPoints)) (L.toDate "20210101") 3 Nothing 0 Nothing Nothing
     _f1WithDue = f1WithDue {feeType= Custom (L.AmountCurve [(L.TsPoint (L.toDate "20220601") 20.0)]), feeDue = 28, feeDueDate = Just _calcDate}
    in
      testCase "test on Custom Fee Type" $
      assertEqual "calc Due Fee" [f1_ , f2_ , f3_ , _f1WithDue]
                                 [D.calcDueFee DT.td _calcDate f1
                                 ,D.calcDueFee DT.td _calcDate2 f1
                                 ,D.calcDueFee DT.td _calcDate3 f1
                                 ,D.calcDueFee DT.td _calcDate f1WithDue
                                 ]

  ]