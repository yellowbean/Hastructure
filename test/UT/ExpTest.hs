module UT.ExpTest(expTests)
where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Time as T
import qualified Lib as L
import qualified Asset as P
import qualified Deal as D
import qualified Deal.DealAction as DA
import qualified UT.DealTest as DT
import Expense
import Types
import qualified Cashflow as CF

import Debug.Trace
debug = flip trace


expTests =  testGroup "Expense Tests"
  [
    let
     f1 = Fee "FeeName1" (RecurFee MonthFirst 50) (L.toDate "20220101") 0 Nothing 0 Nothing Nothing
     f2 = Fee "FeeNameAccum" (RecurFee MonthFirst 50) (L.toDate "20220101") 60 (Just (L.toDate "20220310")) 0 Nothing Nothing
     _calcDate = (L.toDate "20220310")
     _calcDate2 = (L.toDate "20220115")
     _calcDate3 = (L.toDate "20220415")
     feesCalc = sequenceA [(DA.calcDueFee DT.td2 _calcDate f1) ,(DA.calcDueFee DT.td2 _calcDate2 f1) ,(DA.calcDueFee DT.td2 _calcDate3 f2) ,(DA.calcDueFee DT.td2 _calcDate3 f1)]
    in
      testCase "calc on diff same period for recur fee" $
      assertEqual
        "test date"
        (Right [100.0, 0.0, 110.0, 150.0])
        ((feeDue <$>) <$> feesCalc)
    ,
    let
     tsPoints = [(L.TsPoint (L.toDate "20220101") 10.0)
                 ,(L.TsPoint (L.toDate "20220301") 15.0)
                 ,(L.TsPoint (L.toDate "20220601") 20.0)]
     f1 = Fee "FeeName1" (FeeFlow (L.BalanceCurve tsPoints)) (L.toDate "20210101") 0 Nothing 0 Nothing Nothing
     _calcDate = (L.toDate "20220321")
     _calcDate2 = (L.toDate "20220621")
     _calcDate3 = (L.toDate "20211221")
     f1_ = Fee "FeeName1" (FeeFlow (L.BalanceCurve [(L.TsPoint (L.toDate "20220601") 20.0)])) (L.toDate "20210101") 25 (Just (L.toDate "20220321")) 0 Nothing Nothing
     f2_ = f1 {feeDue = 45.0, feeDueDate = Just _calcDate2, feeType = FeeFlow (L.BalanceCurve [])}
     f3_ = f1 {feeDue = 0, feeDueDate = Just _calcDate3}

     f1WithDue = Fee "FeeName1" (FeeFlow (L.BalanceCurve tsPoints)) (L.toDate "20210101") 3 Nothing 0 Nothing Nothing
     _f1WithDue = f1WithDue {feeType= FeeFlow (L.BalanceCurve [(L.TsPoint (L.toDate "20220601") 20.0)]), feeDue = 28, feeDueDate = Just _calcDate}
     feesCalc = sequenceA [DA.calcDueFee DT.td2 _calcDate f1
                          ,DA.calcDueFee DT.td2 _calcDate2 f1
                          ,DA.calcDueFee DT.td2 _calcDate3 f1
                          ,DA.calcDueFee DT.td2 _calcDate f1WithDue ]
    in
      testCase "test on Custom Fee Type" $
      assertEqual "calc Due Fee" (Right [f1_ , f2_ , f3_ , _f1WithDue]) feesCalc

  ]

--cnVatFeeTest = 
--  let 
--    vatFeeInt = Fee "VatFee" 
--                  (PctFee (PoolCollectionIncome CollectedInterest) 0.0325) 
--                  (L.toDate "20220101") 0 Nothing 0 Nothing Nothing
--    poolFlows = CashFlowFrame $ [MortgageFlow (toDate "20220101") 100 20 15 0 0 0 0 0.01
--                                 ,MortgageFlow (toDate "20220101") 100 20 15 0 0 0 0 0.01
--                                 ]
--  in 
--    testGroup "China VAT fee test" $
--    [
--
--
--    ]
   
