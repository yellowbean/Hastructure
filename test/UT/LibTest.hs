module UT.LibTest(curveTests,queryStmtTests,datesTests)
where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Time as T
import Lib

curveTests =testGroup "Curve Tests"
  [
    let
     _ts = (FloatCurve [TsPoint (toDate "20210101") 0.01
                         ,TsPoint (toDate "20230101") 0.02])
     _r1 = getValByDate _ts (toDate "20201231")
     _r2 = getValByDate _ts (toDate "20210201")
     _r3 = getValByDate _ts (toDate "20230102")
     _r4 = getValByDate _ts (toDate "20231231")
    in
      testCase "Query interst rate curve by date" $
      assertEqual
        "test 4 dates"
        [_r1,_r2,_r3,_r4] [0, 0.01,0.02,0.02]
  ]

queryStmtTests = testGroup "queryStmtTest"
  [
   let
    stmt1 = Statement [
             AccTxn (toDate "20200101") 100 (-12) "To:D|ABCD"
             ,AccTxn (toDate "20200101") 100 10 ""
             ,AccTxn (toDate "20200101") 100 (-20) "To:C|ABCD" ]
    r1 = queryStmtAmt (Just stmt1) "To:D\\|ABCD"
    r2 = queryStmtAmt (Just stmt1) "To:[A-Z]\\|ABCD"
    r3 = queryStmtAmt Nothing "To:[A-Z]\\|ABCD"
   in
    testCase "Query With Plain String" $
    assertEqual "Simple String Comment"
             [r1,r2,r3]
             [12,32,0]
  ]

datesTests = testGroup "date related "
  [
   let
      d1 = genDates (toDate "20220801") Monthly 1
   in
     testCase "1 Month" $
     assertEqual "1 Month" [toDate "20220901"] d1
   ,
   let
      d2 = genDates (toDate "20220801") Monthly 0
   in
     testCase "zero extra" $
     assertEqual "1 Month" [] d2

  ]