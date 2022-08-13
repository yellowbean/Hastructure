module UT.LibTest(curveTests,queryStmtTests)
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
    r1 = queryStmtAmt (Just stmt1) "To:D|ABCD"
    --r2 = queryStmtAmt (Just stmt1) "To:[A-Z]\\|ABCD"
   in
    testCase "Query With Plain String" $
    assertEqual "Simple String Comment"
             [r1 ]
             [12 ]
  ]