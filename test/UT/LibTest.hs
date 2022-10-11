module UT.LibTest(curveTests
                 --,queryStmtTests
                 ,datesTests
                 ,prorataTests)
where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Time as T
import Lib
import Util
import Stmt
import Data.Ratio

curveTests =
    let
     _ts = (FloatCurve [TsPoint (toDate "20210101") 0.01
                         ,TsPoint (toDate "20230101") 0.02])
     _r1 = getValByDate _ts (toDate "20201231")
     _r2 = getValByDate _ts (toDate "20210201")
     _r3 = getValByDate _ts (toDate "20230102")
     _r4 = getValByDate _ts (toDate "20231231")

     _priceTs = (PricingCurve
                  [TsPoint (toDate "20210101") 0.01
                  ,TsPoint (toDate "20210110") 0.02])
    in
  testGroup "Curve Tests"
  [
    testCase "Query interst rate curve by date" $
      assertEqual
        "test 4 dates"
        [_r1,_r2,_r3,_r4] [0, 0.01,0.02,0.02]
    ,testCase "Pricing Curve Test1" $
      assertEqual "left"
        0.01
        (getValByDate _priceTs (toDate "20201231"))
    ,testCase "Pricing Curve Test2" $
      assertEqual "Right"
        0.02
        (getValByDate _priceTs (toDate "20210121"))
    ,testCase "Pricing Curve Test3" $
      assertEqual "Mid"
        (13 % 900)
        (getValByDate _priceTs (toDate "20210105"))
  ]

--queryStmtTests = testGroup "queryStmtTest"
--  [
--   let
--    stmt1 = Statement [
--             AccTxn (toDate "20200101") 100 (-12) Empty
--             ,AccTxn (toDate "20200101") 100 10 Empty
--             ,AccTxn (toDate "20200101") 100 (-20) Empty]
--    r1 = queryStmtAmt (Just stmt1) Empty
--    r2 = queryStmtAmt (Just stmt1) Empty
--    r3 = queryStmtAmt Nothing Empty
--   in
--    testCase "Query With Plain String" $
--    assertEqual "Simple String Comment"
--             [r1,r2,r3]
--             [12,32,0]
--  ]

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

prorataTests = testGroup "prorata Test"
  [
    let 
      bals1 = [100,200,300]
    in 
      testCase "3 bals" $
        assertEqual ""
          [10,20,30]
          (prorataFactors bals1 60)
    ,
    let 
      bals2 = [100,200,0]
    in 
      testCase "2 bals" $
        assertEqual ""
          [20,40,0]
          (prorataFactors bals2 60)
  ]