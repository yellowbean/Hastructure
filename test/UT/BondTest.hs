module UT.BondTest(pricingTests)
where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Time as T
import qualified Liability as B
import qualified Lib as L
import qualified Asset as P
import qualified Assumptions as A
import qualified Cashflow as CF

import Debug.Trace
debug = flip trace

b1 = B.Bond{B.bndName="A"
            ,B.bndType=B.Sequential
            ,B.bndOriginInfo= B.OriginalInfo{
                               B.originBalance=3000
                               ,B.originDate= (T.fromGregorian 2022 1 1)
                               ,B.originRate= 0.08}
            ,B.bndInterestInfo= B.Fix 0.08
            ,B.bndBalance=3000
            ,B.bndRate=0.08
            ,B.bndDuePrin=0.0
            ,B.bndDueInt=0.0
            ,B.bndLastIntPay = Just (T.fromGregorian 2022 1 1)
            ,B.bndLastPrinPay = Just (T.fromGregorian 2022 1 1)
            ,B.bndStmt=Just $ L.Statement [ L.BondTxn (L.toDate "20220501") 1500 10 500 0.08 510 ""]}


pricingTests = testGroup "Pricing Tests"
  [
    let
       _ts = (L.FloatCurve [L.TsPoint (L.toDate "20210101") 0.05
                           ,L.TsPoint (L.toDate "20240101") 0.05])
       _pv_day =  (L.toDate "20220201")
       _f_day =  (L.toDate "20230201")
       _pv = B.pv _ts _pv_day _f_day 103
       _diff = _pv - 98.09524
    in
      testCase "PV test" $
      assertBool "simple PV with flat curve"  (_diff < 0.0001)
    ,
    let
        _pv_day =  (L.toDate "20220201")
        _f_day =  (L.toDate "20230201")
        _ts1 = (L.FloatCurve [L.TsPoint (L.toDate "20210101") 0.01
                            ,L.TsPoint (L.toDate "20230101") 0.03])
        _pv1 = B.pv _ts1 _pv_day _f_day 103
        _diff1 = _pv1 - 100.0
    in
      testCase "PV test with curve change in middle" $
      assertBool "simple PV with latest rate point"  (_diff1 < 0.0001)
   ,
    let
      pr = B.priceBond (L.toDate "20210501")
                       (L.FloatCurve
                           [L.TsPoint (L.toDate "20210101") 0.01
                           ,L.TsPoint (L.toDate "20230101") 0.02])
                       b1
    in
      testCase "flat rate discount " $
      assertEqual "Test Pricing on case 01" (B.PriceResult 504.9505 16.831684 0.16666667 1.0 ) pr
    ,
     let
       b2 = b1 { B.bndStmt = Just (L.Statement [L.BondTxn (L.toDate "20220301") 3000 10 300 0.08 310 ""
                                                ,L.BondTxn (L.toDate "20220501") 2700 10 500 0.08 510 ""])}

       pr = B.priceBond (L.toDate "20220201")
                        (L.FloatCurve
                            [L.TsPoint (L.toDate "20220101") 0.01
                            ,L.TsPoint (L.toDate "20220401") 0.03
                            ,L.TsPoint (L.toDate "20220601") 0.05
                            ])
                        b2
     in
       testCase " discount curve with two rate points " $
       assertEqual "Test Pricing on case 01" (B.PriceResult 816.1008 27.203362 0.0483105 0.18040144) pr
    ,
    let
      b3 = b1 {B.bndStmt = Nothing,B.bndInterestInfo = B.InterestByYield 0.02}
    in
      testCase "pay interest to satisfy on yield" $
      assertEqual "" 60 (B.backoutDueIntByYield (L.toDate "20230101") b3)
    ,
    let
      b4 = b1
      pday = L.toDate "20220801"
    in
      testCase "pay prin to a bond" $
      assertEqual "pay down prin" 2400  $ B.bndBalance (B.payPrin pday 600 b4)
    ,
    let
      b5 = b1
      pday = L.toDate "20220801"
    in
      testCase "pay int to 2 bonds" $
      assertEqual "pay int" 2400  $ B.bndBalance (B.payPrin pday 600 b5)
  ]