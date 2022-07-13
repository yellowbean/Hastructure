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
            ,B.bndStmt=Just $ L.Statement [
                                   -- L.BondTxn (T.fromGregorian 2022 1 1) 3000 10 500 0.08 510 ""
                                   --,L.BondTxn (T.fromGregorian 2022 2 1) 2500 10 500 0.08 510 ""
                                   --,L.BondTxn (T.fromGregorian 2022 4 1) 2000 10 500 0.08 510 ""
                                   L.BondTxn (L.toDate "20220501") 1500 10 500 0.08 510 ""]}

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
      pr = B.priceBond (L.toDate "20210501")
                       (L.FloatCurve
                           [L.TsPoint (L.toDate "20210101") 0.025
                           ,L.TsPoint (L.toDate "20230101") 0.025])
                       b1
    in
      testCase "flat rate discount " $
      assertEqual "Test Pricing on case 01" (B.PriceResult 497.56097 0.16666667 1.0) pr


  ]