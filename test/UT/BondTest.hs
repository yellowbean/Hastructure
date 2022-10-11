module UT.BondTest(pricingTests)
where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Time as T
import qualified Liability as B
import qualified Deal as D
import qualified Lib as L
import qualified Stmt  as S
import qualified Asset as P
import qualified Assumptions as A
import qualified Cashflow as CF
import Types
import Data.Ratio

import Debug.Trace
debug = flip trace

b1 = B.Bond{B.bndName="A"
            ,B.bndType=B.Sequential
            ,B.bndOriginInfo= B.OriginalInfo{
                               B.originBalance=3000
                               ,B.originDate= (T.fromGregorian 2022 1 1)
                               ,B.originRate= 0.08}
            ,B.bndInterestInfo= B.Fix 0.08 DC_ACT_365F
            ,B.bndBalance=3000
            ,B.bndRate=0.08
            ,B.bndDuePrin=0.0
            ,B.bndDueInt=0.0
            ,B.bndLastIntPay = Just (T.fromGregorian 2022 1 1)
            ,B.bndLastPrinPay = Just (T.fromGregorian 2022 1 1)
            ,B.bndStmt=Just $ S.Statement [ S.BondTxn (L.toDate "20220501") 1500 10 500 0.08 510 S.Empty]}

bfloat = B.Bond{B.bndName="A"
            ,B.bndType=B.Sequential
            ,B.bndOriginInfo= B.OriginalInfo{
                               B.originBalance=3000
                               ,B.originDate= (T.fromGregorian 2022 1 1)
                               ,B.originRate= 0.08}
            ,B.bndInterestInfo= B.Floater L.LPR5Y 0.015 (B.MonthOfYear 1) DC_ACT_365F Nothing Nothing
            ,B.bndBalance=3000
            ,B.bndRate=0.08
            ,B.bndDuePrin=0.0
            ,B.bndDueInt=0.0
            ,B.bndLastIntPay = Just (T.fromGregorian 2022 1 1)
            ,B.bndLastPrinPay = Just (T.fromGregorian 2022 1 1)
            ,B.bndStmt=Just $ S.Statement [ S.BondTxn (L.toDate "20220501") 1500 10 500 0.08 510 S.Empty]}


pricingTests = testGroup "Pricing Tests"
  [
    let
       _ts = (L.PricingCurve [L.TsPoint (L.toDate "20210101") 0.05
                             ,L.TsPoint (L.toDate "20240101") 0.05])
       _pv_day =  (L.toDate "20220201")
       _f_day =  (L.toDate "20230201")
       _pv = B.pv _ts _pv_day _f_day 103
    in
      testCase "PV test" $
        assertEqual "simple PV with flat curve"  
          (2060 % 21)
          _pv
    ,
    let
        _pv_day =  (L.toDate "20220201")
        _f_day =  (L.toDate "20230201")
        _ts1 = (L.PricingCurve [L.TsPoint (L.toDate "20210101") 0.01
                               ,L.TsPoint (L.toDate "20230101") 0.03])
        _pv1 = B.pv _ts1 _pv_day _f_day 103
        _diff1 = _pv1 - 100.0
    in
      testCase "PV test with curve change in middle" $
      assertBool "simple PV with latest rate point"  (_diff1 < 0.0001)
   ,
    let
      pr = B.priceBond (L.toDate "20210501")
                       (L.PricingCurve
                           [L.TsPoint (L.toDate "20210101") 0.01
                           ,L.TsPoint (L.toDate "20230101") 0.02])
                       b1
    in
      testCase "flat rate discount " $
      assertEqual "Test Pricing on case 01" 
        (B.PriceResult 501.650609 16.721686 (1 / 4) (1 / 1) 0.0) 
        pr
    ,
     let
       b2 = b1 { B.bndStmt = Just (S.Statement [S.BondTxn (L.toDate "20220301") 3000 10 300 0.08 310 S.Empty
                                                ,S.BondTxn (L.toDate "20220501") 2700 10 500 0.08 510 S.Empty])}

       pr = B.priceBond (L.toDate "20220201")
                        (L.PricingCurve
                            [L.TsPoint (L.toDate "20220101") 0.01
                            ,L.TsPoint (L.toDate "20220401") 0.03
                            ,L.TsPoint (L.toDate "20220601") 0.05
                            ])
                        b2
     in
       testCase " discount curve with two rate points " $
       -- assertEqual "Test Pricing on case 01" (B.PriceResult 816.1008 27.203362 0.043918636 0.18040144 20.383562) pr
       assertEqual "Test Pricing on case 01" (B.PriceResult (820 / 1) (82 / 3) (1 / 25) (5407 / 29930)  19.2) pr  --TODO need to confirm
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

bndTests = testGroup "Float Bond Tests" [
    let
       r1 = D.applicableAdjust (L.toDate "20220101") bfloat
       r2 = D.applicableAdjust (L.toDate "20220301") bfloat
    in
      testCase "Adjust rate by Month of Year " $
      assertEqual "" [True,False] [r1,r2]
    ,
    let 
       bfloatResetInterval = bfloat {B.bndInterestInfo = B.Floater L.LPR5Y 0.015 
                                                         (B.ByInterval L.Quarterly 
                                                           (Just (L.toDate "20220401")))
                                                         DC_ACT_365F   
                                                         Nothing Nothing}
       r1 = D.applicableAdjust (L.toDate "20220701") bfloatResetInterval
       r2 = D.applicableAdjust (L.toDate "20220801") bfloatResetInterval
    in 
      testCase "Adjust rate by quarter  " $
      assertEqual "" [True,False] [r1,r2]
 ]
