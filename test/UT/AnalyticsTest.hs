module UT.AnalyticsTest(walTest,durationTest,fvTest,assetPricingTest,irrTest)
where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Time as T
import qualified Lib as L
import Analytics 
import Assumptions
import Types
import Asset (priceAsset)
import AssetClass.AssetBase
import AssetClass.Loan
import InterestRate

import Data.Ratio

walTest = 
  let 
    _ps = [(50,L.toDate "20230630"),(50,L.toDate "20231231")]
  in 
    testGroup "Calc WAL"
    [ 
      testCase "WAL by Month" $ 
        assertEqual ""
          9.06
          (calcWAL ByMonth 100 (L.toDate "20230101") _ps )
      ,testCase "WAL by Year" $ 
        assertEqual ""
          0.74
          (calcWAL ByYear 100 (L.toDate "20230101") _ps )
    ]

durationTest = 
  testGroup "Duration Test" 
  [
    testCase "Duration 1" $ 
      assertEqual "10 Months bullet"
      (273 % 365)
      (calcDuration 
        DC_ACT_365F
        (L.toDate "20230101")
        [(L.toDate "20231001",100)]
        (L.mkRateTs [(L.toDate "20230101",0.01)]))
  , testCase "Duration 2" $ 
      assertEqual "Multiple cf"
      (252921 % 289445)
      (calcDuration 
        DC_ACT_365F
        (L.toDate "20230101")
        [(L.toDate "20231001",100),(L.toDate "20240101",100)]
        (L.mkRateTs [(L.toDate "20230101",0.01)]))
  , testCase "Duration 3" $
      assertEqual "12 Months bullet"
      (364 % 365)
      (calcDuration
        DC_ACT_365F
        (L.toDate "20230101")
        [(L.toDate "20231231",104)]
        (L.mkRateTs [(L.toDate "20230101",0.05)]))
  , testCase "Convexity 1" $
      assertEqual "10 Months bullet"
      (4068161010949933 % 2251799813685248)
      (calcConvexity
        DC_ACT_365F
        (L.toDate "20230101")
        [(L.toDate "20231231",104)]
        (L.mkRateTs [(L.toDate "20230101",0.05)]))
  ]

fvTest = 
  testGroup "FV Test" [
    testCase "FV2 test" $ 
        assertEqual "1-year"
            108
            (fv2 0.08 (L.toDate "20230101") (L.toDate "20240101") 100) 
    ,testCase "FV2 test" $ 
        assertEqual "0.5-year"
            103.89
            (fv2 0.08 (L.toDate "20230101") (L.toDate "20230701") 100) 
  ]

assetPricingTest = 
  testGroup "Pricing on Asset" [
    testCase "Loan Pricing(Inc Int)" $
      assertEqual "Loan Pricing"
        (Right (AssetPrice 1037.38 0.76 0.726208 0.0005369 0.21))
        (priceAsset (PersonalLoan (LoanOriginalInfo 1200 (Fix DC_30_360_US 0.08) 12 Monthly (L.toDate "20240701") I_P Nothing) 1000 0.08 10 Current)
                    (L.toDate "20241002") 
                    (PvRate 0.03) 
                    (LoanAssump Nothing Nothing Nothing Nothing,DummyDelinqAssump,DummyDefaultAssump)
                    Nothing 
                    Inc)
    ,testCase "Loan Pricing(Exc Int)" $
      assertEqual "Loan Pricing"
        (Right (AssetPrice 1037.17 0.76  0.72633840 0.00052012  0.21))
        (priceAsset (PersonalLoan (LoanOriginalInfo 1200 (Fix DC_30_360_US 0.08) 12 Monthly (L.toDate "20240701") I_P Nothing) 1000 0.08 10 Current)
                    (L.toDate "20241002") 
                    (PvRate 0.03) 
                    (LoanAssump Nothing Nothing Nothing Nothing,DummyDelinqAssump,DummyDefaultAssump)
                    Nothing 
                    Exc)
  ]

irrTest = 
  testGroup "Irr Test" [
    testCase "required Amount with 8%" $ 
        assertEqual "12 months"
            (Just 108.0)
            (calcRequiredAmtForIrrAtDate 0.08 (L.toDates ["20230101"])
                                                [-100] 
                                                (L.toDate "20240101"))
    ,testCase "IRR with 8%" $ 
        assertEqual "12 months"
            (Right (360287970912109 % 4503599627370496))
            (calcIRR (L.toDates ["20230101","20240101"]) [-100,108])
    ,testCase "IRR with custom" $ 
        assertEqual "3 months"
            (Right (7681459818792919 % 18014398509481984))
            (calcIRR (L.toDates ["20250101","20250301","20251018"]) [-100,50,70])
  ]
    -- ,testCase "FV2 test" $ 
    --     assertEqual "0.5-year"
    --         103.89
    --         (fv2 0.08 (L.toDate "20230101") (L.toDate "20230701") 100) 
