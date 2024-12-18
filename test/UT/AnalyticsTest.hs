module UT.AnalyticsTest(walTest,durationTest,fvTest,assetPricingTest)
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
      0.74
      (calcDuration 
        (L.toDate "20230101")
        [(L.toDate "20231001",100)]
        (L.mkRateTs [(L.toDate "20230101",0.01)]))
  , testCase "Duration 2" $ 
      assertEqual "Multiple cf"
      0.86
      (calcDuration 
        (L.toDate "20230101")
        [(L.toDate "20231001",100),(L.toDate "20240101",100)]
        (L.mkRateTs [(L.toDate "20230101",0.01)]))
  ]

fvTest = testGroup "FV Test" [
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
        (Right (AssetPrice 1037.38 0.76 0.7 (-1.0) 0.21))
        (priceAsset (PersonalLoan (LoanOriginalInfo 1200 (Fix DC_30_360_US 0.08) 12 Monthly (L.toDate "20240701") I_P Nothing) 1000 0.08 10 Current)
                    (L.toDate "20241002") 
                    (PvRate 0.03) 
                    (LoanAssump Nothing Nothing Nothing Nothing,DummyDelinqAssump,DummyDefaultAssump)
                    Nothing 
                    Inc)
    ,testCase "Loan Pricing(Exc Int)" $
      assertEqual "Loan Pricing"
        (Right (AssetPrice 1037.17 0.76 0.7 (-1.0) 0.21))
        (priceAsset (PersonalLoan (LoanOriginalInfo 1200 (Fix DC_30_360_US 0.08) 12 Monthly (L.toDate "20240701") I_P Nothing) 1000 0.08 10 Current)
                    (L.toDate "20241002") 
                    (PvRate 0.03) 
                    (LoanAssump Nothing Nothing Nothing Nothing,DummyDelinqAssump,DummyDefaultAssump)
                    Nothing 
                    Exc)
  ]
