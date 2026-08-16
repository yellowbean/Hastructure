module UT.PoolTest (poolTest)
where

import Test.Tasty
import Test.Tasty.HUnit

import qualified AssetClass.AssetBase as AB
import qualified Assumptions as A
import qualified Cashflow as CF
import qualified Lib as L
import qualified Pool as P

import InterestRate (RateType (Fix))
import Types (DayCount (DC_ACT_365F))

poolTest :: TestTree
poolTest =
  testGroup
    "Pool test"
    [ testCase "pool DefaultByAmt is allocated proportional to current balance" $
        case P.runPool pool (Just defaultAss) Nothing of
          Left err -> assertFailure err
          Right proj ->
            assertEqual
              "a total default of 40 (25% and 75%) across the asets"
              [10, 30]
              (totalDefaults <$> proj)
    ]
  where
    pool =
      P.Pool
        { P.assets = [mortgage 100, mortgage 300]
        , P.futureCf = Nothing
        , P.futureScheduleCf = Nothing
        , P.asOfDate = L.toDate "20240101"
        , P.issuanceStat = Nothing
        , P.extendPeriods = Nothing
        }

    defaultAss =
      A.PoolLevel
        ( A.MortgageAssump
            (Just (A.DefaultByAmt (40, [1])))
            Nothing
            Nothing
            Nothing
        , A.DummyDelinqAssump
        , A.DummyDefaultAssump
        )

    mortgage balance =
      AB.Mortgage
        ( AB.MortgageOriginalInfo
            balance
            (Fix DC_ACT_365F 0.08)
            12
            L.Monthly
            (L.toDate "20240101")
            AB.Level
            Nothing
            Nothing
        )
        balance
        0.08
        12
        Nothing
        AB.Current

    totalDefaults (CF.CashFlowFrame _ txns, _) =
      sum (CF.mflowDefault <$> txns)
