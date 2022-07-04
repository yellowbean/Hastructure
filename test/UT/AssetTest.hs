module UT.AssetTest(mortgageTests)
where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Time as T
import qualified Lib as L
import qualified Asset as P
import qualified Assumptions as A
import qualified Cashflow as CF

import Debug.Trace
debug = flip trace

tm = P.Mortgage
     (P.OriginalInfo 10000 (P.Fix 0.08) 24 L.Monthly (T.fromGregorian 2021 1 1))
     8000 0.08 19
asOfDate = (T.fromGregorian 2021 6 5)
-- tmcf = calcCashflow tm
tmcf_00 = P.projCashflow tm asOfDate []
tmcf_default = P.projCashflow tm asOfDate [A.DefaultConstant 0.015]

--tmF = Mortgage
--       (OriginalInfo 10000
--          (Floater L.LIBOR1M 0.02 0.075 L.Monthly Nothing)
--          5
--          L.Monthly (T.fromGregorian 2022 1 1))
--       10000 0.08 5
-- tmFf = P.projCashflow tm [A.InterestRateConstant,(A.LIBOR1M 0.07)]


mortgageTests = testGroup "Mortgage cashflow Tests"
  [
    testCase "Fix rate mortgage" $
      --assertEqual "first start date"
     --  19 @=? (CF.sizeCashFlowFrame tmcf_00)
     assertEqual "total size of cf" 19 (CF.sizeCashFlowFrame tmcf_00) -- `debug` ("result"++show(tmcf_00))
     --assertEqual "total size of cf" 19 19
  ]