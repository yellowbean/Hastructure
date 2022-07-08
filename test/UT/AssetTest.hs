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
     (P.OriginalInfo 10000 (P.Fix 0.08) 24 L.Monthly (L.toDate "20210101"))
     8000 0.08 19
asOfDate = L.toDate "20210605"
tmcf_00 = P.projCashflow tm asOfDate []
trs = CF.getTsCashFlowFrame tmcf_00
tmcf_default = P.projCashflow tm asOfDate [A.DefaultConstant 0.015]


mortgageTests = testGroup "Mortgage cashflow Tests"
  [
    testCase "Fix rate mortgage" $
     --  19 @=? (CF.sizeCashFlowFrame tmcf_00)
     assertEqual "total size of cf" 19 (CF.sizeCashFlowFrame tmcf_00) -- `debug` ("result"++show(tmcf_00))
     ,
     testCase "first Date" $
     assertEqual "first date" (L.toDate "20210701")  (CF.tsDate (head trs)) -- `debug` ("result"++show(tmcf_00))
     --assertEqual "total size of cf" 19 19
  ]