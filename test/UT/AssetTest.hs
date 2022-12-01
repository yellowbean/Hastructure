module UT.AssetTest(mortgageTests,mortgageCalcTests,loanTests)
where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Time as T
import qualified Lib as L
import qualified Asset as P
import qualified AssetClass.Mortgage as ACM
import qualified AssetClass.Loan as ACL
import qualified Assumptions as A
import qualified Cashflow as CF

import Debug.Trace
debug = flip trace

tm = ACM.Mortgage
     (P.MortgageOriginalInfo 10000 (P.Fix 0.08) 24 L.Monthly (L.toDate "20210101") P.Level)
     8000 0.08 19 P.Current

tm1 = ACM.Mortgage
     (P.MortgageOriginalInfo 240 (P.Fix 0.08) 24 L.Monthly (L.toDate "20210101") P.Even)
     240 0.08 19 P.Current

tm2 = ACM.Mortgage
     (P.MortgageOriginalInfo 240 (P.Fix 0.08) 24 L.Monthly (L.toDate "20210101") P.Even)
     240 0.08 19 (P.Defaulted Nothing)

asOfDate = L.toDate "20210605"
tmcf_00 = P.projCashflow tm asOfDate []
trs = CF.getTsCashFlowFrame tmcf_00
tmcf_default = P.projCashflow tm asOfDate [A.DefaultConstant 0.015]


mortgageCalcTests = testGroup "Mortgage Calc Test" 
  [
    testCase "Calc Pmt" $
        assertEqual "PMT 01"
           154.15
           (P.calcPmt 1200 0.12 24)
  ]


mortgageTests = testGroup "Mortgage cashflow Tests"
  [
    testCase "Fix rate mortgage" $
     --  19 @=? (CF.sizeCashFlowFrame tmcf_00)
     assertEqual "total size of cf" 19 (CF.sizeCashFlowFrame tmcf_00) -- `debug` ("result"++show(tmcf_00))
     ,
     testCase "first Date" $
     assertEqual "first date" (L.toDate "20210701")  (CF.getDate (head trs)) -- `debug` ("result"++show(tmcf_00))
     --assertEqual "total size of cf" 19 19
     ,
     testCase "Even Principal Type of Mortgage" $
     let
        tm1cf_00 = P.calcCashflow tm1 asOfDate
        trs = CF.getTsCashFlowFrame tm1cf_00
     in
        assertEqual "first row" 12.63  (CF.mflowPrincipal (head trs)) -- `debug` ("result"++show(tmcf_00))
     ,
     testCase "Default asset won't have cashflow if no assumption" $
     let
        asDay = (L.toDate "20220101")
        tm2cf_00 = P.projCashflow tm2 asDay  []
        trs = CF.getTsCashFlowFrame tm2cf_00
     in
        assertEqual "Empty for principal"
                    (0.0, asDay, 1)
                    ((CF.mflowPrincipal (head trs))
                    ,(CF.mflowDate (head trs))
                    ,(length trs))
  ]

loanTests = 
    let 
      loan1 =  ACL.PersonalLoan
                 (P.LoanOriginalInfo 180 (P.Fix 0.08) 36 L.Monthly (L.toDate "20200101") P.I_P) 
                 120
                 0.06
                 24
                 P.Current
      asofDate = (L.toDate "20200615")
      loan1Cf = P.calcCashflow loan1 asofDate
      loan2Cf = P.projCashflow loan1 asofDate []
    in 
      testGroup "Loan cashflow Tests" [ 
       testCase "Loan 1" $
           assertEqual "project period"
             24 
             (CF.sizeCashFlowFrame loan1Cf)
       ,testCase "Last Principal Amount" $
           assertEqual ""
            (Just (CF.LoanFlow (L.toDate "20220601") 0 120 0.61 0 0 0 0 0.06))
            (CF.cfAt loan1Cf 23)
       ,testCase "calcCashflow == projCashflow when assump = []" $
           assertEqual ""
           loan1Cf
           loan2Cf
     ]
