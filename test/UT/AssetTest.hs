module UT.AssetTest(mortgageTests,mortgageCalcTests,loanTests,leaseTests,leaseFunTests,installmentTest)
where

import Test.Tasty
import Test.Tasty.HUnit

import Types

import qualified Data.Time as T
import qualified Lib as L
import qualified Asset as P
import qualified AssetClass.Mortgage as ACM
import qualified AssetClass.Loan as ACL
import qualified AssetClass.Lease as ACR
import qualified AssetClass.Installment as ACI
import qualified Assumptions as A
import qualified Cashflow as CF

import InterestRate

import Debug.Trace
debug = flip trace

tm = ACM.Mortgage
     (P.MortgageOriginalInfo 10000 (Fix 0.08) 24 L.Monthly (L.toDate "20210101") P.Level)
     8000 0.08 19 
     Nothing
     P.Current

tm1 = ACM.Mortgage
     (P.MortgageOriginalInfo 240 (Fix 0.08) 24 L.Monthly (L.toDate "20210101") P.Even)
     240 0.08 19 
     Nothing
     P.Current

tm2 = ACM.Mortgage
     (P.MortgageOriginalInfo 240 (Fix 0.08) 24 L.Monthly (L.toDate "20210101") P.Even)
     240 0.08 19 
     Nothing 
     (P.Defaulted Nothing)

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
                 (P.LoanOriginalInfo 180 (Fix 0.08) 36 L.Monthly (L.toDate "20200101") P.I_P) 
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

leaseFunTests = 
    let 
      a = 0 
      rentals = ACR.accrueRentals 
                    (LeftBalanceCurve
                      [TsPoint  (L.toDate "20230201") 0.05
                       ,TsPoint (L.toDate "20230215") 0.06
                       ,TsPoint (L.toDate "20230301") 0.07])
                    [(L.toDate "20230301")]
                    (L.toDate "20230201")
                    []
    in 
      testGroup "Lease Function Test" [
        testCase "Rental Accural Function" $
          assertEqual "A"
              [1.54] -- 14 days of 0.06, 14 days of 0.07
              rentals
      ]


leaseTests = 
    let 
      lease1 = ACR.RegularLease
                (ACR.LeaseInfo (L.toDate "20230101") 12 MonthEnd 1)
                100
                12
                P.Current
      asofDate = (L.toDate "20230615")
      cf1 = P.calcCashflow lease1 asofDate 

      lease2 = ACR.StepUpLease
                (ACR.LeaseInfo (L.toDate "20230601") 12 MonthEnd 1)
                (ACR.FlatRate MonthEnd 0.02)
                100
                12
                P.Current
      cf2 = P.calcCashflow lease2 asofDate 
      
      lease3 = ACR.StepUpLease
                (ACR.LeaseInfo (L.toDate "20230401") 4 MonthEnd 1)
                (ACR.ByRateCurve MonthEnd [0.02,0.04,0.05,0.06])
                100
                4
                P.Current
      cf3_0 = P.calcCashflow lease3 (L.toDate "20230415")
      cf3 = P.calcCashflow lease3 asofDate

      cf4 = P.projCashflow 
                lease1 
                asofDate 
                [A.LeaseGapDays 45, A.LeaseProjectionEnd (L.toDate "20260601")]
      cf5 =  P.projCashflow lease1 asofDate 
            [A.LeaseGapDays 5
            ,A.LeaseGapDaysByAmount [(0.5,12),(1,22),(2,62),(3,82)] 92
            ,A.LeaseProjectionEnd (L.toDate "20240601")]
    in 
      testGroup "Lease CF Test" [
        testCase "1 year Regular Lease sum of rentals" $
            assertEqual "total rental"
                214
                (sum $ map CF.tsTotalCash (CF.getTsCashFlowFrame cf1)) -- `debug` ("regular test"++show cf1)
        ,testCase "1 year Regular Lease first pay date" $
            assertEqual "first date of regular lease"
                (L.toDate "20230630")
                (head (CF.getDatesCashFlowFrame cf1))
        ,testCase "1 year Stepup lease first pay" $
            assertEqual "first pay"
                (CF.LeaseFlow (L.toDate "20230630") 377.76 29)
                (head (CF.getTsCashFlowFrame cf2))
        ,testCase "1 year Stepup lease" $
            assertEqual "total rental"
                406.76
                (sum $ map CF.tsTotalCash (CF.getTsCashFlowFrame cf2))
        ,testCase "1 year Stepup lease" $
            assertEqual "first rental step up at Month 2"
                (CF.LeaseFlow (L.toDate "20230731") 346.14 31.62)
                ((CF.getTsCashFlowFrame cf2)!!1)

        ,testCase "1 year Stepup Curve lease" $
            assertEqual "first rental step up at Month 0"
                (CF.LeaseFlow (L.toDate "20230430") 97.83 29.0)
                (head (CF.getTsCashFlowFrame cf3_0)) 

        ,testCase "1 year Stepup Curve lease" $
            assertEqual "first rental step up at Month 1"
                (CF.LeaseFlow (L.toDate "20230630") 34.41 31.8)
                (head (CF.getTsCashFlowFrame cf3)) -- `debug` ("CF3->"++show cf3)
        ,testCase "1 year Stepup Curve lease" $
            assertEqual "first rental step up at Month 2"
                (CF.LeaseFlow (L.toDate "20230731") 0 34.41)
                ((CF.getTsCashFlowFrame cf3)!!1)

        ,testCase "Lease with Assumptions" $ 
            assertEqual "Month Gap=2"
            (CF.LeaseFlow (L.toDate "20270331") 0 31)
            (last (CF.getTsCashFlowFrame cf4) )
        ,testCase "Lease with Assumptions" $ 
            assertEqual "Month Gap by Table : New Lease at period 0"
            (CF.LeaseFlow (L.toDate "20240131") 335 8)
            ((CF.getTsCashFlowFrame cf5)!!7)
        ,testCase "Lease with Assumptions" $ 
            assertEqual "Month Gap by Table : New Lease at period 1"
            (CF.LeaseFlow (L.toDate "20240229") 306 29)
            ((CF.getTsCashFlowFrame cf5)!!8)
      ]

installmentTest = 
    let 
      loan1 =  ACI.Installment
                 (P.LoanOriginalInfo 1000 (Fix 0.01) 12 L.Monthly (L.toDate "20220101") P.F_P) 
                 1000 
                 12 
                 P.Current
      asofDate1 = (L.toDate "20220115")
      loan1Cf = P.calcCashflow loan1 asofDate1

      loan2 =  ACI.Installment
                 (P.LoanOriginalInfo 1000 (Fix 0.01) 12 L.Monthly (L.toDate "20220101") P.F_P) 
                 500 
                 12
                 P.Current
      loan2Cf = P.calcCashflow loan2 asofDate1

      asofDate2 = (L.toDate "20220815")
      loan3 =  ACI.Installment
                 (P.LoanOriginalInfo 1000 (Fix 0.01) 12 L.Monthly (L.toDate "20220101") P.F_P) 
                 416.69 
                 5
                 P.Current
      loan3Cf = P.calcCashflow loan3 asofDate2

      loan4 =  ACI.Installment
                 (P.LoanOriginalInfo 1000 (Fix 0.01) 12 L.Monthly (L.toDate "20220101") P.F_P) 
                 208.35 
                 5
                 P.Current
      loan4Cf = P.calcCashflow loan4 asofDate2
    in 
      testGroup "Installment cashflow Tests" [ 
       testCase "Loan 1" $
           assertEqual "project period size"
             12 
             (CF.sizeCashFlowFrame loan1Cf)
      ,testCase "Loan 1 (on schedule)" $
           assertEqual "Balance/Principal/Int at period 1"
             (Just (CF.LoanFlow (L.toDate "20220201") 916.67 83.33 10 0 0 0 0 0.01))
             (CF.cfAt loan1Cf 0)
      ,testCase "Stressed Loan 1" $
           assertEqual "Balance/Principal/Int at period 1"
             (Just (CF.LoanFlow (L.toDate "20220201") 458.33 41.66 5 0 0 0 0 0.01))
             (CF.cfAt loan2Cf 0)
      ,testCase "Loan 2 with aging(on schedule)" $
           assertEqual "Balance/Principal/Int at period 1"
             (Just (CF.LoanFlow (L.toDate "20220901") 333.36 83.33 10 0 0 0 0 0.01))
             (CF.cfAt loan3Cf 0)
      ,testCase "Stress Loan 2 with aging" $
           assertEqual "Balance/Principal/Int at period 1"
             (Just (CF.LoanFlow (L.toDate "20220901") 166.68 41.66 5 0 0 0 0 0.01))
             (CF.cfAt loan4Cf 0)
      ]
