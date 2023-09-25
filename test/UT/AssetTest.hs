module UT.AssetTest(mortgageTests,mortgageCalcTests,loanTests,leaseTests,leaseFunTests,installmentTest,armTest,ppyTest
                   ,delinqScheduleCFTest,delinqMortgageTest)
where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Time as T
import qualified Lib as L
import qualified Asset as P
import qualified AssetClass.AssetBase as AB
import qualified AssetClass.Mortgage as ACM
import qualified AssetClass.Loan as ACL
import qualified AssetClass.Lease as ACR
import qualified AssetClass.Installment as ACI
import qualified Assumptions as A
import qualified Cashflow as CF
import qualified Deal as D
import Types

import InterestRate

import Debug.Trace
debug = flip trace

tm = AB.Mortgage
     (AB.MortgageOriginalInfo 10000 (Fix DC_ACT_365F 0.08) 24 L.Monthly (L.toDate "20210101") AB.Level Nothing)
     8000 0.08 19 
     Nothing
     AB.Current

tm1 = AB.Mortgage
     (AB.MortgageOriginalInfo 240 (Fix DC_ACT_365F 0.08) 24 L.Monthly (L.toDate "20210101") AB.Even Nothing)
     240 0.08 19 
     Nothing
     AB.Current

tm2 = AB.Mortgage
     (AB.MortgageOriginalInfo 240 (Fix DC_ACT_365F 0.08) 24 L.Monthly (L.toDate "20210101") AB.Even Nothing)
     240 0.08 19 
     Nothing 
     (AB.Defaulted Nothing)

asOfDate = L.toDate "20210605"
(tmcf_00,_) = P.projCashflow tm asOfDate ((A.MortgageAssump Nothing Nothing Nothing Nothing),A.DummyDelinqAssump,A.DummyDefaultAssump) Nothing
trs = CF.getTsCashFlowFrame tmcf_00
(tmcf_default,_) = P.projCashflow 
                     tm 
                     asOfDate 
                     (A.MortgageAssump (Just (A.DefaultConstant 0.015)) Nothing Nothing Nothing ,A.DummyDelinqAssump,A.DummyDefaultAssump)
                     Nothing


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
        tm1cf_00 = P.calcCashflow tm1 asOfDate Nothing
        trs = CF.getTsCashFlowFrame tm1cf_00
     in
        assertEqual "first row" 12.63  (CF.mflowPrincipal (head trs)) -- `debug` ("result"++show(tmcf_00))
     ,
     testCase "Default asset won't have cashflow if no assumption" $
     let
        asDay = (L.toDate "20220101")
        (tm2cf_00,_) = P.projCashflow tm2 asDay (A.MortgageAssump Nothing Nothing Nothing Nothing ,A.DummyDelinqAssump,A.DummyDefaultAssump) Nothing
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
      loan1 =  AB.PersonalLoan
                 (AB.LoanOriginalInfo 180 (Fix DC_ACT_365F 0.08) 36 L.Monthly (L.toDate "20200101") AB.I_P) 
                 120
                 0.08
                 24
                 AB.Current
      asofDate = L.toDate "20200615"
      loan1Cf = P.calcCashflow loan1 asofDate Nothing
      (loan2Cf,_) = P.projCashflow loan1 asofDate (A.LoanAssump Nothing Nothing Nothing Nothing ,A.DummyDelinqAssump,A.DummyDefaultAssump) Nothing
    in 
      testGroup "Loan cashflow Tests" [ 
       testCase "Loan 1" $
           assertEqual "project period"
             24 
             (CF.sizeCashFlowFrame loan1Cf)
      -- ,testCase "First cashflow" $
      --     assertEqual ""
      --      (Just (CF.LoanFlow (L.toDate "20210201") 120 0 0.82 0 0 0 0 0.08))
      --      (CF.cfAt loan1Cf 0)
      -- ,testCase "Last Principal Amount" $
      --     assertEqual ""
      --      (Just (CF.LoanFlow (L.toDate "20230101") 0 120 0.82 0 0 0 0 0.08))
      --      (CF.cfAt loan1Cf 23)
      -- ,testCase "calcCashflow == projCashflow when assump = []" $
      --     assertEqual ""
      --     loan1Cf
      --     loan2Cf
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
      lease1 = AB.RegularLease
                (AB.LeaseInfo (L.toDate "20230101") 12 MonthEnd 1)
                100
                12
                AB.Current
      asofDate = (L.toDate "20230615")
      cf1 = P.calcCashflow lease1 asofDate Nothing

      lease2 = AB.StepUpLease
                (AB.LeaseInfo (L.toDate "20230601") 12 MonthEnd 1)
                (AB.FlatRate MonthEnd 0.02)
                100
                12
                AB.Current
      cf2 = P.calcCashflow lease2 asofDate Nothing
      
      lease3 = AB.StepUpLease
                (AB.LeaseInfo (L.toDate "20230401") 4 MonthEnd 1)
                (AB.ByRateCurve MonthEnd [0.02,0.04,0.05,0.06])
                100
                4
                AB.Current
      cf3_0 = P.calcCashflow lease3 (L.toDate "20230415") Nothing
      cf3 = P.calcCashflow lease3 asofDate Nothing

      (cf4,_) = P.projCashflow lease1 asofDate 
                  (A.LeaseAssump (A.GapDays 45)
                                 (A.BaseAnnualRate 0.0)
                                 (L.toDate "20240601")
                                 Nothing
                   ,A.DummyDelinqAssump,A.DummyDefaultAssump)
                   Nothing
      (cf5,_) =  P.projCashflow lease1 asofDate 
                   (A.LeaseAssump (A.GapDaysByAmount [(0.5,12),(1,22),(2,62),(3,82)] 92)
                                  (A.BaseAnnualRate 0.0)
                                  (L.toDate "20240601")
                                  Nothing
                   ,A.DummyDelinqAssump,A.DummyDefaultAssump)
                   Nothing
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
            assertEqual "Month Gap=45 days"
            (CF.LeaseFlow (L.toDate "20250131") 0 31)
            (last (CF.getTsCashFlowFrame cf4) ) -- `debug` ("CF4"++show cf4)
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
      loan1 =  AB.Installment
                 (AB.LoanOriginalInfo 1000 (Fix DC_ACT_365F 0.01) 12 L.Monthly (L.toDate "20220101") AB.F_P)
                 1000 
                 12 
                 AB.Current
      asofDate1 = L.toDate "20220115"
      loan1Cf = P.calcCashflow loan1 asofDate1 Nothing

      loan2 =  AB.Installment
                 (AB.LoanOriginalInfo 1000 (Fix DC_ACT_365F 0.01) 12 L.Monthly (L.toDate "20220101") AB.F_P) 
                 500 
                 12
                 AB.Current
      loan2Cf = P.calcCashflow loan2 asofDate1 Nothing

      asofDate2 = L.toDate "20220815"
      loan3 =  AB.Installment
                 (AB.LoanOriginalInfo 1000 (Fix DC_ACT_365F 0.01) 12 L.Monthly (L.toDate "20220101") AB.F_P) 
                 416.69 
                 5
                 AB.Current
      loan3Cf = P.calcCashflow loan3 asofDate2 Nothing

      loan4 =  AB.Installment
                 (AB.LoanOriginalInfo 1000 (Fix DC_ACT_365F 0.01) 12 L.Monthly (L.toDate "20220101") AB.F_P) 
                 208.35 
                 5
                 AB.Current
      loan4Cf = P.calcCashflow loan4 asofDate2 Nothing
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


armTest = 
  let 
    arm1 = AB.AdjustRateMortgage
            (AB.MortgageOriginalInfo 
              240 
              (Floater DC_ACT_365F SOFR3M 0.01 0.03 (EveryNMonth (L.toDate "20240801") 2) Nothing Nothing Nothing)
              30
              Monthly
              (L.toDate "20230501")
              AB.Level
              Nothing)
            (ARM 12 (Just 0.015) (Just 0.01) (Just 0.09) (Just 0.02) )  
            240 0.08 19 
            Nothing 
            AB.Current
    assump1 = RateCurve 
                SOFR3M
                (IRateCurve [TsPoint (L.toDate "20240501") 0.05 
                            ,TsPoint (L.toDate "20240901") 0.065
                            ,TsPoint (L.toDate "20241215") 0.07
                            ,TsPoint (L.toDate "20250315") 0.10
                            ,TsPoint (L.toDate "20251001") 0.12
                            ])
                
    (arm1_cf,_) = P.projCashflow arm1 (L.toDate "20230601") (A.MortgageAssump Nothing Nothing Nothing Nothing
                                                            ,A.DummyDelinqAssump,A.DummyDefaultAssump) 
                                                            (Just [assump1])
  in 
    testGroup "ARM cashflow tests" [
      testCase "ARM case 1/ cf length" $
        assertEqual "should be 19"
        19
        (CF.sizeCashFlowFrame arm1_cf)
      ,testCase "ARM case 1/ first cash" $
        assertEqual "first cash row"
        (Just (CF.MortgageFlow (L.toDate "20240501") 227.66 12.34 0.6 0 0 0 0 0 0.03 Nothing Nothing ))
        (CF.cfAt arm1_cf 0)
      ,testCase "ARM case 1/ frist reset" $
        assertEqual "first rate"
        (Just (CF.MortgageFlow (L.toDate "20240601") 215.41 12.25 0.85 0 0 0 0 0 0.045 Nothing Nothing ))
        (CF.cfAt arm1_cf 1)
      ,testCase "ARM case 1/periodic reset " $
        assertEqual "first rate"
        (Just (CF.MortgageFlow (L.toDate "20240801") 190.85 12.26 0.93 0 0 0 0 0 0.055 Nothing Nothing ))
        (CF.cfAt arm1_cf 3)
      ,testCase "ARM case 1/remains same before next reset" $
        assertEqual "period before first reset"
        (Just (CF.MortgageFlow (L.toDate "20240901") 178.53 12.32 0.87 0 0 0 0 0 0.055 Nothing Nothing ))
        (CF.cfAt arm1_cf 4)
      ,testCase "ARM case 1" $
        assertEqual "reset with periodic cap"
        (Just (CF.MortgageFlow (L.toDate "20241201") 141.47 12.38 0.96 0 0 0 0 0 0.075 Nothing Nothing))
        (CF.cfAt arm1_cf 7)
      ,testCase "ARM case 1" $
        assertEqual "Period 9"
        (Just (CF.MortgageFlow (L.toDate "20250101") 129.01 12.46 0.88 0 0 0 0 0 0.075 Nothing Nothing ))
        (CF.cfAt arm1_cf 8)
      ,testCase "ARM case 1" $
        assertEqual "Period 10"
        (Just (CF.MortgageFlow (L.toDate "20250201") 116.49 12.52 0.85 0 0 0 0 0 0.08 Nothing Nothing ))
        (CF.cfAt arm1_cf 9)
      ,testCase "ARM case 1" $
        assertEqual "life cap"
        (Just (CF.MortgageFlow (L.toDate "20250401") 91.24 12.65 0.77 0 0 0 0 0 0.09 Nothing Nothing ))
        (CF.cfAt arm1_cf 11)

    ]

---- prepayment penalty 

ppy_1 = Just $ AB.ByTerm 3 0.1 0.01
ppy_2 = Just $ AB.FixAmount 100 Nothing
ppy_2_1 = Just $ AB.FixAmount 100 (Just 2)
ppy_3 = Just $ AB.FixPct 0.01 Nothing
ppy_3_1 = Just $ AB.FixPct 0.01 (Just 2)
ppy_4 = Just $ AB.Sliding 0.1 0.01
ppy_5 = Just $ AB.StepDown [(2,0.5),(12,0.2)]

origin_info = AB.MortgageOriginalInfo 10000 (Fix DC_ACT_365F 0.08) 24 L.Monthly (L.toDate "20210101") AB.Level Nothing

tm_ppy_1 = AB.Mortgage (origin_info { AB.prepaymentPenalty = ppy_1}) 10000 0.08 24 Nothing AB.Current
tm_ppy_2 = AB.Mortgage (origin_info { AB.prepaymentPenalty = ppy_2}) 10000 0.08 24 Nothing AB.Current
tm_ppy_2_1 = AB.Mortgage (origin_info { AB.prepaymentPenalty = ppy_2_1}) 10000 0.08 24 Nothing AB.Current
tm_ppy_3 = AB.Mortgage (origin_info { AB.prepaymentPenalty = ppy_3}) 10000 0.08 24 Nothing AB.Current
tm_ppy_3_1 = AB.Mortgage (origin_info { AB.prepaymentPenalty = ppy_3_1}) 10000 0.08 24 Nothing AB.Current
tm_ppy_4 = AB.Mortgage (origin_info { AB.prepaymentPenalty = ppy_4}) 10000 0.08 24 Nothing AB.Current
tm_ppy_5 = AB.Mortgage (origin_info { AB.prepaymentPenalty = ppy_5}) 10000 0.08 24 Nothing AB.Current

ppyTest = 
  let 
    assump1 = (A.MortgageAssump Nothing (Just (A.PrepaymentCPR 0.03)) Nothing Nothing,A.DummyDelinqAssump,A.DummyDefaultAssump)
    (ppy_cf_1,_) = P.projCashflow tm_ppy_1 (L.toDate "20210101") assump1 Nothing
    (ppy_cf_2,_) = P.projCashflow tm_ppy_2 (L.toDate "20210101") assump1 Nothing
    (ppy_cf_2_1,_) = P.projCashflow tm_ppy_2_1 (L.toDate "20210101") assump1 Nothing
    (ppy_cf_3,_) = P.projCashflow tm_ppy_3 (L.toDate "20210101") assump1 Nothing
    (ppy_cf_3_1,_) = P.projCashflow tm_ppy_3_1 (L.toDate "20210101") assump1 Nothing
    (ppy_cf_4,_) = P.projCashflow tm_ppy_4 (L.toDate "20210101") assump1 Nothing
    (ppy_cf_5,_) = P.projCashflow tm_ppy_5 (L.toDate "20210101") assump1 Nothing
  in 
    testGroup "Prepay Penalty tests" [
      testCase "ppy case 1" $
        assertEqual " using rate0"
        (Just (CF.MortgageFlow (L.toDate "20210201") 9589.55 384.62 66.48 25.83 0 0 0 0 0.08 Nothing (Just 2.58)))
        (CF.cfAt ppy_cf_1 0)
      ,testCase "ppy case 1" $
        assertEqual " using rate1"
        (Just (CF.MortgageFlow (L.toDate "20210501") 8357.98 389.45 58.31 21.92 0 0 0 0 0.08 Nothing (Just 0.21 )))
        (CF.cfAt ppy_cf_1 3)
      ,testCase "ppy case 2" $
        assertEqual " using fix amount"
        (Just (CF.MortgageFlow (L.toDate "20210501") 8357.98 389.45 58.31 21.92 0 0 0 0 0.08 Nothing (Just 100 )))
        (CF.cfAt ppy_cf_2 3)
      ,testCase "ppy case 2 1_0" $
        assertEqual " using fix amount in period"
        (Just (CF.MortgageFlow (L.toDate "20210201")  9589.55 384.62 66.48 25.83 0 0 0 0 0.08 Nothing  (Just 100 )))
        (CF.cfAt ppy_cf_2_1 0)
      ,testCase "ppy case 2 1" $
        assertEqual " using fix amount out of period"
        (Just (CF.MortgageFlow (L.toDate "20210501") 8357.98 389.45 58.31 21.92 0 0 0 0 0.08 Nothing (Just 0 )))
        (CF.cfAt ppy_cf_2_1 3)
      ,testCase "ppy case 3" $
        assertEqual " using life time pct"
        (Just (CF.MortgageFlow (L.toDate "20210501") 8357.98 389.45 58.31 21.92 0 0 0 0 0.08 Nothing (Just 0.21 )))
        (CF.cfAt ppy_cf_3 3)
      ,testCase "ppy case 3 1_0" $
        assertEqual " using pct in period"
        (Just (CF.MortgageFlow (L.toDate "20210201")  9589.55 384.62 66.48 25.83 0 0 0 0 0.08 Nothing  (Just 0.25 )))
        (CF.cfAt ppy_cf_3_1 0)
      ,testCase "ppy case 3 1" $
        assertEqual " using pct out of period"
        (Just (CF.MortgageFlow (L.toDate "20210501") 8357.98 389.45 58.31 21.92 0 0 0 0 0.08 Nothing (Just 0 )))
        (CF.cfAt ppy_cf_3_1 3)
      ,testCase "ppy case 4" $
        assertEqual " using slide at period 0"
        (Just (CF.MortgageFlow (L.toDate "20210201")  9589.55 384.62 66.48 25.83 0 0 0 0 0.08 Nothing  (Just 2.58 )))
        (CF.cfAt ppy_cf_4 0)
      ,testCase "ppy case 4 1" $
        assertEqual " using slide at period 1"
        (Just (CF.MortgageFlow (L.toDate "20210501") 8357.98 389.45 58.31 21.92 0 0 0 0 0.08 Nothing (Just (0.07*21.92))))
        (CF.cfAt ppy_cf_4 3)     
      ,testCase "ppy case 5" $
        assertEqual " using rate 0 before 2 periods"
        (Just (CF.MortgageFlow (L.toDate "20210201")  9589.55 384.62 66.48 25.83 0 0 0 0 0.08 Nothing  (Just (25.83*0.5) )))
        (CF.cfAt ppy_cf_5 0)
      ,testCase "ppy case 5 1" $
        assertEqual " using rate 1 after 2 periods"
        (Just (CF.MortgageFlow (L.toDate "20210501") 8357.98 389.45 58.31 21.92 0 0 0 0 0.08 Nothing (Just (0.2*21.92))))
        (CF.cfAt ppy_cf_5 3)   
    ]

delinqScheduleCFTest = 
  let 
    cfs = [CF.MortgageFlow (L.toDate "20230901") 1000  0 0 0 0 0 0 0 0.08 Nothing Nothing
          ,CF.MortgageFlow (L.toDate "20231001") 500 500 0 0 0 0 0 0 0.08 Nothing Nothing
          ]
    pool = P.Pool ([]::[AB.Mortgage])
                  (Just (CF.CashFlowFrame cfs))
                  (L.toDate "20230801")
                  Nothing
                  (Just MonthEnd)
    assump1 = Just (A.PoolLevel 
                      (A.MortgageDeqAssump (Just (A.DelinqCDR 0.05 (5,0.3))) Nothing Nothing Nothing
                      ,A.DummyDelinqAssump
                      ,A.DummyDefaultAssump))
    assump2 = Just (A.PoolLevel 
                      (A.MortgageDeqAssump (Just (A.DelinqCDR 0.05 (5,0.3))) (Just (A.PrepaymentCPR 0.08)) Nothing Nothing
                      ,A.DummyDelinqAssump
                      ,A.DummyDefaultAssump))

    poolCf = fst . head $ D.runPool pool assump1 Nothing
    poolCf2 = fst . head $ D.runPool pool assump2 Nothing
  in 
    testGroup "delinq run on schedule flow" [
      testCase "case 01" $
        assertEqual "size of cashflow" 
        7
        (CF.sizeCashFlowFrame poolCf) 
      ,testCase "case 02" $
        assertEqual "first row of cf"
        (Just (CF.MortgageFlow (L.toDate "20230901") 995.66  0 0 0 4.34 0 0 0 0.08 Nothing Nothing))
        (CF.cfAt poolCf 0)
      ,testCase "case 03" $
        assertEqual "second row of cf"
        (Just (CF.MortgageFlow (L.toDate "20231001") 493.66  497.82 0 0 4.18 0 0 0 0.08 Nothing Nothing))
        (CF.cfAt poolCf 1)
      ,testCase "case 04" $
        assertEqual "first extended cf, nothing"
        (Just (CF.MortgageFlow (L.toDate "20231031") 493.66  0.0 0 0 0 0 0 0 0.00 Nothing Nothing))
        (CF.cfAt poolCf 2)
      ,testCase "case 05" $
        assertEqual "first default from delinq"
        (Just (CF.MortgageFlow (L.toDate "20240131") 496.69  0.0 0 0 0 1.3 0 1.3 0.000488 Nothing Nothing))
        (CF.cfAt poolCf 5)
      ,testCase "case 06" $
        assertEqual "first loss/recovery from default & first back to perf"
        (Just (CF.MortgageFlow (L.toDate "20240229") 499.66  2.97 0 0 0 1.25 0 1.25 0.000475 Nothing Nothing))
        (CF.cfAt poolCf 6)
      -- ,testCase "case 07" $
      --   assertEqual "first loss/recovery from default & first back to perf"
      --   (Just (CF.MortgageFlow (L.toDate "20240229") 492.36  0.0 0 0 0 1.25 0 1.25 0.0 Nothing Nothing))
      --   (CF.cfAt poolCf 7)
      ,testCase "case with prepay assump" $
        assertEqual "01"
        (Just (CF.MortgageFlow (L.toDate "20230901") 988.64 0 0 7.02 4.34  0.0 0.0 0.0 0.08 Nothing Nothing))
        (CF.cfAt poolCf2 0)
    ]

delinqMortgageTest = 
  let 
    tm1 = AB.Mortgage
           (AB.MortgageOriginalInfo 12 (Fix DC_ACT_365F 0.08) 12 L.Monthly (L.toDate "20210101") AB.Level Nothing)
           240 0.08 3
           Nothing
           AB.Current
    assump1 = (A.MortgageDeqAssump   
                        (Just (A.DelinqCDR 0.05 (2,0.3)))
                        -- (Just (A.PrepaymentCPR 0.08))
                        Nothing
                        Nothing 
                        Nothing
              ,A.DummyDelinqAssump,A.DummyDefaultAssump)
    (CF.CashFlowFrame txns,m) = P.projCashflow tm1 (L.toDate "20200101") assump1 Nothing
  in 
    testGroup "Mortgage Delinq Projection" [
      testCase "" $
        assertEqual "Length of cf"
        5
        (length txns)
      ,testCase "first row" $
        assertEqual "delinq = 1"
        (CF.MortgageFlow (L.toDate "20211101") 159.84 79.12 1.59 0 1.04 0.0 0.0 0.0 0.08 Nothing Nothing)
        (txns!!0)
      ,testCase "last row" $
        assertEqual "with first default/loss/recovery"
        (CF.MortgageFlow (L.toDate "20220101") 0.0 79.51 0.53  0 0.34 0.31 0.0 0.31 0.08 Nothing Nothing)
        (txns!!2)
      ,testCase "extend 1st flow" $
        assertEqual "check default"
        (CF.MortgageFlow (L.toDate "20220201") 0.36 0.36 0.0  0.0 0.0 0.20 0.0 0.2 0.08 Nothing Nothing)
        (txns!!3)
    ]
