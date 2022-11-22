module UT.DealTest(td2,waterfallTests,queryTests,triggerTests)

where

import Test.Tasty
import Test.Tasty.HUnit
import Deal

import qualified Accounts as A
import qualified Asset as P
import qualified Expense as F
import qualified Deal as D
import qualified Liability as L
import qualified Waterfall as W
import qualified Cashflow as CF
import qualified Assumptions as AP
import qualified Call as C
import Lib
import Types

import qualified Data.Map as Map
import qualified Data.Time as T
import qualified Data.Set as S

td2 = TestDeal {
  D.name = "test deal1"
  ,D.status = Amortizing
  ,D.dates = PatternInterval $ 
               (Map.fromList [
                (ClosingDate,((T.fromGregorian 2022 1 1),MonthFirst,(toDate "20300101")))
                ,(CutoffDate,((T.fromGregorian 2022 1 1),MonthFirst,(toDate "20300101")))
                ,(FirstPayDate,((T.fromGregorian 2022 2 25),DayOfMonth 25,(toDate "20300101")))
               ])
  ,D.accounts = (Map.fromList
  [("General", (A.Account { A.accName="General" ,A.accBalance=1000.0 ,A.accType=Nothing, A.accInterest=Nothing ,A.accStmt=Nothing
  })),
   ("Reserve", (A.Account { A.accName="Reserve" ,A.accBalance=0.0 ,A.accType=Just (A.FixReserve 500), A.accInterest=Nothing ,A.accStmt=Nothing
  }))
  ])
  ,D.fees = (Map.fromList [("Service-Fee"
                         ,F.Fee{F.feeName="service-fee"
                                ,F.feeType = F.FixFee 10
                                ,F.feeStart = (T.fromGregorian 2022 1 1)
                                ,F.feeDue = 100
                                ,F.feeDueDate = Nothing
                                ,F.feeArrears = 0
                                ,F.feeLastPaidDay = Nothing
                                ,F.feeStmt = Nothing})])
  ,D.bonds = (Map.fromList [("A"
                             ,L.Bond{
                              L.bndName="A"
                             ,L.bndType=L.Sequential
                             ,L.bndOriginInfo= L.OriginalInfo{
                                                L.originBalance=3000
                                                ,L.originDate= (T.fromGregorian 2022 1 1)
                                                ,L.originRate= 0.08}
                             ,L.bndInterestInfo= L.Fix 0.08 DC_ACT_365F
                             ,L.bndBalance=3000
                             ,L.bndRate=0.08
                             ,L.bndDuePrin=0.0
                             ,L.bndDueInt=0.0
                             ,L.bndDueIntDate=Nothing
                             ,L.bndLastIntPay = Just (T.fromGregorian 2022 1 1)
                             ,L.bndLastPrinPay = Just (T.fromGregorian 2022 1 1)
                             ,L.bndStmt=Nothing})
                             ,("B"
                               ,L.Bond{
                                L.bndName="B"
                               ,L.bndType=L.Equity
                               ,L.bndOriginInfo= L.OriginalInfo{
                                                  L.originBalance=3000
                                                  ,L.originDate= (T.fromGregorian 2022 1 1)
                                                  ,L.originRate= 0.08}
                               ,L.bndInterestInfo= L.Fix 0.08 DC_ACT_365F
                               ,L.bndBalance=500
                               ,L.bndRate=0.08
                               ,L.bndDuePrin=0.0
                               ,L.bndDueInt=0.0
                               ,L.bndDueIntDate=Nothing
                               ,L.bndLastIntPay = Just (T.fromGregorian 2022 1 1)
                               ,L.bndLastPrinPay = Just (T.fromGregorian 2022 1 1)
                               ,L.bndStmt=Nothing})
                         ]
           )
  ,D.pool = P.Pool {P.assets=[P.Mortgage
                                         P.MortgageOriginalInfo{
                                           P.originBalance=4000
                                           ,P.originRate=P.Fix 0.085
                                           ,P.originTerm=60
                                           ,P.period=Monthly
                                           ,P.startDate=(T.fromGregorian 2022 1 1)
                                           ,P.prinType= P.Level}
                                         4000
                                         0.085
                                         60
                                         P.Current
                                ,P.Mortgage
                                   P.MortgageOriginalInfo{
                                     P.originBalance=4000
                                     ,P.originRate=P.Fix 0.085
                                     ,P.originTerm=60
                                     ,P.period=Monthly
                                     ,P.startDate=(T.fromGregorian 2022 1 1)
                                     ,P.prinType= P.Level}
                                   200
                                   0.085
                                   60
                                   (P.Defaulted Nothing)
                                 ]
                 ,P.futureCf=Nothing
                 ,P.asOfDate = T.fromGregorian 2022 1 1}
   ,D.waterfall = Map.fromList [(W.DistributionDay Amortizing, [
                                 (Nothing, W.PayFee ["General"] ["Service-Fee"])
                                 ,(Nothing, W.PayFeeBy (W.DuePct 0.5) ["General"] ["Service-Fee"])
                                 ,(Nothing, W.TransferReserve W.Source  "Reserve" "General")
                                 ,(Nothing, W.TransferReserve W.Target  "General" "Reserve")
                                 ,(Nothing, W.PayInt "General" ["A"])
                                 ,(Nothing, W.PayPrin "General" ["A"])
   ])]
 ,D.collects = [W.Collect W.CollectedInterest "General"
             ,W.Collect W.CollectedPrincipal "General"]
 ,D.call = Nothing
 ,D.triggers = Just $
                Map.fromList $
                  [(BeginDistributionWF,[(AfterDate (toDate "20220501"),DealStatusTo Revolving)])]
 ,D.overrides = Nothing
}

waterfallTests =  testGroup "Waterfall Tests"
  [
    let
     afterAction = D.performAction (toDate "20220301") td2 $ (Nothing, W.PayPrinBy (W.RemainBalPct 0.05) "General" "B")
     afterBnd = (D.bonds afterAction) Map.! "B"
     afterBndA = (D.bonds afterAction) Map.! "A"
     afterAcc = (D.accounts afterAction) Map.! "General"
    in
      testCase "after pay till pct of deal bond balance" $
      assertEqual "junior bond balance " [158.75,3000] [(L.bndBalance afterBnd),(L.bndBalance afterBndA)]
  ]

queryTests =  testGroup "deal stat query Tests"
  [
    let
     currentDefBal = D.queryDeal td2 CurrentPoolDefaultedBalance
    in
     testCase "query current assets in defaulted status" $
     assertEqual "should be 200" 200 currentDefBal
  ]

triggerTests = testGroup "Trigger Tests"
  [ let 
      setup = 0 
      poolflows = CF.CashFlowFrame $
                     [CF.MortgageFlow (toDate "20220201") 800 100 20 0 0 0 0 0.08
                     ,CF.MortgageFlow (toDate "20220301") 700 100 20 0 0 0 0 0.08
                     ,CF.MortgageFlow (toDate "20220401") 600 100 20 0 0 0 0 0.08
                     ,CF.MortgageFlow (toDate "20220501") 500 100 20 0 0 0 0 0.08
                     ,CF.MortgageFlow (toDate "20220601") 400 100 20 0 0 0 0 0.08
                     ,CF.MortgageFlow (toDate "20220701") 300 100 20 0 0 0 0 0.08
                     ]
      ads = [PoolCollection (toDate "20220201") "" 
             ,RunWaterfall  (toDate "20220225") ""
             ,PoolCollection (toDate "20220301")""
             ,RunWaterfall  (toDate "20220325") ""
             ,PoolCollection (toDate "20220401")""
             ,RunWaterfall  (toDate "20220425") ""
             ,PoolCollection (toDate "20220501")""
             ,RunWaterfall  (toDate "20220525") ""
             ,PoolCollection (toDate "20220601")""
             ,RunWaterfall  (toDate "20220625") ""
             ,PoolCollection (toDate "20220701")""
             ,RunWaterfall  (toDate "20220725") ""  ]
      fdeal = run2 td2 (Just poolflows) (Just ads) Nothing Nothing 
    in 
      testCase "deal becomes revolving" $
      assertEqual "revoving" 
        Revolving 
        (status fdeal)
  ]

