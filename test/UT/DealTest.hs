module UT.DealTest(td2,waterfallTests,queryTests,triggerTests,dateTests,liqProviderTest)

where

import Test.Tasty
import Test.Tasty.HUnit
import Deal

import qualified Accounts as A
import qualified Stmt as S
import qualified Asset as P
import qualified AssetClass.Mortgage as ACM
import qualified AssetClass.AssetBase as AB
import qualified Expense as F
import qualified Deal.DealBase as D
import qualified Liability as L
import qualified Waterfall as W
import qualified Cashflow as CF
import qualified Assumptions as AP
import qualified Call as C
import InterestRate
import qualified CreditEnhancement as CE
import qualified Triggers as Trg
import Lib
import Types

import qualified Data.Map as Map
import qualified Data.Time as T
import qualified Data.Set as S

td2 = D.TestDeal {
  D.name = "test deal1"
  ,D.status = Amortizing
  ,D.rateSwap = Nothing
  ,D.currencySwap = Nothing
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
                                                ,L.originRate= 0.08
                                                ,L.maturityDate = Nothing}
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
                                                  ,L.originRate= 0.08
                                                  ,L.maturityDate = Nothing}
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
  ,D.pool = P.Pool {P.assets=[AB.Mortgage
                                         AB.MortgageOriginalInfo{
                                           AB.originBalance=4000
                                           ,AB.originRate=Fix 0.085
                                           ,AB.originTerm=60
                                           ,AB.period=Monthly
                                           ,AB.startDate=(T.fromGregorian 2022 1 1)
                                           ,AB.prinType= AB.Level}
                                         4000
                                         0.085
                                         60
                                         Nothing
                                         AB.Current
                                ,AB.Mortgage
                                   AB.MortgageOriginalInfo{
                                     AB.originBalance=4000
                                     ,AB.originRate=Fix 0.085
                                     ,AB.originTerm=60
                                     ,AB.period=Monthly
                                     ,AB.startDate=(T.fromGregorian 2022 1 1)
                                     ,AB.prinType= AB.Level}
                                   200
                                   0.085
                                   60
                                   Nothing
                                   (AB.Defaulted Nothing)
                                 ]
                 ,P.futureCf=Nothing
                 ,P.asOfDate = T.fromGregorian 2022 1 1
                 ,P.issuanceStat = Nothing}
   ,D.waterfall = Map.fromList [(W.DistributionDay Amortizing, [
                                  (W.PayFee ["General"] ["Service-Fee"])
                                 ,(W.PayFeeBy (DuePct 0.5) ["General"] ["Service-Fee"])
                                 ,(W.TransferReserve W.Source  "Reserve" "General")
                                 ,(W.TransferReserve W.Target  "General" "Reserve")
                                 ,(W.PayInt "General" ["A"])
                                 ,(W.PayPrin "General" ["A"])
   ])]
 ,D.collects = [W.Collect W.CollectedInterest "General"
             ,W.Collect W.CollectedPrincipal "General"]
 ,D.custom = Nothing
 ,D.call = Nothing
 ,D.liqProvider = Just $ Map.fromList $
                    [("Liq1",CE.LiqFacility 
                                "" 
                                CE.FixSupport
                                (Just 100)
                                50
                                (toDate "20220201")
                                Nothing
                                Nothing
                                (Just (CE.FixRate MonthEnd 0.05 (Just (toDate "20220201"))))
                                Nothing
                                (Just (S.Statement [S.SupportTxn (toDate "20220215") (Just 110) 10 40 (Just 0) (Just 0) S.Empty 
                                                    ,S.SupportTxn (toDate "20220315") (Just 100) 10 50 (Just 0) (Just 0) S.Empty])))]
 ,D.triggers = Just $
                Map.fromList $
                  [(BeginDistributionWF,[ Trg.Trigger{Trg.trgCondition = IfDate G (toDate "20220501")
                                                     ,Trg.trgEffects = Trg.DealStatusTo Revolving
                                                     ,Trg.trgStatus = False 
                                                     ,Trg.trgCurable = False }])]
 ,D.overrides = Nothing
 ,D.ledgers = Nothing
}

waterfallTests =  testGroup "Waterfall Tests"
  [
    let
     afterAction = performAction (toDate "20220301") td2 $ (W.PayPrinBy (RemainBalPct 0.05) "General" "B")
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
     currentDefBal = queryDeal td2 CurrentPoolDefaultedBalance
    in
     testCase "query current assets in defaulted status" $
     assertEqual "should be 200" 200 currentDefBal
  ]

triggerTests = testGroup "Trigger Tests"
  [ let 
      setup = 0 
      poolflows = CF.CashFlowFrame $
                     [CF.MortgageFlow (toDate "20220201") 800 100 20 0 0 0 0 0.08 Nothing
                     ,CF.MortgageFlow (toDate "20220301") 700 100 20 0 0 0 0 0.08 Nothing
                     ,CF.MortgageFlow (toDate "20220401") 600 100 20 0 0 0 0 0.08 Nothing 
                     ,CF.MortgageFlow (toDate "20220501") 500 100 20 0 0 0 0 0.08 Nothing
                     ,CF.MortgageFlow (toDate "20220601") 400 100 20 0 0 0 0 0.08 Nothing
                     ,CF.MortgageFlow (toDate "20220701") 300 100 20 0 0 0 0 0.08 Nothing
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
      (fdeal,_) = run2 td2 poolflows (Just ads) Nothing Nothing Nothing []
    in 
      testCase "deal becomes revolving" $
      assertEqual "revoving" 
        Revolving 
        (D.status fdeal)
  ]

dateTests = 
  let 
   a = PreClosingDates
        (toDate "20220601") 
        (toDate "20220610") 
        Nothing
        (toDate "20220901") 
        (toDate "20220630",MonthEnd)
        (toDate "20220715",DayOfMonth 10)
  in 
   testGroup "Deal Tests" 
   [ testCase "Dates pattern" $
     assertEqual  ""
    ((toDate "20220601"), (toDate "20220610"),(toDate "20220715")
     ,[PoolCollection (toDate "20220630") "",PoolCollection (toDate "20220731") "",PoolCollection (toDate "20220831") ""]
     ,[RunWaterfall (toDate "20220715") "",RunWaterfall (toDate "20220810") ""]
     ,(toDate "20220901") )
     (populateDealDates a)
   ]
  
liqProviderTest = 
  let 
    liq1 = CE.LiqFacility "" 
                       CE.FixSupport
                       (Just 100)
                       90
                       (toDate "20220201")
                       Nothing 
                       Nothing
                       (Just (CE.FixRate MonthEnd 0.05 (Just (toDate "20220201"))))
                       Nothing
                       (Just (S.Statement 
                               [S.SupportTxn (toDate "20220215") (Just 110) 40 40 (Just 0) (Just 0) S.Empty
                               ,S.SupportTxn (toDate "20220315") (Just 100) 50 90 (Just 0) (Just 0) S.Empty
                               ]))
  in 
    testGroup "Liq provider test" 
      [testCase "Liq Provider Int test" $
          assertEqual ""
           93
           (CE.liqCredit $ accrueLiqProvider td2 (toDate "20221101") liq1)
      ]
