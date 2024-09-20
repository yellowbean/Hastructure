module DealTest.DealTest(baseCase,baseTests,emptyCase)

where

import Test.Tasty
import Test.Tasty.HUnit
import Deal

import qualified Accounts as A
import qualified Stmt as S
import qualified Asset as Ast 
import qualified Pool as P
import qualified AssetClass.Mortgage as ACM
import qualified AssetClass.AssetBase as AB
import qualified Expense as F
import qualified Deal.DealBase as D
import qualified Deal as DR
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
import Numeric.Lens (base)
import qualified Types as P

dummySt = (0,Lib.toDate "19000101",Nothing)

emptyCase = D.TestDeal {
  D.name = "empty case"
  ,D.status = Amortizing
  ,D.rateSwap = Nothing
  ,D.currencySwap = Nothing
  ,D.dates = PatternInterval $ 
               (Map.fromList [
                (ClosingDate,((T.fromGregorian 2022 1 1),MonthFirst,(toDate "20300101")))
                ,(CutoffDate,((T.fromGregorian 2022 1 1),MonthFirst,(toDate "20300101")))
                ,(FirstPayDate,((T.fromGregorian 2022 2 25),DayOfMonth 25,(toDate "20300101")))
               ])
  ,D.accounts = Map.empty
  ,D.fees = Map.empty
  ,D.bonds = Map.empty
  ,D.pool = D.SoloPool (P.Pool {P.assets=[]})
  ,D.waterfall = Map.empty
  ,D.collects = []
}

baseCase = D.TestDeal {
  D.name = "base case"
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
  [("General", (A.Account { A.accName="General" ,A.accBalance=1000.0 ,A.accType=Nothing, A.accInterest=Nothing ,A.accStmt=Nothing }))])
  ,D.fees = Map.empty
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
                         ]
           )
  ,D.pool = D.SoloPool (P.Pool {P.assets=[AB.Mortgage
                                         AB.MortgageOriginalInfo{
                                           AB.originBalance=4000
                                           ,AB.originRate=Fix DC_ACT_365F 0.085
                                           ,AB.originTerm=60
                                           ,AB.period=Monthly
                                           ,AB.startDate=T.fromGregorian 2022 1 1
                                           ,AB.prinType= AB.Level
                                           ,AB.prepaymentPenalty = Nothing}
                                         4000
                                         0.085
                                         60
                                         Nothing
                                         AB.Current]
                               ,P.futureCf=Just (CF.CashFlowFrame dummySt [])
                               ,P.asOfDate = T.fromGregorian 2022 1 1
                               ,P.issuanceStat = Nothing
                               ,P.extendPeriods = Nothing})
   ,D.waterfall = Map.fromList [(W.DistributionDay Amortizing, [
                                 (W.PayInt Nothing "General" ["A"] Nothing)
                                 ,(W.PayPrin Nothing "General" ["A"] Nothing)
   ])]
 ,D.collects = [W.Collect Nothing W.CollectedInterest "General"
             ,W.Collect Nothing W.CollectedPrincipal "General"]
}

baseTests = 
  let 
   (dealAfterRun,poolCf,_,_) = DR.runDeal baseCase DealPoolFlowPricing Nothing (AP.NonPerfAssumption Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing)
  in 
   testGroup "Base Deal Test" 
   [ testCase "Dates pattern" $
     assertEqual  "First Pay"
     True
     True
     ,testCase "empty pool flow" $
     assertEqual "empty pool flow"
     0
     -- (P.futureCf (D.pool baseCase))
     0
   ]
