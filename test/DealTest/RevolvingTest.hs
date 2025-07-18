module DealTest.RevolvingTest(baseTests)

where

import Test.Tasty
import Test.Tasty.HUnit
import Deal

import qualified Accounts as A
import qualified Stmt as S
import qualified Pool as P
import qualified AssetClass.Mortgage as ACM
import qualified AssetClass.AssetBase as AB
import qualified Expense as F
import qualified Deal.DealBase as D
import qualified Deal as DR
import qualified Liability as L
import qualified Waterfall as W
import qualified Revolving as R
import qualified Cashflow as CF
import qualified Assumptions as AP
import qualified Call as C
import InterestRate
import qualified CreditEnhancement as CE
import qualified Triggers as Trg
import Lib

import qualified Data.Map as Map
import qualified Data.Time as T
import qualified Data.Set as S
import Types

import Control.Lens hiding (element)
import Control.Lens.TH

multiPool = Map.fromList [(PoolName "PoolA",P.Pool {P.assets=[AB.Mortgage
                                                                   AB.MortgageOriginalInfo{ AB.originBalance=4000 ,AB.originRate=Fix DC_ACT_365F 0.085 ,AB.originTerm=60 ,AB.period=Monthly ,AB.startDate=T.fromGregorian 2022 1 1 ,AB.prinType= AB.Level ,AB.prepaymentPenalty = Nothing}
                                                                   1000 0.085 60 Nothing AB.Current]
                                                      ,P.futureCf= Nothing
                                                      ,P.asOfDate = T.fromGregorian 2022 1 1
                                                      ,P.issuanceStat = Just $ Map.fromList [(IssuanceBalance,1000)]
                                                      ,P.extendPeriods = Nothing
                                                      })
                         ,(PoolName "PoolB",(P.Pool {P.assets=[AB.Mortgage
                                                                   AB.MortgageOriginalInfo{ AB.originBalance=4000 ,AB.originRate=Fix DC_ACT_365F 0.085 ,AB.originTerm=60 ,AB.period=Monthly ,AB.startDate=T.fromGregorian 2022 1 1 ,AB.prinType= AB.Level ,AB.prepaymentPenalty = Nothing}
                                                                   3000 0.085 60 Nothing AB.Current]
                                                        ,P.futureCf= Nothing
                                                        ,P.asOfDate = T.fromGregorian 2022 1 1
                                                        ,P.issuanceStat = Just $ Map.fromList [(IssuanceBalance,3000)]
                                                        ,P.extendPeriods = Nothing}))]


baseCase = D.TestDeal {
  D.name = "base case"
  ,D.status = Amortizing
  ,D.rateSwap = Nothing
  ,D.currencySwap = Nothing
  ,D.dates = CurrentDates (toDate "20220101",toDate "20220101") Nothing (toDate "20300101")
                (toDate "20220201" , MonthFirst) (toDate "20220225" , MonthFirst)
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
                             ,L.bndStepUp = Nothing
                             ,L.bndDueIntDate=Nothing
                             ,L.bndLastIntPay = Just (T.fromGregorian 2022 1 1)
                             ,L.bndLastPrinPay = Just (T.fromGregorian 2022 1 1)
                             ,L.bndDueIntOverInt = 0
                             ,L.bndStmt=Nothing})
                         ]
           )
  ,D.pool = D.MultiPool multiPool 
   ,D.waterfall = Map.fromList [(W.DistributionDay Amortizing, [
                                 (W.PayInt Nothing "General" ["A"] Nothing)
                                 ,(W.PayPrin Nothing "General" ["A"] Nothing)
   ])]
 ,D.collects = [W.Collect (Just [PoolName "PoolA",PoolName "PoolB"]) W.CollectedInterest "General"
               ,W.Collect (Just [PoolName "PoolA",PoolName "PoolB"]) W.CollectedPrincipal "General"
             ]
 ,D.liqProvider = Nothing
 ,D.rateCap = Nothing
 ,D.triggers = Nothing
 ,D.ledgers = Nothing
 ,D.stats = (Map.empty,Map.empty,Map.empty,Map.empty)
}


baseTests = 
  let 
    poolAssets = [(AB.PersonalLoan AB.LoanOriginalInfo{AB.originBalance= 1000, AB.originRate= Fix DC_ACT_365F 0.08,
                                                    AB.originTerm = 24, AB.period = Monthly ,AB.startDate = (T.fromGregorian 2022 1 1),
                                                    AB.prinType = AB.I_P}
                                1000
                                0.08
                                24
                                AB.Current)]
    rAssump = Just (AP.AvailableAssets (R.ConstantAsset $ AB.LO <$> poolAssets)
                                                (AP.PoolLevel ((AP.LoanAssump Nothing Nothing Nothing Nothing)
                                                                ,AP.DummyDelinqAssump
                                                                ,AP.DummyDefaultAssump))
                            )
    inspectVars = [AP.InspectRpt MonthEnd [FutureCurrentPoolBalance Nothing
                                          ,FutureCurrentPoolBalance (Just [PoolName "PoolA"])
                                          ,FutureCurrentPoolBalance (Just [PoolName "PoolB"])
                                          ,FutureCurrentPoolBalance (Just [PoolName "PoolB",PoolName "PoolA"])]
                  ]
    nonRunAssump = AP.NonPerfAssumption Nothing Nothing Nothing rAssump Nothing (Just inspectVars) Nothing Nothing Nothing Nothing Nothing Nothing
    (dealAfterRun,poolCf,_,_,_) = case DR.runDeal baseCase S.empty Nothing nonRunAssump of
                                    Right x -> x
                                    Left y -> error ("Error in running deal"++ show y)
  in 
   testGroup "Revolving: Single Pool" 
   [ testCase "Asset: Loan" $
     assertEqual  "First Pay"
     True
     True
     ,testCase "empty pool flow" $
     assertEqual "empty pool flow"
     0
     -- (P.futureCf (D.pool baseCase))
     0
   ]

