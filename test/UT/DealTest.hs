module UT.DealTest(td)
where

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

import qualified Data.Map as Map
import qualified Data.Time as T
import qualified Data.Set as S

td = TestDeal {
  D.name = "test deal1"
  ,D.dates = (Map.fromList [("closing-date",(T.fromGregorian 2022 1 1))
                         ,("cutoff-date",(T.fromGregorian 2022 1 1))
                         ,("first-pay-date",(T.fromGregorian 2022 2 25))
                         ])
  ,D.payPeriod = Monthly
  ,D.collectPeriod = Monthly
  ,D.accounts = (Map.fromList
  [("General", (A.Account { A.accName="General" ,A.accBalance=0.0 ,A.accType=Nothing, A.accInterest=Nothing ,A.accStmt=Nothing
  })),
   ("Reserve", (A.Account { A.accName="General" ,A.accBalance=0.0 ,A.accType=Just (A.FixReserve 500), A.accInterest=Nothing ,A.accStmt=Nothing
  }))
  ])
  ,D.fees = (Map.fromList [("Service-Fee"
                         ,F.Fee{F.feeName="service-fee"
                                ,F.feeType = F.FixFee
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
                             ,L.bndInterestInfo= L.Fix 0.08
                             ,L.bndBalance=3000
                             ,L.bndRate=0.08
                             ,L.bndDuePrin=0.0
                             ,L.bndDueInt=0.0
                             ,L.bndLastIntPay = Just (T.fromGregorian 2022 1 1)
                             ,L.bndLastPrinPay = Just (T.fromGregorian 2022 1 1)
                             ,L.bndStmt=Nothing})
                         ]
           )
  ,D.pool = P.Pool {P.assets=[P.Mortgage
                                         P.OriginalInfo{
                                           P.originBalance=4000
                                           ,P.originRate=P.Fix 0.085
                                           ,P.originTerm=60
                                           ,P.period=Monthly
                                           ,P.startDate=(T.fromGregorian 2022 1 1)
                                           ,P.prinType= P.Level}
                                         4000
                                         0.085
                                         60]
                 ,P.futureCf=Nothing
                 ,P.asOfDate = T.fromGregorian 2022 1 1}
   ,D.waterfall = Map.fromList [(W.DistributionDay, [
                                 W.PayFee ["General"] ["Service-Fee"]
                                 ,W.PayFeeBy (W.DuePct 0.5) ["General"] ["Service-Fee"]
                                 ,W.TransferReserve W.TillSource  "General" "General" Nothing
                                 ,W.TransferReserve W.TillTarget  "General" "General" Nothing
                                 ,W.PayInt "General" ["A"]
                                 ,W.PayPrin "General" ["A"]
   ])]
 ,D.collects = [W.Collect W.CollectedInterest "General"
             ,W.Collect W.CollectedPrincipal "General"]
 ,D.call = Nothing
}
