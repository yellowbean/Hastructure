{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Deal (TestDeal,run) where

import qualified Accounts as A
import qualified Asset as P
--import qualified Asset (Mortgage, Pool) as P
import qualified Equity as E
import qualified Expense as F
import qualified Liability as L
import qualified Waterfall as W
import qualified Cashflow as CF
import Lib
-- import Pool
-- import qualified Data.HashMap.Strict as M

import qualified Data.Map as Map
import qualified Data.Time as T
import qualified Data.Set as S
import Data.List
import Debug.Trace
import Data.Aeson hiding (json)
import Language.Haskell.TH
import Data.Aeson.TH
import Data.Aeson.Types

class SPV a where
  projBondCashflow :: a -> ()
  projAssetCashflow :: a -> ()

class Statmet s where 
  consolidate :: s -> s  


data TestDeal = TestDeal {
  name :: String
  ,dates :: Map.Map String T.Day
  ,payPeriod :: Period
  ,collectPeriod :: Period
  ,accounts :: Map.Map String A.Account
  ,fees :: Map.Map String F.Fee
  ,bonds :: Map.Map String L.Bond
  ,pool ::  (P.Pool P.Mortgage)
  ,waterfall :: W.DistributionSeq
  ,collects :: [W.CollectionRule]
} deriving (Show)


$(deriveJSON defaultOptions ''TestDeal)

td = TestDeal {
  name = "test deal1"
  ,dates = (Map.fromList [("closing-date",(T.fromGregorian 2022 1 1))
                         ,("cutoff-date",(T.fromGregorian 2022 1 1))
                         ,("first-pay-date",(T.fromGregorian 2022 2 25))
                         ])
  ,payPeriod = Monthly
  ,collectPeriod = Monthly
  ,accounts = (Map.fromList [("General", (A.Account {
   A.accName="General" ,A.accBalance=0.0 ,A.accType=A.Virtual ,A.accStmt=Nothing
  }))])
  ,fees = (Map.fromList [("Service-Fee"
                         ,F.Fee{F.feeName="service-fee"
                                ,F.feeType = (F.FixFee 500)
                                ,F.feeStart = (T.fromGregorian 2022 1 1)
                                ,F.feeDue = 0
                                ,F.feeArrears = 0
                                ,F.feeLastPaidDay = Nothing
                                ,F.feeStmt = Nothing})])
  ,bonds = (Map.fromList [("A"
                          ,L.Bond{
                              L.bndName="A"
                             ,L.bndType=L.Passthrough
                             ,L.bndOriginInfo= L.OriginalInfo{
                                                L.originBalance=3000
                                                ,L.originDate= (T.fromGregorian 2022 1 1)
                                                ,L.originRate= 0.08
                                                ,L.originLockoutEnd=Nothing}
                             ,L.bndInterestInfo= L.Fix 0.08
                             ,L.bndBalance=3000
                             ,L.bndDuePrin=0.0
                             ,L.bndDueInt=0.0
                             ,L.bndLastIntPay = Just (T.fromGregorian 2022 1 1)
                             ,L.bndLastPrinPay = Just (T.fromGregorian 2022 1 1)
                             ,L.bndStmt=Nothing})
                          ])
  ,pool = P.Pool {P.assets=[P.Mortgage
                                         P.OriginalInfo{
                                           P.originBalance=4000
                                           ,P.originRate=0.085
                                           ,P.originTerm=360
                                           ,P.period=Monthly
                                           ,P.startDate=(T.fromGregorian 2022 1 1)}
                                         4000
                                         0.085
                                         360
                                         ]}
 ,waterfall = [
   W.PayFee "General" ["Service-Fee"]
   ,W.PayInt "General" ["A"]
   ,W.PayPrin "General" ["A"]
   ]
 ,collects = [W.Collect W.CollectedInterest "General"
             ,W.Collect W.CollectedPrincipal "General"]
}

performAction :: T.Day -> TestDeal -> W.Action -> TestDeal
performAction d t (W.Transfer an1 an2) =
  t {accounts = accMapAfterDeposit}
  where
    accMap = (accounts t)
    sourceAcc = Map.lookup an1 accMap
    transferAmt = case sourceAcc of
                    Just acc -> (A.accBalance acc)
                    Nothing -> 0
    accMapAfterDraw = Map.adjust (A.draw transferAmt d "") an1 accMap
    accMapAfterDeposit = Map.adjust (A.deposit transferAmt d "") an2 accMapAfterDraw

performAction d t (W.PayFee an fns) =
  t {accounts = accMapAfterPay, fees = feeMapUpdated}
  where
    feeMap = (fees t)
    accMap = (accounts t)
    acc = (trace ("pay fee acc map"++show(accMap)) accMap) Map.! (trace ("payfee using acc"++show(an)) an )

    feesToPay = map (\x -> feeMap Map.! x ) fns
    feesWithDue = map (\x -> calcDueFee t d x) feesToPay
    feeDueAmts = map (\x -> (F.feeDue x) ) feesWithDue

    availBal = A.accBalance acc
    actualPaidOut = min availBal $ foldl (+) 0 feeDueAmts
    feesAmountToBePaid = zip feesWithDue  $ prorataFactors feeDueAmts availBal
    feesPaid = map (\(f,amt) -> (F.payFee d amt f)) feesAmountToBePaid

    feeMapUpdated = Map.union (Map.fromList $ zip fns feesPaid) feeMap
    accMapAfterPay = Map.adjust (A.draw actualPaidOut d "Pay Fee") an accMap

performAction d t (W.PayInt an bnds) =
  t {accounts = accMapAfterPay, bonds = bndMapUpdated}
  where
    bndMap = trace ("bond maps =>"++ show(bonds t)) (bonds t)
    accMap = (accounts t)
    acc = (trace ("pay int acc map"++show(accMap)) accMap) Map.! (trace ("pay Int using acc"++show(an)) an )

    bndsToPay = map (\x -> bndMap Map.! (trace ("Looking up"++show(x)) x )) (trace ("bond map"++show(bndMap)) bnds)

    bndsWithDue = map (\x -> calcDueInt t d x) bndsToPay
    bndsDueAmts = map (\x -> (L.bndDueInt x) ) bndsWithDue

    availBal = A.accBalance acc
    actualPaidOut = min availBal $ foldl (+) 0 bndsDueAmts
    bndsAmountToBePaid = zip bndsWithDue  $ prorataFactors bndsDueAmts availBal
    bndsPaid = map (\(l,amt) -> (L.payInt d amt l)) bndsAmountToBePaid

    bndMapUpdated =   Map.union (Map.fromList $ zip bnds bndsPaid) bndMap
    accMapAfterPay = Map.adjust (A.draw actualPaidOut d "Pay Int") an accMap

performAction d t (W.PayPrin an bnds) =
  t {accounts = accMapAfterPay, bonds = bndMapUpdated}
  where
    bndMap = (bonds t)
    accMap = (accounts t)
    acc = accMap Map.! (trace ("pay PRIN using acc"++show(an)) an )

    bndsToPay = map (\x -> bndMap Map.! x ) bnds
    bndsWithDue = map (\x -> calcDuePrin t d x) bndsToPay
    bndsDueAmts = map (\x -> (L.bndDuePrin x) ) bndsWithDue

    availBal = A.accBalance acc
    actualPaidOut = min availBal $ foldl (+) 0 bndsDueAmts
    bndsAmountToBePaid = zip bndsWithDue  $ prorataFactors bndsDueAmts availBal
    bndsPaid = map (\(l,amt) -> (L.payPrin d amt l)) bndsAmountToBePaid

    bndMapUpdated =  Map.union (Map.fromList $ zip bnds bndsPaid) bndMap
    accMapAfterPay = Map.adjust (A.draw actualPaidOut d "Pay Prin") an accMap


-- data Forecast = PoolFlow CF.CashFlowFrame
-- TODO fix pool collection period , looks like it is flows steps of bonds
run :: TestDeal -> Int -> TestDeal
run t currentPeriod =
  if ((trace ("current p"++show(currentPeriod)) currentPeriod) < (trace ("Pool size"++show(poolCfSize)) poolCfSize))
    then
      let
         runDate =  pCollectionDates!!currentPeriod
         accs = depositPoolInflow (collects t) runDate pCollectionCf (accounts t)
         tWithNewCollection = t {accounts=accs}
      in
      run
        (if ((fromIntegral firstPayIndex) <= currentPeriod) then 
            (foldl (performAction (bPayDates!!(currentPeriod - 1)))
                   tWithNewCollection 
                   (waterfall t))
            --tWithNewCollection
        else 
            tWithNewCollection)
        (currentPeriod + 1)
    else
      (trace "DONE" t)
  where
    startDate = Map.findWithDefault (T.fromGregorian 1970 1 1) "closing-date" (dates t)
    firstPayDate = Map.findWithDefault (T.fromGregorian 1970 1 1) "first-pay-date" (dates t)
    poolCf = rPool $ pool t
    pCollectionInt = (collectPeriod t)
    bPayInt = (payPeriod t)
    pCollectionDates = map (\x -> afterNPeriod startDate x pCollectionInt) [1..100]
    firstPayIndex = length $ takeWhile (\x -> x < firstPayDate ) pCollectionDates
    bPayDates = (replicate (firstPayIndex - 1) (T.fromGregorian 1970 1 1)) ++ map (\x -> afterNPeriod firstPayDate x bPayInt) [0..100]

    pCollectionCf = CF.CashFlowFrame $ CF.aggTsByDates 
                   ( CF.getTsCashFlowFrame poolCf) 
                   (trace ("coll dates"++show(pCollectionDates)) pCollectionDates)
    poolCfSize =  CF.sizeCashFlowFrame pCollectionCf


rPool :: (P.Pool P.Mortgage) -> CF.CashFlowFrame
rPool the_pool  =
  P.aggPool $ P.runPool (P.assets the_pool)

getPoolBalance :: TestDeal -> Float
getPoolBalance TestDeal{pool=p} = 100
  --foldl (getCurrentBal + ) 0 p.assets

getBondBalance :: TestDeal -> Float
getBondBalance TestDeal{pool=p} = 100

calcDueFee :: TestDeal -> T.Day -> F.Fee -> F.Fee
calcDueFee t calcDay f@(F.Fee fn (F.FixFee amt)  fs fd fa _ _)
  = f{ F.feeDue = amt}
calcDueFee t calcDay f@(F.Fee fn (F.AnnualRateFee base r) fs fd fa maybeFlpd _)
  = case maybeFlpd of
      (Just flpd ) ->
        f{ F.feeDue = fd + baseBal * r * (periodToYear flpd calcDay ACT_360)}
      Nothing ->
        f{ F.feeDue = fd + baseBal * r * (periodToYear tClosingDate calcDay ACT_360)}
    where
      baseBal =  case base of
        F.CurrentPoolBalance -> getPoolBalance t
        F.CurrentBondBalance -> getBondBalance t
        _ -> 0.0
      tClosingDate = Map.findWithDefault (T.fromGregorian 1970 1 1) "closing-date" (dates t)


calcDueInt :: TestDeal -> T.Day -> L.Bond -> L.Bond
calcDueInt t calc_date b@(L.Bond bn bt  bo bi bond_bal _ _ lstIntPay _ _) =
  b {L.bndDueInt = (dueInt+int_arrears) }
  where
    int_arrears = 0
    lastIntPayDay = case lstIntPay of
                      Just pd -> pd
                      Nothing -> Map.findWithDefault (T.fromGregorian 1970 1 1) "closing-date" (dates t)
    dueInt = calcInt bond_bal lastIntPayDay calc_date 0.08 ACT_365


calcDuePrin :: TestDeal -> T.Day -> L.Bond -> L.Bond
calcDuePrin t calc_date b@(L.Bond bn bt bo bi bond_bal prin_arr int_arrears _ _ _) =
  b {L.bndDuePrin = duePrin}
  where
    duePrin = bond_bal


depositPoolInflow :: [W.CollectionRule] -> T.Day -> CF.CashFlowFrame -> Map.Map String A.Account -> Map.Map String A.Account
depositPoolInflow rules d cf amap =
  foldl fn amap rules
  where
      currentPoolInflow = CF.getSingleTsCashFlowFrame (trace ("cf to find->"++ show(cf)) cf) (trace ("D->"++show(d)) d)
      fn _acc _r@(W.Collect _ _accName) =
          Map.adjust (A.deposit collectedCash d "") _accName _acc
          where 
              collectedCash = collectCash _r (trace ("inflow=>" ++ show(currentPoolInflow)) currentPoolInflow)
      collectCash r ts =
        case (trace ("running collect rule =>" ++ show(r)) r) of
          (W.Collect W.CollectedInterest _)   -> CF.mflowInterest (trace ("ts to be collected=> " ++ show(ts)) ts)
          (W.Collect W.CollectedPrincipal _)  -> CF.mflowPrincipal ts
          (W.Collect W.CollectedRecoveries _) -> CF.mflowRecovery ts
          (W.Collect W.CollectedPrepayment _) -> CF.mflowPrepayment ts
