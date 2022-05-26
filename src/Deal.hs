{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Deal (TestDeal) where

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
import       Data.Aeson       hiding (json)
import Language.Haskell.TH
import Data.Aeson.TH
import Data.Aeson.Types

class SPV a where
  projBondCashflow :: a -> ()
  projAssetCashflow :: a -> ()

data TestDeal = TestDeal {
  name :: String
  ,dates :: Map.Map String T.Day
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
                         ,("cutoff-date",(T.fromGregorian 2022 1 1))])
  ,accounts = (Map.fromList [("General", (A.Account {
   A.accName="General" ,A.accBalance=0.0 ,A.accType=A.Virtual ,A.accStmt=Nothing
  }))])
  ,fees = (Map.fromList [("Service-Fee"
                         ,F.Fee{F.feeName="service-fee"
                                ,F.feeType = (F.FixFee 500)
                                ,F.feeStart = (T.fromGregorian 2022 1 1)
                                ,F.feeDue = 0
                                ,F.feeArrears = 0
                                ,F.feeLastPaidDay = Nothing})])
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
    acc = accMap Map.! an

    feesToPay = map (\x -> feeMap Map.! x ) fns
    feesWithDue = map (\x -> calcDueFee t d x) feesToPay
    feeDueAmts = map (\x -> (F.feeDue x) ) feesWithDue

    availBal = A.accBalance acc
    actualPaidOut = min availBal $ foldl (+) 0 feeDueAmts
    feesAmountToBePaid = zip feesWithDue  $ prorataFactors feeDueAmts availBal
    feesPaid = map (\(f,amt) -> (F.payFee d amt f)) feesAmountToBePaid

    feeMapUpdated = Map.fromList $ zip fns feesPaid
    accMapAfterPay = Map.adjust (A.draw actualPaidOut d "Pay Fee") an accMap

performAction d t (W.PayInt an bnds) =
  t {accounts = accMapAfterPay, bonds = bndMapUpdated}
  where
    bndMap = (bonds t)
    accMap = (accounts t)
    acc = accMap Map.! an

    bndsToPay = map (\x -> bndMap Map.! x ) bnds
    bndsWithDue = map (\x -> calcDueInt t d x) bndsToPay
    bndsDueAmts = map (\x -> (L.bndDueInt x) ) bndsWithDue

    availBal = A.accBalance acc
    actualPaidOut = min availBal $ foldl (+) 0 bndsDueAmts
    bndsAmountToBePaid = zip bndsWithDue  $ prorataFactors bndsDueAmts availBal
    bndsPaid = map (\(l,amt) -> (L.payInt d amt l)) bndsAmountToBePaid

    bndMapUpdated = Map.fromList $ zip bnds bndsPaid
    accMapAfterPay = Map.adjust (A.draw actualPaidOut d "Pay Int") an accMap

performAction d t (W.PayPrin an bnds) = t
  t {accounts = accMapAfterPay, bonds = bndMapUpdated}
  where
    bndMap = (bonds t)
    accMap = (accounts t)
    acc = accMap Map.! an

    bndsToPay = map (\x -> bndMap Map.! x ) bnds
    bndsWithDue = map (\x -> calcDuePrin t d x) bndsToPay
    bndsDueAmts = map (\x -> (L.bndDuePrin x) ) bndsWithDue

    availBal = A.accBalance acc
    actualPaidOut = min availBal $ foldl (+) 0 bndsDueAmts
    bndsAmountToBePaid = zip bndsWithDue  $ prorataFactors bndsDueAmts availBal
    bndsPaid = map (\(l,amt) -> (L.payPrin d amt l)) bndsAmountToBePaid

    bndMapUpdated = Map.fromList $ zip bnds bndsPaid
    accMapAfterPay = Map.adjust (A.draw actualPaidOut d "Pay Prin") an accMap


data Forecast = PoolFlow CF.CashFlowFrame

run :: [Forecast] -> TestDeal -> Int -> TestDeal
run fcst t currentPeriod =
  if (currentPeriod < poolCfSize)
    then
      let
         runDate = (T.fromGregorian 2022 1 1)
         accs = depositPoolInflow (collects t) runDate poolCf (accounts t)
         tWithNewCollection = t {accounts=accs}
      in
      run
        fcst
        (foldl (performAction runDate) tWithNewCollection (waterfall t))
        (currentPeriod + 1)
    else
      t
  where
    poolCf = rPool $ pool t
    poolCollectionCf = CF.aggTsByDates $ CF.getTsCashFlowFrame poolCf
    poolCfSize =  CF.sizeCashFlowFrame poolCf


rPool :: (P.Pool P.Mortgage) -> CF.CashFlowFrame
rPool the_pool  =
  P.aggPool $ P.runPool (P.assets the_pool)

getPoolBalance :: TestDeal -> Float
getPoolBalance TestDeal{pool=p} = 100
  --foldl (getCurrentBal + ) 0 p.assets

calcDueFee :: TestDeal -> T.Day -> F.Fee -> F.Fee
calcDueFee t calcDay f@(F.Fee fn (F.FixFee amt)  fs fd fa (Just flpd))
  = f{ F.feeDue = amt}
calcDueFee t calcDay f@(F.Fee fn (F.PctFee base r)  fs fd fa (Just flpd))
  = f{ F.feeDue = fd + baseBal * r * (periodToYear flpd calcDay ACT_360)}
  where
    baseBal =  case base of
      F.CurrentPoolBalance -> getPoolBalance t

calcDueInt :: TestDeal -> T.Day -> L.Bond -> L.Bond
calcDueInt t calc_date b@(L.Bond bn bt  bo bi bond_bal _ _ (Just lstIntPay) (Just lstPrinPay) _) =
  b {L.bndDueInt = (dueInt+int_arrears) }
  where
    int_arrears = 0
    dueInt = calcInt bond_bal lstIntPay calc_date 0.08 ACT_365


calcDuePrin :: TestDeal -> T.Day -> L.Bond -> L.Bond
calcDuePrin t calc_date b@(L.Bond bn bt bo bi bond_bal prin_arr int_arrears (Just lstIntPay) (Just lstPrinPay) _) =
  b {L.bndDuePrin = duePrin}
  where
    duePrin = bond_bal


depositPoolInflow :: [W.CollectionRule] -> T.Day -> CF.CashFlowFrame -> Map.Map String A.Account -> Map.Map String A.Account
depositPoolInflow rules d cf amap =
  foldl fn amap rules 
  where
      currentPoolInflow = CF.getSingleTsCashFlowFrame cf d
      fn _acc _r@(W.Collect _ _accName) =
          Map.adjust (A.deposit collectedCash d "") _accName _acc
          where 
              collectedCash = collectCash _r currentPoolInflow
      
      collectCash r ts =
        case r of
          (W.Collect W.CollectedInterest _)   -> CF.mflowInterest ts
          (W.Collect W.CollectedPrincipal _)  -> CF.mflowPrincipal ts
          (W.Collect W.CollectedRecoveries _) -> CF.mflowRecovery ts
