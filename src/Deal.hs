{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Deal (TestDeal,run,run2,getInits,runDeal,ExpectReturn(..)) where

import qualified Accounts as A
import qualified Asset as P
--import qualified Asset (Mortgage, Pool) as P
import qualified Equity as E
import qualified Expense as F
import qualified Liability as L
import qualified Waterfall as W
import qualified Cashflow as CF
import qualified Assumptions as AP
import Lib
-- import Pool
-- import qualified Data.HashMap.Strict as M

import qualified Data.Map as Map
import qualified Data.Time as T
import qualified Data.Set as S
import qualified Control.Lens as LS
import Data.List
import Data.Maybe
import Data.Aeson hiding (json)
import Language.Haskell.TH
import Data.Aeson.TH
import Data.Aeson.Types

class SPV a where
  projBondCashflow :: a -> ()
  projAssetCashflow :: a -> ()


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
   A.accName="General" ,A.accBalance=0.0 ,A.accType=Nothing, A.accInterest=Nothing ,A.accStmt=Nothing
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
                             ,L.bndRate=0.08
                             ,L.bndDuePrin=0.0
                             ,L.bndDueInt=0.0
                             ,L.bndLastIntPay = Just (T.fromGregorian 2022 1 1)
                             ,L.bndLastPrinPay = Just (T.fromGregorian 2022 1 1)
                             ,L.bndStmt=Nothing})
                         ,("B"
                          ,L.Bond{
                              L.bndName="B"
                             ,L.bndType=L.Passthrough
                             ,L.bndOriginInfo= L.OriginalInfo{
                                                L.originBalance=3000
                                                ,L.originDate= (T.fromGregorian 2022 1 1)
                                                ,L.originRate= 0.08
                                                ,L.originLockoutEnd=Nothing}
                             ,L.bndInterestInfo= L.Floater LIBOR6M 0.01 0.085 Quarterly Nothing Nothing
                             ,L.bndBalance=3000
                             ,L.bndRate=0.08
                             ,L.bndDuePrin=0.0
                             ,L.bndDueInt=0.0
                             ,L.bndLastIntPay = Just (T.fromGregorian 2022 1 1)
                             ,L.bndLastPrinPay = Just (T.fromGregorian 2022 1 1)
                             ,L.bndStmt=Nothing})])
  ,pool = P.Pool {P.assets=[P.Mortgage
                                         P.OriginalInfo{
                                           P.originBalance=4000
                                           ,P.originRate=P.Fix 0.085
                                           ,P.originTerm=60
                                           ,P.period=Monthly
                                           ,P.startDate=(T.fromGregorian 2022 1 1)}
                                         4000
                                         0.085
                                         60
                                         ]}
 ,waterfall = [
   W.PayFee "General" ["Service-Fee"]
   ,W.PayInt "General" ["A"]
   ,W.PayPrin "General" ["A"]
   ]
 ,collects = [W.Collect W.CollectedInterest "General"
             ,W.Collect W.CollectedPrincipal "General"]
}

--runDeal 
--    td 
--    DealStatus 
--    (Just ([AP.InterestRateCurve LIBOR6M [(T.fromGregorian 2020 1 1,0.03)]]))

performAction :: T.Day -> TestDeal -> W.Action -> TestDeal
performAction d t (W.Transfer an1 an2) =
  t {accounts = accMapAfterDeposit}
  where
    accMap = (accounts t)
    sourceAcc = Map.lookup an1 accMap
    transferAmt = case sourceAcc of
                    Just acc -> (A.accBalance acc)
                    Nothing -> 0
    accMapAfterDraw = Map.adjust (A.draw transferAmt d ("Transfer To:"++an2)) an1 accMap
    accMapAfterDeposit = Map.adjust (A.deposit transferAmt d ("Transfer From:"++an1)) an2 accMapAfterDraw

performAction d t (W.ReserveTransferSource sa ta) =
    t {accounts = accMapAfterTransfer }
  where
    accMap = (accounts t)
    sourceAcc = accMap Map.! sa
    targetAcc = accMap Map.! ta
    sourceBal = calcTargetAmount t sourceAcc
    transferAmt = max ((A.accBalance sourceAcc) - sourceBal ) 0
    accMapAfterTransfer
      = case transferAmt of
          0 -> accMap
          amt ->  Map.adjust (A.draw amt d "withdraw") sa  $ Map.adjust (A.deposit amt d "transfer") ta $ accMap

performAction d t (W.ReserveTransferTarget sa ta) =
     t {accounts = accMapAfterTransfer }
   where
     accMap = (accounts t)
     sourceAcc = accMap Map.! sa
     targetAcc = accMap Map.! ta
     targetBal = calcTargetAmount t targetAcc
     transferAmtTarget = max (targetBal - (A.accBalance targetAcc)) 0
     actualTransferAmt = min transferAmtTarget (A.accBalance sourceAcc)
     accMapAfterTransfer
       = case actualTransferAmt of
           0 -> accMap
           amt ->  Map.adjust (A.draw amt d "withdraw") sa  $ Map.adjust (A.deposit amt d "transfer") ta $ accMap

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

    feeMapUpdated = Map.union (Map.fromList $ zip fns feesPaid) feeMap
    accMapAfterPay = Map.adjust (A.draw actualPaidOut d "Pay Fee") an accMap

performAction d t (W.PayInt an bnds) =
  t {accounts = accMapAfterPay, bonds = bndMapUpdated}
  where
    bndMap = (bonds t)
    accMap = (accounts t)
    acc = accMap Map.! an 

    bndsToPay = map (\x -> bndMap Map.! x ) bnds
    bndsWithDue = filter (\x -> ((L.bndDueInt x) > 0)) $ map (\x -> calcDueInt t d x) bndsToPay
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
    acc = accMap Map.! an 

    bndsToPay = filter (\x -> ((L.bndBalance x) > 0)) $ map (\x -> bndMap Map.! x ) bnds
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
  if ( currentPeriod <  poolCfSize)
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
       t
  where
    startDate = Map.findWithDefault (T.fromGregorian 1970 1 1) "closing-date" (dates t)
    firstPayDate = Map.findWithDefault (T.fromGregorian 1970 1 1) "first-pay-date" (dates t)
    poolCf = P.aggPool $ P.runPool (P.assets (pool t)) Nothing
    pCollectionInt = (collectPeriod t)
    bPayInt = (payPeriod t)
    pCollectionDates = map (\x -> afterNPeriod startDate x pCollectionInt) [1..100]
    firstPayIndex = length $ takeWhile (\x -> x < firstPayDate ) pCollectionDates
    bPayDates = (replicate (firstPayIndex - 1) (T.fromGregorian 1970 1 1)) ++ map (\x -> afterNPeriod firstPayDate x bPayInt) [0..100]

    pCollectionCf = CF.CashFlowFrame $ CF.aggTsByDates 
                   ( CF.getTsCashFlowFrame poolCf) 
                   pCollectionDates
    poolCfSize =  CF.sizeCashFlowFrame pCollectionCf

data ActionOnDate = CollectPoolIncome T.Day
                   |RunWaterfall T.Day
                   deriving (Show)

instance Ord ActionOnDate where
  compare (CollectPoolIncome d1) (CollectPoolIncome d2) = compare d1 d2
  compare (RunWaterfall d1) (RunWaterfall d2) = compare d1 d2
  compare (CollectPoolIncome d1) (RunWaterfall d2) = compare d1 d2
  compare (RunWaterfall d1) (CollectPoolIncome d2) = compare d1 d2

instance Eq ActionOnDate where
  (CollectPoolIncome d1) == (CollectPoolIncome d2) = d1 == d2
  (RunWaterfall d1) == (RunWaterfall d2) = d1 == d2
  (CollectPoolIncome d1) == (RunWaterfall d2) = d1 == d2
  (RunWaterfall d1) == (CollectPoolIncome d2) = d1 == d2


setBondNewRate :: T.Day -> [RateAssumption] -> L.Bond -> L.Bond
setBondNewRate d ras b@(L.Bond _ _ _ ii _ _ _ _ _ _ _) 
  = b { L.bndRate = (applyFloatRate ii d ras) }

applyFloatRate :: L.InterestInfo -> T.Day -> [RateAssumption] -> Float 
applyFloatRate (L.Floater idx spd rt p f c) d ras
  = idx_rate + spd
    where 
      idx_rate = case ra of 
        Just (RateCurve _idx _ts) -> getRateByDate _ts d 
        Nothing -> -0.5 
      ra = find (\(RateCurve _idx _ts) -> (_idx==idx)) ras 

setBndsNextIntRate :: TestDeal -> T.Day -> Maybe [RateAssumption] -> TestDeal 
setBndsNextIntRate t d (Just ras) = t {bonds = updatedBonds}
    where 
        isFloat (L.Bond _ _ _ (L.Floater _ _ _ _ _ _) _ _ _ _ _ _ _ ) = True
        isFloat (L.Bond _ _ _ (L.Fix _ ) _ _ _ _ _ _ _ ) = False
        floatBonds = filter (\x -> isFloat x) $ Map.elems (bonds t)
        floatBondNames = map (\x -> (L.bndName x)) floatBonds
        updatedBonds = foldr (Map.adjust (setBondNewRate d ras)) (bonds t) floatBondNames

setBndsNextIntRate t d Nothing = t 

run2 :: TestDeal -> Maybe CF.CashFlowFrame -> Maybe [ActionOnDate]
    -> Maybe [RateAssumption] -> TestDeal
run2 t (Just _poolFlow) (Just (ad:ads)) rates =
      case ad of
        CollectPoolIncome d ->
          run2
            (t {accounts=accs})
            (CF.removeTsCashFlowFrameByDate _poolFlow d)
            (Just ads)
            rates
          where
            accs = depositPoolInflow (collects t) d _poolFlow (accounts t)
        RunWaterfall d ->
          run2
            (setBndsNextIntRate 
                (foldl (performAction d) t (waterfall t))
                d 
                rates)
            (Just _poolFlow)
            (Just ads)
            rates

run2 t Nothing Nothing Nothing =
    run2 t (Just pcf) (Just ads) Nothing
  where
    (ads,pcf,rcurves) = getInits t Nothing 

run2 t Nothing _ _ = (prepareDeal t)

data ExpectReturn = DealStatus
                  | DealPoolFlow
                  deriving (Show)

runDeal :: TestDeal -> ExpectReturn -> Maybe [AP.AssumptionBuilder] 
        -> (TestDeal,Maybe CF.CashFlowFrame)
runDeal t er assumps =
  case er of
    DealStatus ->  (finalDeal, Nothing)
    DealPoolFlow -> (finalDeal, Just pcf)

  where
    finalDeal = run2 t (Just pcf) (Just ads) (Just rcurves)
    (ads,pcf,rcurves) = getInits t assumps

prepareDeal :: TestDeal -> TestDeal 
prepareDeal t = t {bonds = Map.map L.consolStmt (bonds t)}


buildRateCurves :: [RateAssumption]-> [AP.AssumptionBuilder] -> [RateAssumption] 
buildRateCurves rs (assump:assumps) = 
    case assump of 
      AP.InterestRateConstant i f -> 
        buildRateCurves ((RateFlat i f):rs) assumps
      AP.InterestRateCurve i ds ->  -- Index [(T.Day, Float)]
        buildRateCurves ((RateCurve i (dsToTs ds)):rs) assumps
      _ -> buildRateCurves rs assumps    
    where  
        -- [(T.Day, Float)]     FloatCurve [TsPoint T.Day Float]
        dsToTs ds = FloatCurve $ map (\(d,f) -> (TsPoint d f) ) ds
buildRateCurves rs [] = rs


getInits :: TestDeal -> Maybe [AP.AssumptionBuilder] 
         -> ([ActionOnDate], CF.CashFlowFrame, [RateAssumption])
getInits t assumps =
  ( sort $ bPayDates ++ pCollectionDatesA
    ,pCollectionCf, rateCurves)
  where
    startDate = Map.findWithDefault (T.fromGregorian 1970 1 1) "closing-date" (dates t)
    firstPayDate = Map.findWithDefault (T.fromGregorian 1970 1 1) "first-pay-date" (dates t)

    pCollectionInt = (collectPeriod t)
    bPayInt = (payPeriod t)

    projNum = 512
    bPayDates = map (\x -> RunWaterfall (afterNPeriod firstPayDate x bPayInt)) [0..projNum]
    pCollectionDates = map (\x -> (afterNPeriod startDate x pCollectionInt)) [0..projNum]
    pCollectionDatesA = map (\x -> CollectPoolIncome x) pCollectionDates

    poolCf = P.aggPool $ P.runPool ( P.assets (pool t) ) assumps
    pCollectionCf = CF.CashFlowFrame $ CF.aggTsByDates (CF.getTsCashFlowFrame poolCf) pCollectionDates

    rateCurves = buildRateCurves [] (fromMaybe [] assumps) -- [RateCurve LIBOR6M (FloatCurve [(TsPoint (T.fromGregorian 2022 1 1) 0.01)])]


queryDeal :: TestDeal -> DealStats ->  Float
queryDeal t s =
  case s of
    CurrentBondBalance ->
       Map.foldr (\x acc -> ((L.bndBalance x) + acc)) 0.0 (bonds t)
    OriginalBondBalance ->
       Map.foldr (\x acc -> (L.originBalance (L.bndOriginInfo x)) + acc) 0.0 (bonds t)
    CurrentPoolBalance ->
       foldl (\acc x -> (acc + (P.getCurrentBal x))) 0.0 (P.assets (pool t))
    OriginalPoolBalance ->
       foldl (\acc x -> (acc + (P.getOriginBal x))) 0.0 (P.assets (pool t))


calcDueFee :: TestDeal -> T.Day -> F.Fee -> F.Fee
calcDueFee t calcDay f@(F.Fee fn (F.FixFee amt)  fs fd fa _ _)
  = f{ F.feeDue = amt}
calcDueFee t calcDay f@(F.Fee fn (F.AnnualRateFee feeBase r) fs fd fa maybeFlpd _)
  = case maybeFlpd of
      (Just flpd ) ->
        f{ F.feeDue = fd + baseBal * r * (periodToYear flpd calcDay ACT_360)}
      Nothing ->
        f{ F.feeDue = fd + baseBal * r * (periodToYear tClosingDate calcDay ACT_360)}
    where
      baseBal = queryDeal t feeBase
      tClosingDate = Map.findWithDefault (T.fromGregorian 1970 1 1) "closing-date" (dates t)


calcDueInt :: TestDeal -> T.Day -> L.Bond -> L.Bond
calcDueInt t calc_date b@(L.Bond bn bt  bo bi bond_bal bond_rate _ _ lstIntPay _ _) =
  b {L.bndDueInt = (dueInt+int_arrears) }
  where
    int_arrears = 0
    lastIntPayDay = case lstIntPay of
                      Just pd -> pd
                      Nothing -> Map.findWithDefault (T.fromGregorian 1970 1 1) "closing-date" (dates t)
    dueInt = calcInt bond_bal lastIntPayDay calc_date bond_rate ACT_365


calcDuePrin :: TestDeal -> T.Day -> L.Bond -> L.Bond
calcDuePrin t calc_date b@(L.Bond bn bt bo bi bond_bal _ prin_arr int_arrears _ _ _) =
  b {L.bndDuePrin = duePrin}
  where
    duePrin = bond_bal

calcTargetAmount :: TestDeal -> A.Account -> Float
calcTargetAmount t (A.Account _ n i (Just r) _ ) =
   eval r
   where
     eval ra = case ra of
       A.PctReserve ds _rate -> (queryDeal t ds) * _rate
       A.FixReserve amt -> amt
       A.Max ra1 ra2 -> max (eval ra1) (eval ra2)

depositPoolInflow :: [W.CollectionRule] -> T.Day -> CF.CashFlowFrame -> Map.Map String A.Account -> Map.Map String A.Account
depositPoolInflow rules d cf amap =
  foldl fn amap rules
  where
      currentPoolInflow = CF.getSingleTsCashFlowFrame cf d
      fn _acc _r@(W.Collect _ _accName) =
          Map.adjust (A.deposit collectedCash d "Deposit CF from Pool") _accName _acc
          where 
              collectedCash = collectCash _r currentPoolInflow
      collectCash r ts =
        case  r of
          (W.Collect W.CollectedInterest _)   -> CF.mflowInterest ts
          (W.Collect W.CollectedPrincipal _)  -> CF.mflowPrincipal ts
          (W.Collect W.CollectedRecoveries _) -> CF.mflowRecovery ts
          (W.Collect W.CollectedPrepayment _) -> CF.mflowPrepayment ts

$(deriveJSON defaultOptions ''ExpectReturn)
