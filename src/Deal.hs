{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Deal (TestDeal(..),run2,getInits,runDeal,ExpectReturn(..)
            ,calcDueFee,applicableAdjust,performAction,queryDeal) where

import qualified Accounts as A
import qualified Asset as P
import qualified Expense as F
import qualified Liability as L
import qualified Waterfall as W
import qualified Cashflow as CF
import qualified Assumptions as AP
import qualified Call as C
import Lib

import qualified Data.Map as Map
import qualified Data.Time as T
import qualified Data.Set as S
import qualified Control.Lens as LS
import Data.List
import Data.Maybe
import Data.Aeson hiding (json)
import qualified Data.Aeson.Encode.Pretty as Pretty
import Language.Haskell.TH
import Data.Aeson.TH
import Data.Aeson.Types

import Debug.Trace
debug = flip trace

_startDate = (T.fromGregorian 1970 1 1)

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
  ,waterfall :: Map.Map W.ActionWhen W.DistributionSeq
  ,collects :: [W.CollectionRule]
  ,call :: Maybe [C.CallOption]
} deriving (Show)

$(deriveJSON defaultOptions ''TestDeal)

performAction :: T.Day -> TestDeal -> W.Action -> TestDeal
performAction d t (W.Transfer an1 an2 tags) =
  t {accounts = accMapAfterDeposit}
  where
    accMap = (accounts t)
    sourceAcc = Map.lookup an1 accMap
    transferAmt = case sourceAcc of
                    Just acc -> (A.accBalance acc)
                    Nothing -> 0
    _tags = case tags of
              Just x -> x
              Nothing -> ""
    accMapAfterDraw = Map.adjust (A.draw transferAmt d ("To:"++an2++"|"++_tags)) an1 accMap
    accMapAfterDeposit = Map.adjust (A.deposit transferAmt d ("From:"++an1++"|"++_tags)) an2 accMapAfterDraw

performAction d t (W.TransferBy an1 an2 formula) =
  t {accounts = accMapAfterDeposit}
  where
    accMap = (accounts t)
    sourceAcc = accMap Map.! an1
    targetAcc = accMap Map.! an2 -- `debug` ("Target>>"++an2)

    formulaAmount =
      case formula of
        W.ABCD -> (queryDeal t (CumulativeDefaultBalance d))
                  + (queryStmtAmt (A.accStmt targetAcc) ("SupportPay:"++an1))
                  - (queryStmtAmt (A.accStmt sourceAcc) ("To:"++an2++"\\|ABCD"))
        _ -> -1

    transferAmt = min formulaAmount (A.accBalance sourceAcc) -- `debug` ("already transfer amt"++show(queryStmtAmt (A.accStmt sourceAcc) ("To:"++an2++"|ABCD") ))

    accMapAfterDraw = Map.adjust (A.draw transferAmt d ("To:"++an2++"|"++show(formula))) an1 accMap
    accMapAfterDeposit = Map.adjust (A.deposit transferAmt d ("From:"++an1++"|"++show(formula))) an2 accMapAfterDraw

performAction d t (W.TransferReserve meetAcc sa ta tags) =
    t {accounts = accMapAfterTransfer }
  where
    accMap = (accounts t)
    sourceAcc = accMap Map.! sa
    targetAcc = accMap Map.! ta
    sourceAccBal = (A.accBalance sourceAcc)
    targetAccBal = (A.accBalance targetAcc) 
    transferAmt = 
        case meetAcc of 
             W.TillSource -> 
                 let 
                     sourceTarBal = calcTargetAmount t sourceAcc
                 in 
                     max (sourceAccBal - sourceTarBal ) 0
             W.TillTarget ->
                 let 
                   targetBal = calcTargetAmount t targetAcc
                   transferAmtTarget = max (targetBal - targetAccBal) 0
                 in 
                     min transferAmtTarget sourceAccBal

    accMapAfterTransfer
      = case transferAmt of
          0 -> accMap
          amt ->  Map.adjust (A.draw amt d "withdraw") sa  $ Map.adjust (A.deposit amt d "transfer") ta $ accMap

performAction d t (W.PayFee ans fns) =
  t {accounts = accMapUpdated, fees = feeMapUpdated}
  where
    feeMap = (fees t)
    accSet = S.fromList ans
    accMap = Map.filterWithKey (\k _ -> (S.member k accSet)) (accounts t)

    feesToPay = map (\x -> feeMap Map.! x ) fns
    feesWithDue = map (\x -> calcDueFee t d x) feesToPay
    feeDueAmts = map (\x -> (F.feeDue x) ) feesWithDue

    accNList = Map.toList accMap
    availBalLst = [ (n,(A.accBalance x)) | (n,x) <- accNList ]
    availAccBals = map snd availBalLst
    availAccNames = map fst availBalLst
    accList = map (\x -> accMap Map.! x) ans

    availBal = sum availAccBals

    actualPaidOut = min availBal $ sum feeDueAmts
    feesAmountToBePaid = zip feesWithDue  $ prorataFactors feeDueAmts availBal
    feesPaid = map (\(f,amt) -> (F.payFee d amt f)) feesAmountToBePaid

    feeMapUpdated = Map.union (Map.fromList $ zip fns feesPaid) feeMap

    accsAfterPay = A.supportPay accList d actualPaidOut ("Pay Fee",("SupportPay:"++(head ans)))
    accMapUpdated = Map.union (Map.fromList (zip ans accsAfterPay)) (accounts t)


performAction d t (W.PayFeeBy limit ans fns) =
  t {accounts = accMapUpdated, fees = feeMapUpdated}
  where
    feeMap = (fees t)
    accSet = S.fromList ans
    accMap = Map.filterWithKey (\k _ -> (S.member k accSet)) (accounts t)

    feesToPay = map (\x -> feeMap Map.! x ) fns
    feesWithDue = map (\x -> calcDueFee t d x) feesToPay
    feeDueAmts = case limit of
                   (W.DuePct pct) -> map (\x -> (F.feeDue x) * pct ) feesWithDue
                   (W.DueCapAmt amt) -> map (\x -> (min (F.feeDue x) amt)) feesWithDue

    accNList = Map.toList accMap
    availBalLst = [ (n,(A.accBalance x)) | (n,x) <- accNList]
    availAccBals = map snd availBalLst
    availAccNames = map fst availBalLst
    accList = map (\x -> accMap Map.! x) ans

    availBal = sum availAccBals

    actualPaidOut = min availBal $ sum feeDueAmts
    feesAmountToBePaid = zip feesWithDue  $ prorataFactors feeDueAmts availBal
    feesPaid = map (\(f,amt) -> (F.payFee d amt f)) feesAmountToBePaid

    feeMapUpdated = Map.union (Map.fromList $ zip fns feesPaid) feeMap

    accsAfterPay = A.supportPay accList d actualPaidOut ("Pay Fee",("SupportPay:"++(head ans)))
    accMapUpdated = Map.union (Map.fromList (zip ans accsAfterPay)) (accounts t)

performAction d t (W.PayInt an bnds) =
  t {accounts = accMapAfterPay, bonds = bndMapUpdated}
  where
    bndMap = (bonds t)
    accMap = (accounts t)
    acc = accMap Map.! an

    availBal = A.accBalance acc
    bndsToPay = map (\x -> bndMap Map.! x ) bnds

    bndsWithDue = filter (\x -> ((L.bndDueInt x) > 0)) $ map (\x -> calcDueInt t d x) bndsToPay
    bndsDueAmts = map (\x -> (L.bndDueInt x) ) bndsWithDue
    bndsNames = map L.bndName bndsWithDue

    actualPaidOut = min availBal $ foldl (+) 0 bndsDueAmts
    bndsAmountToBePaid = zip bndsWithDue  $ prorataFactors bndsDueAmts availBal

    bndsPaid = map (\(l,amt) -> (L.payInt d amt l)) bndsAmountToBePaid

    bndMapUpdated =   Map.union (Map.fromList $ zip bndsNames bndsPaid) bndMap
    accMapAfterPay = Map.adjust (A.draw actualPaidOut d "Pay Int") an accMap

performAction d t (W.PayTillYield an bnds) =
    performAction d t (W.PayInt an bnds)

performAction d t (W.PayResidual Nothing an bndName) =
  t {accounts = accMapAfterPay, bonds = bndMapAfterPay}
  where
    bndMap = bonds t
    accMap = accounts t

    availBal = A.accBalance $ accMap Map.! an

    accMapAfterPay = Map.adjust (A.draw availBal d "Pay Int") an accMap
    bndMapAfterPay = Map.adjust (L.payInt d availBal) bndName bndMap

performAction d t (W.PayFeeResidual limit an feeName) =
  t {accounts = accMapAfterPay, fees = feeMapAfterPay}
  where
    feeMap = fees t
    accMap = accounts t

    availBal = A.accBalance $ accMap Map.! an
    paidOutAmt = case limit of
                   Just (W.DuePct pct) ->  pct * availBal
                   Just (W.DueCapAmt cap) ->  min cap availBal
                   Nothing -> availBal


    accMapAfterPay = Map.adjust (A.draw paidOutAmt d "Pay Fee") an accMap
    feeMapAfterPay = Map.adjust (F.payFee d paidOutAmt) feeName feeMap

performAction d t (W.PayPrinBy (W.RemainBalPct q pct) an bndName) =
  t {accounts = accMapAfterPay, bonds = bndMapAfterPay}
  where
    bndMap = (bonds t)
    accMap = (accounts t)

    availBal = A.accBalance $ accMap Map.! an
    targetBnd = bndMap Map.! bndName
    targetBndBal = L.bndBalance targetBnd

    otherBndBal = (queryDeal t CurrentBondBalance) - targetBndBal

    dueAmount = (1/(1-pct)) * ( targetBndBal * (1-pct) - (pct * otherBndBal))
    actAmount = min availBal $ max dueAmount 0

    accMapAfterPay = Map.adjust
                        (A.draw actAmount d
                                ("Pay Prin:"++show(q)++":"++show(pct)))
                        an
                        accMap
    bndMapAfterPay = Map.adjust (L.payPrin d actAmount) bndName bndMap

performAction d t (W.PayPrin an bnds) =
  t {accounts = accMapAfterPay, bonds = bndMapUpdated} -- `debug` ("Bond Prin Pay Result"++show(bndMapUpdated))
  where
    bndMap = (bonds t)
    accMap = (accounts t)
    acc = accMap Map.! an

    bndsToPay = filter (\x -> ((L.bndBalance x) > 0)) $ map (\x -> bndMap Map.! x ) bnds
    availBal = A.accBalance acc
    -- TODO  add filter lockout bonds here
    bndsWithDue = map (\x -> calcDuePrin t d x) bndsToPay  --
    bndsDueAmts = map (\x -> (L.bndDuePrin x) ) bndsWithDue

    actualPaidOut = min availBal $ foldl (+) 0 bndsDueAmts -- `debug` ("bonds totoal due ->"++show(bndsDueAmts))
    bndsAmountToBePaid = zip bndsWithDue (prorataFactors bndsDueAmts availBal)
    bndsPaid = map (\(l,amt) -> (L.payPrin d amt l)) bndsAmountToBePaid --  `debug` ("pay prin->>>To"++show(bnds))

    bndMapUpdated =  Map.union (Map.fromList $ zip bnds bndsPaid) bndMap
    accMapAfterPay = Map.adjust (A.draw actualPaidOut d ("Pay Prin:"++show(bnds))) an accMap

performAction d t (W.LiquidatePool lm an) =
  t {accounts = accMapAfterLiq } -- TODO need to remove assets
  where
    liqAmt = calcLiquidationAmount lm (pool t) d
    accMap = accounts t
    accMapAfterLiq = Map.adjust (A.deposit liqAmt d ("Liquidation Proceeds:"++show lm)) an accMap

data ActionOnDate = CollectPoolIncome T.Day
                   |RunWaterfall T.Day String
                   |PoolCollection T.Day String
                   deriving (Show)

instance Ord ActionOnDate where
  compare (CollectPoolIncome d1) (CollectPoolIncome d2) = compare d1 d2
  compare (PoolCollection d1 _) (PoolCollection d2 _) = compare d1 d2
  compare (RunWaterfall d1 _) (RunWaterfall d2 _) = compare d1 d2
  compare (CollectPoolIncome d1) (RunWaterfall d2 _) = compare d1 d2
  compare (PoolCollection d1 _) (RunWaterfall d2 _) = compare d1 d2
  compare (RunWaterfall d1 _) (CollectPoolIncome d2) = compare d1 d2
  compare (RunWaterfall d1 _) (PoolCollection d2 _) = compare d1 d2

instance Eq ActionOnDate where
  (CollectPoolIncome d1) == (CollectPoolIncome d2) = d1 == d2
  (PoolCollection d1 _) == (PoolCollection d2 _) = d1 == d2
  (RunWaterfall d1 _) == (RunWaterfall d2 _) = d1 == d2
  (CollectPoolIncome d1) == (RunWaterfall d2 _) = d1 == d2
  (PoolCollection d1 _) == (RunWaterfall d2 _) = d1 == d2
  (RunWaterfall d1 _) == (CollectPoolIncome d2) = d1 == d2
  (RunWaterfall d1 _) == (PoolCollection d2 _) = d1 == d2


setBondNewRate :: T.Day -> [RateAssumption] -> L.Bond -> L.Bond
setBondNewRate d ras b@(L.Bond _ _ _ ii _ _ _ _ _ _ _) 
  = b { L.bndRate = (applyFloatRate ii d ras) }

getRateAssumptionByIndex :: [RateAssumption] -> Index -> Maybe RateAssumption
getRateAssumptionByIndex ras idx
  = find
      (\x -> case x of
              (RateCurve _idx _ts) -> (_idx==idx)
              (RateFlat _idx _rval) -> (_idx==idx))
      ras


applyFloatRate :: L.InterestInfo -> T.Day -> [RateAssumption] -> Float 
applyFloatRate (L.Floater idx spd p f c) d ras
  = idx_rate + spd
    where 
      idx_rate = case ra of 
        Just (RateCurve _idx _ts) -> getValByDate _ts d 
        Just (RateFlat _idx _r) ->  _r
        Nothing -> 0.0
      ra = getRateAssumptionByIndex ras idx

applicableAdjust :: T.Day -> L.Bond -> Bool
applicableAdjust d (L.Bond _ _ oi (L.Floater _ _ rr _ _) _ _ _ _ _ _ _ )
  = case rr of 
      L.ByInterval p mStartDate ->
          let 
            _startDate =  fromMaybe (L.originDate oi) mStartDate
            diff = T.diffGregorianDurationClip _startDate d
          in
            0 == mod (T.cdMonths diff) (fromIntegral ( monthsOfPeriod p))
      L.MonthOfYear monthIndex ->
          let 
            (_,m,_) = T.toGregorian d
          in 
            m == monthIndex

applicableAdjust d (L.Bond _ _ oi (L.Fix _ ) _ _ _ _ _ _ _ ) = False
applicableAdjust d (L.Bond _ _ oi (L.InterestByYield _ ) _ _ _ _ _ _ _ ) = False

setBndsNextIntRate :: TestDeal -> T.Day -> Maybe [RateAssumption] -> TestDeal
setBndsNextIntRate t d (Just ras) = t {bonds = updatedBonds}
    where 
        floatBonds = filter (applicableAdjust d) $ Map.elems (bonds t)
        floatBondNames = map L.bndName floatBonds
        updatedBonds = foldr (Map.adjust (setBondNewRate d ras)) (bonds t) floatBondNames

setBndsNextIntRate t d Nothing = t 

testCall :: TestDeal -> T.Day -> C.CallOption -> Bool 
testCall t d opt = 
    case opt of 
       C.PoolBalance x -> (queryDeal t (FutureCurrentPoolBalance d)) < x
       C.BondBalance x -> (queryDeal t CurrentBondBalance) < x
       C.PoolFactor x ->  ((queryDeal t (FutureCurrentPoolBalance d)) / (queryDeal t FutureOriginalPoolBalance))  < x
       C.BondFactor x ->  (queryDeal t BondFactor) < x
       C.OnDate x -> ( x == d )
       C.AfterDate x -> d > x
       C.And xs -> all (testCall t d) xs
       C.Or xs -> any (testCall t d) xs


testCalls :: TestDeal -> T.Day -> [C.CallOption] -> Bool
testCalls t d [] = False
testCalls t d opts = any (\x -> testCall t d x) opts -- `debug` ("testing call options")

run2 :: TestDeal -> Maybe CF.CashFlowFrame -> Maybe [ActionOnDate]
    -> Maybe [RateAssumption] -> Maybe ([C.CallOption])-> TestDeal
run2 t _ (Just []) _ _   = (prepareDeal t)

run2 t poolFlow (Just (ad:ads)) rates calls
  |  (length ads) == 0 = (prepareDeal t)  -- `debug` ("Total Acc balance" ++ show(queryDeal t AllAccBalance))
  |  (isNothing poolFlow) && ((queryDeal t AllAccBalance) == 0) = (prepareDeal t) -- `debug` ("Total Acc balance" ++ show(queryDeal t AllAccBalance))
  | otherwise
  = case ad of
        PoolCollection d waterfallName ->
            case poolFlow of
              Just _poolFlow ->
                 (run2 dAfter (CF.removeTsCashFlowFrameByDate _poolFlow d) (Just ads) rates calls)
                 where
                    dAfter = foldl (performAction d) (t {accounts=accs}) waterfallToExe
                    waterfallToExe = Map.findWithDefault [] W.EndOfPoolCollection (waterfall t)  -- `debug` ("AD->"++show(ad)++"remain ads"++show(length ads))
                    accs = depositPoolInflow (collects t) d _poolFlow (accounts t)  -- `debug` ("Deposit->"++show(d))
              Nothing -> (run2 t poolFlow (Just ads) rates calls)

        RunWaterfall d waterfallName ->
          case calls of
            Just callOpts ->
                if (testCalls dAfterWaterfall d callOpts) then
                  prepareDeal $ foldl (performAction d) t cleanUpActions
                  -- prepareDeal $ liquidatePool lq d lqAcc t --  `debug` ("Called !"++show(d))
                else
                  (run2 dAfterRateSet poolFlow (Just ads) rates calls)
            Nothing ->
               (run2 dAfterRateSet poolFlow (Just ads) rates Nothing)
          where
               waterfallToExe = Map.findWithDefault [] W.DistributionDay (waterfall t)
               cleanUpActions = Map.findWithDefault [] W.CleanUp (waterfall t)
               dAfterWaterfall = (foldl (performAction d) t waterfallToExe)-- `debug` ("Total Acc balance" ++ show(queryDeal t AllAccBalance))
               dAfterRateSet = setBndsNextIntRate dAfterWaterfall d rates  -- `debug` ("After Rate Set")


run2 t Nothing Nothing Nothing Nothing
  = run2 t (Just pcf) (Just ads) Nothing Nothing
  where
    (ads,pcf,rcurves,clls,_) = getInits t Nothing

run2 t Nothing _ _ _ = (prepareDeal t) -- `debug` "End ????"


calcLiquidationAmount :: C.LiquidationMethod -> (P.Pool a) -> T.Day -> Float
calcLiquidationAmount alm pool d 
  = case alm of 
      C.BalanceFactor currentFactor defaultFactor ->
          case (P.futureCf pool) of 
            Nothing -> 0
            Just (_futureCf) ->
                let 
                  poolInflow = CF.getEarlierTsCashFlowFrame _futureCf d 
                  earlierTxns = CF.getTxnAsOf _futureCf d
                  currentDefaulBal = sum $ map (\x -> (CF.mflowDefault x) - (CF.mflowRecovery x) - (CF.mflowLoss x)) earlierTxns
                in 
                  case poolInflow of 
                    Nothing -> 0
                    Just (_ts) ->   -- TODO need to check if missing last row
                        ((CF.mflowBalance _ts) * currentFactor + currentDefaulBal * defaultFactor)

      C.PV discountRate recoveryPct ->
          case (P.futureCf pool) of
            Nothing -> 0 
            Just (_futureCf) ->
                let 
                  futureTxns = CF.getTxnAfter _futureCf d
                  earlierTxns = CF.getTxnAsOf _futureCf d
                  pvCf = sum $ map (\x -> pv2  discountRate  d (CF.tsDate x) (CF.tsTotalCash x)) futureTxns 
                  currentDefaulBal = sum $ map (\x -> (CF.mflowDefault x) - (CF.mflowRecovery x) - (CF.mflowLoss x)) earlierTxns
                in 
                  currentDefaulBal * recoveryPct + pvCf 

liquidatePool :: C.LiquidationMethod -> T.Day -> String -> TestDeal -> TestDeal
liquidatePool lq d accName t =
  t {accounts = Map.adjust updateFn accName accs} -- `debug` ("Accs->"++show(accs))
  where
     proceeds = calcLiquidationAmount lq (pool t) d
     updateFn = A.deposit proceeds d "Liquidation Proceeds"
     accs = (accounts t)

data ExpectReturn = DealStatus
                  | DealPoolFlow
                  | DealPoolFlowPricing
                  | DealTxns
                  | ExecutionSummary
                  deriving (Show)

data TxnComponent = Account String
                   | Expense String
                   | Bond String
                   | Pool String
                   deriving (Show)

instance Eq TxnComponent where 
    (Account s1) == (Account s2) = s1 ==  s2
    (Bond s1) == (Bond s2) = s1 ==  s2
    (Expense s1) == (Expense s2) = s1 == s2

instance Ord TxnComponent where 
    compare (Account s1) (Account s2) = compare s1 s2
    compare (Bond s1) (Bond s2) = compare s1 s2
    compare (Expense s1) (Expense s2) = compare s1 s2

pairTxn :: (Map.Map TxnComponent (Maybe Statement)) -> [(TxnComponent, Txn)]
pairTxn m = Map.foldrWithKey (\k v t ->  [ (k,txn) | txn <- (getTxns v)]++t) [] m 

extractExecutionTxns:: TestDeal ->  [(TxnComponent, Txn)]
extractExecutionTxns td  = 
      (pairTxn bndStmts)++(pairTxn accStmts) ++(pairTxn feeStmts)
  where 
      bndStmts = Map.mapKeys (\x -> Bond x) $ Map.mapWithKey (\k v -> (L.bndStmt v)) (bonds td)
      accStmts = Map.mapKeys (\x -> Account x) $ Map.mapWithKey (\k v -> (A.accStmt v)) (accounts td)
      feeStmts = Map.mapKeys (\x -> Expense x) $ Map.mapWithKey (\k v -> (F.feeStmt v)) (fees td)

priceBonds :: TestDeal -> AP.BondPricingInput -> Map.Map String L.PriceResult
priceBonds t (AP.DiscountCurve d dc) = Map.map (\b -> L.priceBond d dc b) (bonds t)

runDeal :: TestDeal -> ExpectReturn -> Maybe [AP.AssumptionBuilder] -> Maybe AP.BondPricingInput
        -> (TestDeal,Maybe CF.CashFlowFrame, Maybe [(TxnComponent, Txn)],Maybe (Map.Map String L.PriceResult))
runDeal t er assumps bpi =
  case er of
    DealStatus ->  (finalDeal, Nothing, Nothing, Nothing)
    DealPoolFlow -> (finalDeal, Just pcf, Nothing, Nothing)
    DealPoolFlowPricing -> (finalDeal, Just pcf, Nothing, bndPricing) -- `debug` ("with pricing"++show(bndPricing))
    DealTxns -> (finalDeal, Just pcf, Just (extractExecutionTxns(finalDeal)),Nothing)
  where
    bndPricing = case bpi of
                   Nothing -> Nothing    `debug` ("pricing bpi with Nothing")
                   Just _bpi -> Just (priceBonds finalDeal _bpi)   -- `debug` ("Pricing result")
    finalDeal = run2 t2 (Just pcf) (Just ads) (Just rcurves) calls  -- `debug` (">>ADS==>> "++show(ads))
    (ads,pcf,rcurves,calls,t2) = getInits t assumps

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
        dsToTs ds = FloatCurve $ map (\(d,f) -> (TsPoint d f) ) ds
buildRateCurves rs [] = rs

buildCallOptions :: Maybe [C.CallOption] -> [AP.AssumptionBuilder] -> Maybe [C.CallOption]
buildCallOptions rs (assump:assumps) =
    case assump of  
      AP.CallWhen opts -> buildCallOptions (Just opts) assumps --`debug` ("assump in build"++show(assumps))
      _ -> buildCallOptions rs assumps

buildCallOptions Nothing [] =  Nothing
buildCallOptions rs [] =  rs


setFutureCF :: TestDeal -> CF.CashFlowFrame -> TestDeal
setFutureCF t cf = 
    t {pool = newPool}
    where 
        _pool = (pool t)
        newPool = _pool {P.futureCf = (Just cf)}


getInits :: TestDeal -> Maybe [AP.AssumptionBuilder] -> 
    ([ActionOnDate], CF.CashFlowFrame, [RateAssumption],Maybe [C.CallOption]
      ,TestDeal)
getInits t (Just assumps) =
    (actionDates
    ,pCollectionCfAfterCutoff
    ,rateCurves
    ,callOptions  
    ,t_with_cf)   -- `debug` ("Rate Curve"++show(rateCurves))
  where
    startDate = Map.findWithDefault _startDate "cutoff-date" (dates t)
    firstPayDate = Map.findWithDefault _startDate "first-pay-date" (dates t)

    pCollectionInt = (collectPeriod t)
    bPayInt = (payPeriod t)


    projNum = 512
    bPayDates = map (\x -> RunWaterfall (afterNPeriod firstPayDate x bPayInt) "base") [0..projNum]
    pCollectionDates = map (\x -> (afterNPeriod startDate x pCollectionInt)) [0..projNum]
    pCollectionDatesA = map (\x -> PoolCollection x "collection") pCollectionDates

    stopDate = find (\x -> case x of    
                            (AP.StopRunBy d) -> True
                            _ -> False) assumps -- `debug` (">>Assumps"++show(assumps))

    _actionDates = sort $ bPayDates ++ pCollectionDatesA
    actionDates = case stopDate of
                    Just (AP.StopRunBy d) ->  filter (\x -> case x of
                                                  (RunWaterfall _d _) -> _d < d
                                                  -- CollectPoolIncome _d -> _d < d
                                                  (PoolCollection _d _) -> _d < d
                                                  ) _actionDates
                    Nothing ->  _actionDates  -- `debug` (">>stop date"++show(stopDate))

    poolCf = P.aggPool $ P.runPool2 (pool t)  assumps  -- `debug` ("Assets Agged pool Cf->"++show(pool t))
    poolCfTs = filter (\txn -> (CF.tsDate txn) > startDate)  $ CF.getTsCashFlowFrame poolCf
    pCollectionCfAfterCutoff = CF.CashFlowFrame $  CF.aggTsByDates poolCfTs pCollectionDates --  `debug` ("poolCf Dates"++show(pCollectionDates)) `debug` ("pool cf ts"++show(poolCfTs))
    t_with_cf  = setFutureCF t pCollectionCfAfterCutoff -- `debug` ("aggedCf:->>"++show(pCollectionCfAfterCutoff))
    rateCurves = buildRateCurves [] assumps   -- [RateCurve LIBOR6M (FloatCurve [(TsPoint (T.fromGregorian 2022 1 1) 0.01)])]
    callOptions = buildCallOptions Nothing assumps -- `debug` ("Assump"++show(assumps))


queryDeal :: TestDeal -> DealStats ->  Float
queryDeal t s =
  case s of
    CurrentBondBalance ->
       Map.foldr (\x acc -> ((L.bndBalance x) + acc)) 0.0 (bonds t)
    OriginalBondBalance ->
       Map.foldr (\x acc -> (L.originBalance (L.bndOriginInfo x)) + acc) 0.0 (bonds t)
    CurrentPoolBalance ->
       foldl (\acc x -> (acc + (P.getCurrentBal x))) 0.0 (P.assets (pool t))
    CurrentPoolDefaultedBalance ->
       foldl (\acc x -> (acc + (P.getCurrentBal x)))
             0.0 $
             filter (\a-> P.isDefaulted a ) (P.assets (pool t))
    OriginalPoolBalance ->
       foldl (\acc x -> (acc + (P.getOriginBal x))) 0.0 (P.assets (pool t))
    BondFactor -> 
        (queryDeal t CurrentBondBalance) / (queryDeal t OriginalBondBalance)
    PoolFactor -> 
        (queryDeal t CurrentPoolBalance) / (queryDeal t OriginalPoolBalance)

    AllAccBalance ->
        Map.foldr (\x acc -> (A.accBalance x)+acc) 0.0 (accounts t)

    FutureOriginalPoolBalance ->
      CF.mflowBalance $ head (CF.getTsCashFlowFrame _pool_cfs)
     where
      _pool_cfs = fromMaybe (CF.CashFlowFrame []) (P.futureCf (pool t))

    FutureCurrentPoolBalance asOfDay ->
         case _poolSnapshot of
            Just ts -> CF.mflowBalance ts
            Nothing -> 0
        where
         _pool_cfs = fromMaybe (CF.CashFlowFrame []) (P.futureCf (pool t))
         _poolSnapshot = CF.getEarlierTsCashFlowFrame _pool_cfs asOfDay

    FutureCurrentPoolFactor asOfDay ->
        (queryDeal t (FutureCurrentPoolBalance asOfDay)) / (queryDeal t FutureOriginalPoolBalance)

    CurrentPoolCollectionInt asOfDay ->
      case (P.futureCf (pool t)) of
        Nothing -> 0
        Just _futureCf ->
          case (CF.getTxnLatestAsOf _futureCf asOfDay) of
            Just flow -> CF.mflowInterest flow
            Nothing -> 0

    CumulativeDefaultBalance asOfDay ->
        let
          futureDefaults = case (P.futureCf (pool t)) of
                             Just futureCf ->  foldr (\r a -> (CF.tsDefaultBal r) + a)  0  $ CF.getTxnAsOf futureCf asOfDay -- `debug` (">>as of day"++show(asOfDay))
                             Nothing -> 0.0
          currentDefaults = queryDeal t CurrentPoolDefaultedBalance
        in
          futureDefaults + currentDefaults

    CurrentBondBalanceOf bns ->
       let
          bnSet = S.fromList bns
          bSubMap = Map.filterWithKey (\bn b -> (S.member bn bnSet)) (bonds t)
       in
          sum $ map (\x -> (L.bndBalance x) ) $ Map.elems bSubMap


calcDueFee :: TestDeal -> T.Day -> F.Fee -> F.Fee
calcDueFee t calcDay f@(F.Fee fn F.FixFee  fs fd (Just _fdDay) fa _ _)
  | _fdDay /= calcDay = f{ F.feeDue = fd , F.feeDueDate = Just calcDay}
  | otherwise = f
  
calcDueFee t calcDay f@(F.Fee fn F.FixFee fs fd Nothing fa _ _)
  = f{ F.feeDue = fd, F.feeDueDate = Just calcDay}

calcDueFee t calcDay f@(F.Fee fn (F.AnnualRateFee feeBase r) fs fd Nothing fa lpd _)
  = calcDueFee t calcDay f {F.feeDueDate = Just _startDate }

calcDueFee t calcDay f@(F.Fee fn (F.AnnualRateFee feeBase r) fs fd (Just _fdDay) fa lpd _)
  | _fdDay == calcDay = f
  | otherwise = f{ F.feeDue = fd + baseBal * r * (periodToYear feeStartDate calcDay ACT_360)
                            , F.feeDueDate = Just calcDay }
                 where
                     feeStartDate = case lpd of
                                        (Just _lpd) -> _lpd
                                        Nothing -> tClosingDate 
                     baseBal = queryDeal t feeBase
                     tClosingDate = Map.findWithDefault _startDate "closing-date" (dates t)

calcDueFee t calcDay f@(F.Fee fn (F.PctFee PoolCollectionInt r) fs fd _fdDay fa lpd _)
   = f{ F.feeDue = fd + baseBal * r , F.feeDueDate = Just calcDay }
     where
     baseBal = queryDeal t (CurrentPoolCollectionInt calcDay)

calcDueFee t calcDay f@(F.Fee fn (F.RecurFee p amt)  fs fd Nothing fa _ _)
  = f{ F.feeDue = amt * (fromIntegral (periodsBetween calcDay fs p)) , F.feeDueDate = Just calcDay } -- `debug` ("New fee"++show(f))

calcDueFee t calcDay f@(F.Fee fn (F.Custom ts)  fs fd Nothing fa mflpd _)
  = f{ F.feeDue = newFeeDue
      ,F.feeDueDate = Just calcDay
      ,F.feeType = (F.Custom futureDue)} -- `debug` ("New fee"++show(f))
    where
      newFeeDue =  cumulativeDue + fd
      (currentNewDue,futureDue) = splitTsByDate ts calcDay
      cumulativeDue = sumValTs currentNewDue

calcDueFee t calcDay f@(F.Fee fn (F.RecurFee p amt)  fs fd (Just _fdDay) fa _ _)
  | _fdDay == calcDay = f
  | periodGap == 0 = f
  | otherwise = f { F.feeDue = (fd+(amt*(fromIntegral periodGap))) , F.feeDueDate = Just calcDay } `debug` ("Gap->"++show(fromIntegral periodGap))
  where
  periodGap = periodsBetween calcDay _fdDay p

calcDueInt :: TestDeal -> T.Day -> L.Bond -> L.Bond
calcDueInt t calc_date b@(L.Bond bn L.Z bo bi bond_bal bond_rate _ _ lstIntPay _ _) 
  = b {L.bndDueInt = 0 }

calcDueInt t calc_date b@(L.Bond bn _ bo (L.InterestByYield y) bond_bal _ _ intDue lstIntPay _ mStmt)
  = b {L.bndDueInt = newDue }
  where
  newDue = L.backoutDueIntByYield calc_date b

calcDueInt t calc_date b@(L.Bond bn bt bo bi bond_bal bond_rate _ _ lstIntPay _ _) =
  b {L.bndDueInt = (dueInt+int_arrears) }
  where
    int_arrears = 0
    lastIntPayDay = case lstIntPay of
                      Just pd -> pd
                      Nothing -> Map.findWithDefault _startDate "closing-date" (dates t)
    dueInt = calcInt bond_bal lastIntPayDay calc_date bond_rate ACT_365


calcDuePrin :: TestDeal -> T.Day -> L.Bond -> L.Bond
calcDuePrin t calc_date b@(L.Bond bn L.Sequential bo bi bond_bal _ prin_arr int_arrears _ _ _) =
  b {L.bndDuePrin = duePrin} 
  where
    duePrin = bond_bal 

calcDuePrin t calc_date b@(L.Bond bn (L.Lockout cd) bo bi bond_bal _ prin_arr int_arrears _ _ _) =
  if (cd > calc_date)  then 
    b {L.bndDuePrin = 0}
  else
    b {L.bndDuePrin = duePrin}
  where
    duePrin = bond_bal 

calcDuePrin t calc_date b@(L.Bond bn (L.PAC schedule) bo bi bond_bal _ prin_arr int_arrears _ _ _) =
  b {L.bndDuePrin = duePrin} -- `debug` ("bn >> "++bn++"Due Prin set=>"++show(duePrin) )
  where
    scheduleDue = getValOnByDate schedule calc_date  
    duePrin = max (bond_bal - scheduleDue) 0 -- `debug` ("In PAC ,target balance"++show(schedule)++show(calc_date)++show(scheduleDue))

calcDuePrin t calc_date b@(L.Bond bn (L.PAC_Anchor schedule bns) bo bi bond_bal _ prin_arr int_arrears _ _ _) =
  b {L.bndDuePrin = duePrin} -- `debug` ("bn >> "++bn++"Due Prin set=>"++show(duePrin) )
  where
    scheduleDue = getValOnByDate schedule calc_date
    anchor_bond_balance = (queryDeal t (CurrentBondBalanceOf bns))
    duePrin = if (anchor_bond_balance > 0) then
                 max (bond_bal - scheduleDue) 0
              else
                 bond_bal

calcDuePrin t calc_date b@(L.Bond bn L.Z bo bi bond_bal bond_rate prin_arr int_arrears lstIntPay _ _) =
  if (all (\x -> (isZbond x)) activeBnds) then
      b {L.bndDuePrin = bond_bal} -- `debug` ("bn >> "++bn++"Due Prin set=>"++show(duePrin) )
  else 
      b {L.bndDuePrin = 0, L.bndBalance = new_bal, L.bndLastIntPay=Just calc_date} -- `debug` ("bn >> "++bn++"Due Prin set=>"++show(duePrin) )
  where
    isZbond (L.Bond _ bt _ _ _ _ _ _ _ _ _) 
      = case bt of
          L.Z -> True
          _ -> False
    activeBnds = filter (\x -> (L.bndBalance x) > 0) (Map.elems (bonds t))
    new_bal = bond_bal + dueInt
    lastIntPayDay = case lstIntPay of
                      Just pd -> pd
                      Nothing -> Map.findWithDefault _startDate "closing-date" (dates t)
    dueInt = calcInt bond_bal lastIntPayDay calc_date bond_rate ACT_365

calcDuePrin t calc_date b@(L.Bond bn L.Equity bo bi bond_bal _ prin_arr int_arrears _ _ _) =
  b {L.bndDuePrin = bond_bal }

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
  foldl fn amap rules  -- `debug` ("Depositing at "++show(d))
  where
      currentPoolInflow = CF.getSingleTsCashFlowFrame cf d

      fn _acc _r@(W.Collect _ _accName) =
          Map.adjust (A.deposit collectedCash d "Deposit CF from Pool") _accName _acc
          where 
              collectedCash = collectCash _r currentPoolInflow
      
      collectCash r ts =
        case r of
          (W.Collect W.CollectedInterest _)   -> CF.mflowInterest ts
          (W.Collect W.CollectedPrincipal _)  -> CF.mflowPrincipal ts
          (W.Collect W.CollectedRecoveries _) -> CF.mflowRecovery ts
          (W.Collect W.CollectedPrepayment _) -> CF.mflowPrepayment ts

$(deriveJSON defaultOptions ''ExpectReturn)
$(deriveJSON defaultOptions ''TxnComponent)

td = TestDeal {
  name = "test deal1"
  ,dates = (Map.fromList [("closing-date",(T.fromGregorian 2022 1 1))
                         ,("cutoff-date",(T.fromGregorian 2022 1 1))
                         ,("first-pay-date",(T.fromGregorian 2022 2 25))
                         ])
  ,payPeriod = Monthly
  ,collectPeriod = Monthly
  ,accounts = (Map.fromList
  [("General", (A.Account { A.accName="General" ,A.accBalance=0.0 ,A.accType=Nothing, A.accInterest=Nothing ,A.accStmt=Nothing
  })),
   ("Reserve", (A.Account { A.accName="General" ,A.accBalance=0.0 ,A.accType=Just (A.FixReserve 500), A.accInterest=Nothing ,A.accStmt=Nothing
  }))
  ])
  ,fees = (Map.fromList [("Service-Fee"
                         ,F.Fee{F.feeName="service-fee"
                                ,F.feeType = F.FixFee
                                ,F.feeStart = (T.fromGregorian 2022 1 1)
                                ,F.feeDue = 100
                                ,F.feeDueDate = Nothing
                                ,F.feeArrears = 0
                                ,F.feeLastPaidDay = Nothing
                                ,F.feeStmt = Nothing})])
  ,bonds = (Map.fromList [("A"
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
  ,pool = P.Pool {P.assets=[P.Mortgage
                                         P.OriginalInfo{
                                           P.originBalance=4000
                                           ,P.originRate=P.Fix 0.085
                                           ,P.originTerm=60
                                           ,P.period=Monthly
                                           ,P.startDate=(T.fromGregorian 2022 1 1)
                                           ,P.prinType= P.Level}
                                         4000
                                         0.085
                                         60
                                         P.Current]
                 ,P.futureCf=Nothing
                 ,P.asOfDate = T.fromGregorian 2022 1 1}
   ,waterfall = Map.fromList [(W.DistributionDay, [
                                 W.PayFee ["General"] ["Service-Fee"]
                                 ,W.PayFeeBy (W.DuePct 0.5) ["General"] ["Service-Fee"]
                                 ,W.TransferReserve W.TillSource  "General" "General" Nothing
                                 ,W.TransferReserve W.TillTarget  "General" "General" Nothing
                                 ,W.PayInt "General" ["A"]
                                 ,W.PayPrin "General" ["A"]])
                               ,(W.CleanUp, [W.LiquidatePool (C.BalanceFactor 1.0 0.2) "A"])]
 ,collects = [W.Collect W.CollectedInterest "General"
             ,W.Collect W.CollectedPrincipal "General"]
 ,call = Just [C.PoolFactor 0.08]
}


