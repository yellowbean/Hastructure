{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Deal (TestDeal(..),run2,getInits,runDeal,ExpectReturn(..)
            ,calcDueFee,applicableAdjust,performAction,queryDeal
            ,setFutureCF,calcTargetAmount) where

import qualified Accounts as A
import qualified Asset as P
import qualified Expense as F
import qualified Liability as L
import qualified CreditEnhancement as CE
import qualified Waterfall as W
import qualified Cashflow as CF
import qualified Assumptions as AP
import qualified Call as C
import Lib
import Util
import Types
import Stmt (TxnComment(..),Statement(..),Txn,getTxns,getTxnAmt,getTxnDate,getTxnComment
            ,queryTxnAmt)

import qualified Data.Map as Map
import qualified Data.Time as T
import qualified Data.Set as S
import qualified Control.Lens as LS
import Data.List
import Data.Fixed
import Data.Maybe
import Data.Time.Clock
import Data.Aeson hiding (json)
import qualified Data.Aeson.Encode.Pretty as Pretty
import Language.Haskell.TH
import Data.Aeson.TH
import Data.Aeson.Types

import Debug.Trace
debug = flip trace

_startDate = T.fromGregorian 1970 1 1
_farEnoughDate = T.fromGregorian 2080 1 1

class SPV a where
  getBondByName :: a -> Maybe [String] -> Map.Map String L.Bond
  getBondStmtByName :: a -> Maybe [String] -> Map.Map String (Maybe Statement)
  getFeeByName :: a -> Maybe [String] -> Map.Map String F.Fee
  getAccountByName :: a -> Maybe [String] -> Map.Map String A.Account
  

class DealDates a where 
  getClosingDate :: a -> Date
  getFirstPayDate :: a -> Date

data TestDeal = TestDeal {
  name :: String
  ,status :: DealStatus
  ,dates :: DateDesp
  ,accounts :: Map.Map String A.Account
  ,fees :: Map.Map String F.Fee
  ,bonds :: Map.Map String L.Bond
  ,pool ::  P.Pool P.Mortgage
  ,waterfall :: Map.Map W.ActionWhen W.DistributionSeq
  ,collects :: [W.CollectionRule]
  ,call :: Maybe [C.CallOption]
  ,liqProvider :: Maybe (Map.Map String CE.LiqFacility)
  ,triggers :: Maybe (Map.Map (WhenTrigger,Trigger) TriggerEffect)
  ,overrides :: Maybe [OverrideType]
} deriving (Show)

instance SPV TestDeal where
  getBondByName t bns
    = case bns of
         Nothing -> bonds t
         Just _bns -> Map.filterWithKey (\k _ ->  (S.member k (S.fromList _bns))) (bonds t)

  getBondStmtByName t bns
    = Map.map L.bndStmt bndsM
      where
      bndsM = Map.map L.consolStmt $ getBondByName t bns

  getFeeByName t fns
    = case fns of
         Nothing -> fees t
         Just _fns -> Map.filterWithKey (\k _ ->  (S.member k (S.fromList _fns))) (fees t)
  
  getAccountByName t ans
    = case ans of
         Nothing -> accounts t
         Just _ans -> Map.filterWithKey (\k _ ->  (S.member k (S.fromList _ans))) (accounts t)

instance DealDates DateDesp where 
  getClosingDate (PatternInterval _m)
    = let 
        (sd,dp,ed) = _m Map.! ClosingDate 
      in 
         sd
         
  getClosingDate (CustomDates _ pActions cd bActions )
    = cd

  getClosingDate (FixInterval _m _p1 _p2)  
    = _m Map.! ClosingDate

  getFirstPayDate (PatternInterval _m) 
    = let 
        (sd,dp,ed) = _m Map.! FirstPayDate
      in 
         sd
  
  getFirstPayDate (CustomDates _ pActions _ bActions )
    = actionDate $ head bActions
  
  getFirstPayDate (FixInterval _m _p1 _p2)  
    = _m Map.! FirstPayDate


testPre :: Date -> TestDeal ->  Pre -> Bool
testPre d t p =
  case p of
    Types.And p1 p2 -> (testPre d t p1) && (testPre d t p1)
    Or p1 p2 -> (testPre d t p1) || (testPre d t p1)
    IfZero s  -> (queryDeal t s) == 0.0 -- `debug` ("S->"++show(s)++">>"++show((queryDeal t s)))
    IfGT s  amt -> (queryDeal t s) > amt
    IfGET s  amt -> (queryDeal t s) >= amt
    IfLT s  amt -> (queryDeal t s) < amt
    IfLET s  amt -> (queryDeal t s) <= amt

performAction :: Date -> TestDeal -> (Maybe Pre, W.Action) -> TestDeal
performAction d t (Just _pre, _action)
  | testPre d t _pre = performAction d t (Nothing, _action)
  | otherwise  = t

performAction d t (Nothing, W.Transfer an1 an2) =
  t {accounts = accMapAfterDeposit}
  where
    accMap = accounts t
    sourceAcc = accMap Map.! an1
    transferAmt = max 0 (A.accBalance sourceAcc)
    comment = Transfer an1 an2
    accMapAfterDraw = Map.adjust (A.draw transferAmt d comment ) an1 accMap
    accMapAfterDeposit = Map.adjust (A.deposit transferAmt d  comment) an2 accMapAfterDraw

performAction d t (Nothing, W.TransferBy limit an1 an2) =
  t {accounts = accMapAfterDeposit}  -- `debug` ("ABCD "++show(d))
  where
    accMap = accounts t
    sourceAcc = accMap Map.! an1
    targetAcc = accMap Map.! an2 -- `debug` ("Target>>"++an2)

    formulaAmount = case limit of 
                      W.DuePct r -> r * A.accBalance sourceAcc
                      W.DueCapAmt a -> min a (A.accBalance sourceAcc)
                      W.DS ds -> queryDeal t ds
                      W.Formula W.ABCD -> max 
                                            ((queryDeal t (CumulativeDefaultBalance d)) + 
                                               (negate (queryTxnAmt targetAcc (Transfer an2 an1))) +
                                               (negate (queryTxnAmt sourceAcc (Transfer an1 an2))))
                                          0
                                        --`debug`
                                        --  ("AB"++show (queryDeal t (CumulativeDefaultBalance d))
                                        --   ++"C"++ show (negate (queryTxnAmt targetAcc (Transfer an2 an1)))
                                        --   ++"D"++ show (negate (queryTxnAmt sourceAcc (Transfer an1 an2))
                                        --  ))

    transferAmt = min formulaAmount (A.accBalance sourceAcc) -- `debug` ("already transfer amt"++show(queryStmtAmt (A.accStmt sourceAcc) ("To:"++an2++"|ABCD") ))

    accMapAfterDraw = Map.adjust (A.draw transferAmt d (Transfer an1 an2)) an1 accMap
    accMapAfterDeposit = Map.adjust (A.deposit transferAmt d (Transfer an1 an2)) an2 accMapAfterDraw

performAction d t (Nothing, (W.TransferReserve meetAcc sa ta tags) )=
    t {accounts = accMapAfterTransfer }
  where
    accMap = accounts t
    sourceAcc = accMap Map.! sa
    targetAcc = accMap Map.! ta
    sourceAccBal = A.accBalance sourceAcc
    targetAccBal = A.accBalance targetAcc 
    transferAmt = 
        case meetAcc of 
             W.Source ->
                 let 
                   sourceTarBal = calcTargetAmount t d sourceAcc
                 in 
                   max (sourceAccBal - sourceTarBal ) 0
             W.Target ->
                 let 
                   targetBal = calcTargetAmount t d targetAcc
                   transferAmtTarget = max (targetBal - targetAccBal) 0 -- `debug` ("Target balance ->> "++show(targetBal))
                 in 
                   min transferAmtTarget sourceAccBal

    accMapAfterTransfer
      = case transferAmt of
          0 -> accMap
          amt ->  Map.adjust (A.draw amt d (Transfer sa ta)) sa  $ Map.adjust (A.deposit amt d (Transfer sa ta)) ta $ accMap

performAction d t (Nothing, W.PayFee ans fns) =
  t {accounts = accMapUpdated, fees = feeMapUpdated}
  where
    feeMap = fees t
    accSet = S.fromList ans
    accMap = Map.filterWithKey (\k _ -> S.member k accSet) (accounts t)

    feesToPay = map (feeMap Map.!) fns
    -- feesWithDue = map (calcDueFee t d) feesToPay  -- `debug` ("Show Fee"++show(feesToPay))
    feeDueAmts = map F.feeDue feesToPay  

    accNList = Map.toList accMap -- `debug` ("Show Fee with Due "++show(feeDueAmts))
    availBalLst = [ (n,A.accBalance x) | (n,x) <- accNList ]
    availAccBals = map snd availBalLst
    availAccNames = map fst availBalLst
    accList = map (accMap Map.!) ans

    availBal = sum availAccBals

    actualPaidOut = min availBal $ sum feeDueAmts -- `debug` ("Fee Due Amounts"++show(feeDueAmts))
    feesAmountToBePaid = zip feesToPay  $ prorataFactors feeDueAmts availBal
    feesPaid = map (\(f,amt) -> F.payFee d amt f) feesAmountToBePaid

    feeMapUpdated = Map.union (Map.fromList $ zip fns feesPaid) feeMap

    accsAfterPay = A.supportPay accList d actualPaidOut (SeqPayFee fns ,SeqPayFee fns)  
    accMapUpdated = Map.union (Map.fromList (zip ans accsAfterPay)) (accounts t)


performAction d t (Nothing, W.PayFeeBy limit ans fns) =
  t {accounts = accMapUpdated, fees = feeMapUpdated}
  where
    feeMap = fees t
    accSet = S.fromList ans
    accMap = Map.filterWithKey (\k _ -> S.member k accSet) (accounts t)

    feesToPay = map (feeMap Map.!) fns
    -- feesWithDue = map (calcDueFee t d) feesToPay
    feeDueAmts = case limit of
                  (W.DuePct pct) -> map (\x -> pct * (F.feeDue x) ) feesToPay
                  (W.DueCapAmt amt) -> map (\x -> (min (F.feeDue x) amt)) feesToPay

    accNList = Map.toList accMap
    availBalLst = [ (n,A.accBalance x) | (n,x) <- accNList]
    availAccBals = map snd availBalLst
    availAccNames = map fst availBalLst
    accList = map (accMap Map.!) ans

    availBal = sum availAccBals

    actualPaidOut = min availBal $ sum feeDueAmts
    feesAmountToBePaid = zip feesToPay  $ prorataFactors feeDueAmts availBal
    feesPaid = map (\(f,amt) -> F.payFee d amt f) feesAmountToBePaid

    feeMapUpdated = Map.union (Map.fromList $ zip fns feesPaid) feeMap

    accsAfterPay = A.supportPay accList d actualPaidOut (SeqPayFee fns,SeqPayFee fns)  
    accMapUpdated = Map.union (Map.fromList (zip ans accsAfterPay)) (accounts t)

performAction d t (Nothing, W.PayInt an bnds) =
  t {accounts = accMapAfterPay, bonds = bndMapUpdated}
  where
    bndMap = bonds t
    accMap = accounts t
    acc = accMap Map.! an

    availBal = A.accBalance acc
    bndsToPay = map (bndMap Map.!) bnds
    bndsWithDue = filter (\x -> L.bndDueInt x > 0) $ map (calcDueInt t d) bndsToPay

    bndsDueAmts = map L.bndDueInt bndsWithDue
    bndsNames = map L.bndName bndsWithDue

    actualPaidOut = min availBal $ sum bndsDueAmts -- `debug` ("due mats"++ show bndsDueAmts ++">>"++ show availBal)
    bndsAmountToBePaid = zip bndsWithDue $ prorataFactors bndsDueAmts availBal -- `debug` ("prorata"++ show (prorataFactors bndsDueAmts availBal) )

    bndsPaid = map (\(l,amt) -> L.payInt d amt l) bndsAmountToBePaid

    bndMapUpdated =   Map.union (Map.fromList $ zip bndsNames bndsPaid) bndMap
    comment = PayInt bnds Nothing
    accMapAfterPay = Map.adjust 
                       (A.draw actualPaidOut d comment)
                       an
                       accMap

performAction d t (Nothing, W.PayTillYield an bnds) =
    performAction d t (Nothing, W.PayInt an bnds)

performAction d t (Nothing, W.PayResidual Nothing an bndName) =
  t {accounts = accMapAfterPay, bonds = bndMapAfterPay}
  where
    bndMap = bonds t
    accMap = accounts t

    availBal = A.accBalance $ accMap Map.! an

    accMapAfterPay = Map.adjust (A.draw availBal d (PayYield bndName)) an accMap
    bndMapAfterPay = Map.adjust (L.payInt d availBal) bndName bndMap

performAction d t (Nothing, W.PayFeeResidual limit an feeName) =
  t {accounts = accMapAfterPay, fees = feeMapAfterPay}
  where
    feeMap = fees t
    accMap = accounts t

    availBal = A.accBalance $ accMap Map.! an
    paidOutAmt = case limit of
                   Just (W.DuePct pct) ->  pct * availBal
                   Just (W.DueCapAmt cap) ->  min cap availBal
                   Nothing -> availBal


    accMapAfterPay = Map.adjust (A.draw paidOutAmt d (PayFeeYield feeName)) an accMap
    feeMapAfterPay = Map.adjust (F.payFee d paidOutAmt) feeName feeMap

-- ^ pay bond till its balance as pct of total balance
performAction d t (Nothing, W.PayPrinBy (W.RemainBalPct pct) an bndName)=  --Need to replace with formula
  t {accounts = accMapAfterPay, bonds = bndMapAfterPay}
  where
    bndMap = bonds t
    accMap = accounts t

    availBal = A.accBalance $ accMap Map.! an
    targetBnd = bndMap Map.! bndName
    targetBndBal = L.bndBalance targetBnd

    otherBndBal = (queryDeal t CurrentBondBalance) - targetBndBal

    _pct = fromRational pct
    dueAmount = (1/(1-_pct)) * (targetBndBal * (1-_pct) - (_pct * otherBndBal))
    actAmount = min availBal $ max dueAmount 0

    accMapAfterPay = Map.adjust
                        (A.draw actAmount d Empty) --TODO
                        an
                        accMap
    bndMapAfterPay = Map.adjust (L.payPrin d actAmount) bndName bndMap

performAction d t (Nothing, W.PayPrin an bnds) =
  t {accounts = accMapAfterPay, bonds = bndMapUpdated} -- `debug` ("Bond Prin Pay Result"++show(bndMapUpdated))
  where
    bndMap = bonds t
    accMap = accounts t
    acc = accMap Map.! an

    bndsToPay = filter (\x -> ((L.bndBalance x) > 0)) $ map (\x -> bndMap Map.! x ) bnds
    availBal = A.accBalance acc
    -- TODO  add filter lockout bonds here
    bndsWithDue = map (calcDuePrin t d) bndsToPay  --
    bndsDueAmts = map L.bndDuePrin bndsWithDue

    actualPaidOut = min availBal $ sum bndsDueAmts -- `debug` ("bonds totoal due ->"++show(bndsDueAmts))
    bndsAmountToBePaid = zip bndsWithDue (prorataFactors bndsDueAmts availBal)
    bndsPaid = map (\(l,amt) -> L.payPrin d amt l) bndsAmountToBePaid --  `debug` ("pay prin->>>To"++show(bnds))

    bndMapUpdated =  Map.union (Map.fromList $ zip bnds bndsPaid) bndMap
    accMapAfterPay = Map.adjust (A.draw actualPaidOut d (PayPrin bnds Nothing)) an accMap

performAction d t (Nothing, W.PayPrinResidual an bnds) = 
  t {accounts = accMapAfterPay, bonds = bndMapUpdated} -- `debug` ("Bond Prin Pay Result"++show(bndMapUpdated))
  where
    bndMap = bonds t
    accMap = accounts t
    acc = accMap Map.! an

    bndsToPay = filter (\x -> ((L.bndBalance x) > 0)) $ map (\x -> bndMap Map.! x ) bnds
    availBal = A.accBalance acc
    -- TODO  add filter lockout bonds here
    bndsDueAmts = map L.bndBalance bndsToPay

    actualPaidOut = min availBal $ sum  bndsDueAmts -- `debug` ("bonds totoal due ->"++show(bndsDueAmts))
    bndsAmountToBePaid = zip bndsToPay (prorataFactors bndsDueAmts availBal)
    bndsPaid = map (\(l,amt) -> L.payPrin d amt l) bndsAmountToBePaid --  `debug` ("pay prin->>>To"++show(bnds))

    bndMapUpdated =  Map.union (Map.fromList $ zip bnds bndsPaid) bndMap
    accMapAfterPay = Map.adjust (A.draw actualPaidOut d (PayPrin bnds Nothing)) an accMap

performAction d t (Nothing, W.LiquidatePool lm an) =
  t {accounts = accMapAfterLiq } -- TODO need to remove assets
  where
    liqAmt = calcLiquidationAmount lm (pool t) d
    accMap = accounts t
    accMapAfterLiq = Map.adjust (A.deposit liqAmt d (LiquidationProceeds liqAmt)) an accMap

performAction d t (Nothing, W.CalcFee fns) 
  = t {fees = newFeeMap }
  where 
    fset = S.fromList fns
    newFeeMap = Map.mapWithKey
                  (\fn _f ->
                    if S.member fn fset then 
                      calcDueFee t d _f 
                    else
                      _f)
                  (fees t)

performAction d t (Nothing, W.CalcBondInt bns) 
  = t {bonds = newBondMap }
  where 
    bset = S.fromList bns
    newBondMap = Map.mapWithKey
                  (\bn _b ->
                    if S.member bn bset then 
                      calcDueInt t d _b 
                    else
                      _b)
                  (bonds t)

performAction d t@TestDeal{accounts=accs,liqProvider = Just _liqProvider} (Nothing, W.LiqSupport limit pName an)
  = t { accounts = newAccMap, liqProvider = Just newLiqMap }
  where 
      _transferAmt = case limit of 
                      Nothing -> 0 
                      Just (W.DS (ReserveAccGap [an]))
                        -> queryDeal t (ReserveAccGapAt d [an])
                      _ -> 0
      transferAmt = min _transferAmt $ CE.liqBalance $  _liqProvider Map.! pName
      accMap = (accounts t)
      newAccMap = Map.adjust (A.deposit transferAmt d (LiquidationSupport pName)) an accMap
      newLiqMap = Map.adjust (CE.draw transferAmt d ) pName _liqProvider 


setBondNewRate :: T.Day -> [RateAssumption] -> L.Bond -> L.Bond
setBondNewRate d ras b@(L.Bond _ _ _ ii _ _ _ _ _ _ _ _) 
  = b { L.bndRate = applyFloatRate ii d ras }

getRateAssumptionByIndex :: [RateAssumption] -> Index -> Maybe RateAssumption
getRateAssumptionByIndex ras idx
  = find
      (\case
        (RateCurve _idx _ts) -> (_idx==idx)
        (RateFlat _idx _rval) -> (_idx==idx))
      ras


applyFloatRate :: L.InterestInfo -> T.Day -> [RateAssumption] -> IRate
applyFloatRate (L.Floater idx spd p dc mf mc) d ras
  = case (mf,mc) of
      (Nothing,Nothing) -> _rate
      (Just f,Nothing) -> max f _rate
      (Just f,Just c) -> min c $ max f _rate
      (Nothing,Just c) -> min c _rate
    where
      idx_rate = case ra of 
        Just (RateCurve _idx _ts) -> fromRational $ getValByDate _ts d
        Just (RateFlat _idx _r) ->   _r
        Nothing -> 0.0
      ra = getRateAssumptionByIndex ras idx
      _rate = idx_rate + spd

applicableAdjust :: T.Day -> L.Bond -> Bool
applicableAdjust d (L.Bond _ _ oi (L.Floater _ _ rr _ _ _) _ _ _ _ _ _ _ _ )
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

applicableAdjust d (L.Bond _ _ oi (L.Fix _ _ ) _ _ _ _ _ _ _ _ ) = False
applicableAdjust d (L.Bond _ _ oi (L.InterestByYield _ ) _ _ _ _ _ _ _ _ ) = False

setBndsNextIntRate :: TestDeal -> T.Day -> Maybe [RateAssumption] -> TestDeal
setBndsNextIntRate t d (Just ras) = t {bonds = updatedBonds}
    where 
        floatBonds = filter (applicableAdjust d) $ Map.elems (bonds t)
        floatBondNames = map L.bndName floatBonds -- `debug` ("Resetting bonds=>"++ show floatBondNames)
        updatedBonds = foldr (Map.adjust (setBondNewRate d ras)) (bonds t) floatBondNames

setBndsNextIntRate t d Nothing = t 

testCall :: TestDeal -> T.Day -> C.CallOption -> Bool 
testCall t d opt = 
    case opt of 
       C.PoolBalance x -> (queryDeal t (FutureCurrentPoolBalance d)) < x
       C.BondBalance x -> (queryDeal t CurrentBondBalance) < x
       C.PoolFactor x ->  (queryDealRate t (FutureCurrentPoolFactor d)) < fromRational x
                        --  `debug` ("D "++show d++ "Pool Factor query ->" ++ show (queryDealRate t (FutureCurrentPoolFactor d)))
                        -- _factor  < (fromRational x)  `debug` ("Test on Pool Factor: "++show((pool t))++" v-> "++show(_factor))
                        -- where
                        --   _factor = (queryDeal t (FutureCurrentPoolBalance d)) / (queryDeal t OriginalPoolBalance)
       C.BondFactor x ->  (queryDealRate t BondFactor) < (fromRational x)
       C.OnDate x -> x == d 
       C.AfterDate x -> d > x
       C.And xs -> all (testCall t d) xs
       C.Or xs -> any (testCall t d) xs

testCalls :: TestDeal -> Date -> [C.CallOption] -> Bool
testCalls t d [] = False
testCalls t d opts = any (testCall t d) opts -- `debug` ("testing call options")

queryTrigger :: TestDeal -> WhenTrigger -> Map.Map (WhenTrigger,Trigger) TriggerEffect
queryTrigger (TestDeal _ _ _ _ _ _ _ _ _ _ _ Nothing _) wt = Map.empty
queryTrigger (TestDeal _ _ _ _ _ _ _ _ _ _ _ (Just trgsM) _) wt 
  = Map.filterWithKey (\(_wt,_) v -> _wt == wt) trgsM  
       

testTrigger :: TestDeal -> Date -> Trigger -> Bool 
testTrigger t d trigger = 
  case trigger of 
    (ThresholdConstant Below ds v) -> (queryDeal t ds) < v
    (ThresholdConstant EqBelow ds v) -> (queryDeal t ds) <= v
    (ThresholdConstant Above ds v) -> (queryDeal t ds) > v
    (ThresholdConstant EqAbove ds v) -> (queryDeal t ds) >= v
    (ThresholdCurve Below ds ts ) -> (queryDeal t ds) < (fromRational (getValByDate ts d))
    (ThresholdCurve EqBelow ds ts ) -> (queryDeal t ds) <= (fromRational (getValByDate ts d))
    (ThresholdCurve Above ds ts ) -> (queryDeal t ds) > (fromRational (getValByDate ts d))
    (ThresholdCurve EqAbove ds ts ) -> (queryDeal t ds) >= (fromRational (getValByDate ts d))
    
    AfterDate _d -> d > _d
    AfterOnDate _d ->  d > _d

    (AllTrigger tgs) -> all (testTrigger t d) tgs
    (AnyTrigger tgs) -> any (testTrigger t d) tgs

    Always b -> b


testTriggers :: TestDeal -> Date -> [Trigger] -> Bool
testTriggers t d [] = False
testTriggers t d triggers = any (testTrigger t d) triggers 

runEffects :: TestDeal -> Date -> TriggerEffect -> TestDeal 
runEffects t d te 
  = case te of 
      DealStatusTo _ds -> t {status=_ds}
      DoAccrueFee fns -> 
        let 
          fset = S.fromList fns
          newFeeMap = Map.mapWithKey 
                            (\k v ->
                              if (S.member k fset) then 
                                (calcDueFee t d v)
                              else 
                                v) 
                            (fees t)
        in 
          t {fees = newFeeMap}  
      _ -> t `debug` ("Shouldn't happen")


runTriggers :: TestDeal -> Date -> Map.Map (WhenTrigger,Trigger) TriggerEffect -> TestDeal 
--runTriggers t d empty = t              | AlwaysTrue
runTriggers t d _trgs = 
  foldl 
    (\_t _te -> runEffects _t d _te)
    t
    triggeredEffects 
  where 
    triggeredEffects = Map.elems $
                         Map.filterWithKey
                           (\(aw,k) v -> testTrigger t d k)
                           _trgs

  -- TODO in the best case, trigger should only run when the target variable changes
  -- ie. pool factor should be run when the pool collection day
  --     bond factor should be run at the begining or the ending of the waterfall.
  


run2 :: TestDeal -> Maybe CF.CashFlowFrame -> Maybe [ActionOnDate] -> Maybe [RateAssumption] -> Maybe ([C.CallOption])-> TestDeal
run2 t _ (Just []) _ _   = (prepareDeal t)   `debug` ("End with Empty ActionOnDate")

run2 t poolFlow (Just (ad:ads)) rates calls
  -- | (isNothing poolFlow) && ((queryDeal t AllAccBalance) == 0) = prepareDeal t  `debug` ("End with pool flow and acc balance")
  -- | (isNothing poolFlow) && ((queryDeal t CurrentBondBalance) == 0) 
  | (isNothing poolFlow) && ((queryDeal t  AllAccBalance) == 0) 
     = prepareDeal $ foldl 
                      (performAction (actionDate ad)) 
                      t 
                      cleanUpActions 
                      `debug` ("End with pool flow and bond balance")-- `debug` ("Deal end with bal =0 ")
  | otherwise
  = case ad of
      PoolCollection d _ ->
          case poolFlow of
            Just _poolFlow ->
               run2 dRunWithTrigger1 outstanding_flow (Just ads) rates calls -- `debug` ("Running Pool at"++ show d)
               where
                  (collected_flow,outstanding_flow) = CF.splitCashFlowFrameByDate _poolFlow d  
                  accs = depositPoolInflow (collects t) d collected_flow (accounts t) -- `debug` ("Splitting:"++show(d)++"|||"++show(collected_flow))--  `debug` ("Running AD P"++show(d)) --`debug` ("Deposit-> Collection Date "++show(d)++"with"++show(collected_flow))
                  dAfterDeposit = t {accounts=accs}
                  dRunWithTrigger0 = runTriggers dAfterDeposit d $ queryTrigger dAfterDeposit EndCollection
                  waterfallToExe = Map.findWithDefault [] W.EndOfPoolCollection (waterfall t)  -- `debug` ("AD->"++show(ad)++"remain ads"++show(length ads))
                  dAfterAction = foldl (performAction d) dRunWithTrigger0 waterfallToExe
                  dRunWithTrigger1 = runTriggers dAfterAction d $ queryTrigger dAfterAction EndCollectionWF
            Nothing -> run2 t poolFlow (Just ads) rates calls

      RunWaterfall d _ ->
        case calls of
          Just callOpts ->
              if testCalls dRunWithTrigger1 d callOpts then
                prepareDeal $ foldl (performAction d) dRunWithTrigger1 cleanUpActions `debug` ("Called ! "++ show d)
              else
                run2 dRunWithTrigger1 poolFlow (Just ads) rates calls  `debug` ("Not called "++ show d )
          Nothing ->
             run2 dRunWithTrigger1 poolFlow (Just ads) rates Nothing `debug` ("Running Waterfall at"++ show d)--  `debug` ("!!!Running waterfall"++show(ad)++"Next ad"++show(head ads)++"PoolFLOW>>"++show(poolFlow)++"AllACCBAL"++show(queryDeal t AllAccBalance))
        where
             -- waterfallToExe = Map.findWithDefault [] W.DistributionDay (waterfall t) --  `debug` ("Getting waterfall with wf"++show(waterfallToExe))
             dRunWithTrigger0 = runTriggers t d $ queryTrigger t BeginDistributionWF
             waterfallToExe = Map.findWithDefault 
                                [] 
                                (W.DistributionDay (status t)) 
                                (waterfall t)

             dAfterWaterfall = foldl (performAction d) dRunWithTrigger0 waterfallToExe  -- `debug` ("Waterfall>>>"++show(waterfallToExe))
             dAfterRateSet = setBndsNextIntRate dAfterWaterfall d rates  -- `debug` ("Running Rate assumption"++show(rates)) -- `debug` ("After Rate Set")
             dRunWithTrigger1 = runTriggers dAfterRateSet d $ queryTrigger dAfterRateSet EndDistributionWF
      EarnAccInt d accName ->
        let 
          newAcc = Map.adjust (`A.depositInt` d)  accName  (accounts t)
          dAfterInt = t {accounts=newAcc} 
        in 
          run2 dAfterInt poolFlow (Just ads) rates Nothing
      
      AccrueFee d feeName -> 
        let 
          newFeeMap = Map.adjust (calcDueFee t d) feeName (fees t)
          dAfterFeeAccrued = t {fees=newFeeMap}
        in 
          run2 dAfterFeeAccrued poolFlow (Just ads) rates Nothing

      ResetLiqProvider d liqName -> 
        case (liqProvider t) of 
          Nothing -> run2 t poolFlow (Just ads) rates Nothing
          (Just mLiqProvider) 
            -> let 
                 newLiqMap = Map.adjust (updateLiqProvider t d) liqName mLiqProvider
                 dAfterResetLiq = t {liqProvider =Just newLiqMap}
               in   
                 run2 dAfterResetLiq poolFlow (Just ads) rates Nothing
            
      where
        cleanUpActions = Map.findWithDefault [] W.CleanUp (waterfall t)  --  `debug` ("Running AD"++show(d))


run2 t Nothing Nothing Nothing Nothing
  = run2 t (Just pcf) (Just ads) Nothing Nothing   `debug` ("Everything is Nothing")
  where
    (ads,pcf,rcurves,clls,_) = getInits t Nothing -- `debug` ("Init Done")

run2 t Nothing _ _ _ = prepareDeal t `debug` ("End with Pool CF2")


calcLiquidationAmount :: C.LiquidationMethod -> (P.Pool a) -> Date -> Amount
calcLiquidationAmount alm pool d 
  = case alm of 
      C.BalanceFactor currentFactor defaultFactor ->
          case (P.futureCf pool) of 
            Nothing -> 0  -- `debug` ("No futureCF")
            Just _futureCf ->
                let 
                  poolInflow = CF.getEarlierTsCashFlowFrame _futureCf d -- `debug` ("liq:"++show _futureCf++"D"++ show d)
                  earlierTxns = CF.getTxnAsOf _futureCf d
                  currentDefaulBal = sum $ map (\x -> (CF.mflowDefault x) - (CF.mflowRecovery x) - (CF.mflowLoss x)) earlierTxns
                in 
                  case poolInflow of 
                    Nothing -> 0  -- `debug` ("No pool Inflow")
                    Just _ts ->   -- TODO need to check if missing last row
                        (mulBR (CF.mflowBalance _ts) currentFactor) + (mulBR currentDefaulBal defaultFactor) 
                        -- `debug` ("LIQ:"++show poolInflow)

      C.PV discountRate recoveryPct ->
          case (P.futureCf pool) of
            Nothing -> 0 
            Just _futureCf ->
                let 
                  futureTxns = CF.getTxnAfter _futureCf d
                  earlierTxns = CF.getTxnAsOf _futureCf d
                  pvCf = sum $ map (\x -> pv2  discountRate  d (CF.tsDate x) (CF.tsTotalCash x)) futureTxns 
                  currentDefaulBal = sum $ map (\x -> (CF.mflowDefault x) - (CF.mflowRecovery x) - (CF.mflowLoss x)) earlierTxns
                in 
                  pvCf + mulBI currentDefaulBal recoveryPct

liquidatePool :: C.LiquidationMethod -> T.Day -> String -> TestDeal -> TestDeal
liquidatePool lq d accName t =
  t {accounts = Map.adjust updateFn accName accs} -- `debug` ("Accs->"++show(accs))
  where
     proceeds = calcLiquidationAmount lq (pool t) d
     updateFn = A.deposit proceeds d (LiquidationProceeds proceeds)
     accs = accounts t

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
    DealTxns -> (finalDeal, Just pcf, Just (extractExecutionTxns finalDeal ),Nothing)
  where
    (ads,pcf,rcurves,calls,t2) = getInits t assumps --  `debug` ("Init in runDeal")
    finalDeal = run2 t2 (Just pcf) (Just ads) (Just rcurves) calls -- `debug` ("ACTIONS"++show ads)-- `debug` ("Init Actions"++show(sort ads)) -- ++"pool flows"++show(pcf)) -- `debug` (">>ADS==>> "++show(ads))
    bndPricing = case bpi of
                   Nothing -> Nothing   -- `debug` ("pricing bpi with Nothing")
                   Just _bpi -> Just (priceBonds finalDeal _bpi)  -- `debug` ("Pricing with"++show _bpi)

prepareDeal :: TestDeal -> TestDeal 
prepareDeal t = t {bonds = Map.map L.consolStmt (bonds t)} -- `debug` ("Consolidation in preparingw")

buildRateCurves :: [RateAssumption]-> [AP.AssumptionBuilder] -> [RateAssumption] 
buildRateCurves rs (assump:assumps) = 
    case assump of 
      AP.InterestRateConstant i f -> 
        buildRateCurves ((RateFlat i f):rs) assumps
      AP.InterestRateCurve i ds ->  -- Index [(T.Day, Float)]
        buildRateCurves ((RateCurve i (dsToTs ds)):rs) assumps
      _ -> buildRateCurves rs assumps    
    where  
        dsToTs ds = IRateCurve $ map (\(d,f) -> (TsPoint d f) ) ds
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
    _pool = pool t
    newPool = _pool {P.futureCf = Just cf}

populateDealDates :: DateDesp -> (Date,Date,[ActionOnDate],[ActionOnDate],Date)
populateDealDates (CustomDates _cutoff _ps _closing _bs) 
  = (actionDate (head _ps)
     ,actionDate (head _bs)
     ,_ps
     ,_bs
     ,(actionDate (max (last _ps) (last _bs))))

populateDealDates (PatternInterval _m) 
  = (startDate,firstPayDate,pa,ba,max ed1 ed2) -- `debug` ("PA>>>"++ show pa)
    where 
      pa = [ PoolCollection _d "" | _d <- genSerialDatesTill startDate dp1 ed1 ]
      ba = [ RunWaterfall _d "" | _d <- genSerialDatesTill firstPayDate dp2 ed2 ]
      (startDate,dp1,ed1) = _m Map.! CutoffDate
      (firstPayDate,dp2,ed2) = _m Map.! FirstPayDate 
    

getInits :: TestDeal -> Maybe [AP.AssumptionBuilder] -> 
    ([ActionOnDate], CF.CashFlowFrame, [RateAssumption],Maybe [C.CallOption]
      ,TestDeal)
getInits t mAssumps =
    (allActionDates
    ,pCollectionCfAfterCutoff
    ,rateCurves
    ,callOptions  
    ,t_with_cf)   -- `debug` ("deal with pool"++show(pool t_with_cf))
  where
    assumps = fromMaybe [] mAssumps

    projNum = 512
    
    (startDate,firstPayDate,pActionDates,bActionDates,endDate) = populateDealDates (dates t)   
    intEarnDates = A.buildEarnIntAction (Map.elems (accounts t)) _farEnoughDate []
    iAccIntDates = [ EarnAccInt _d accName | (accName,accIntDates) <- intEarnDates
                                           , _d <- accIntDates ] -- `debug` ("PoolactionDates"++show  pActionDates)
                                           
    --fee accrue dates 
    _feeAccrueDates = F.buildFeeAccrueAction (Map.elems (fees t)) _farEnoughDate []
    feeAccrueDates = [ AccrueFee _d _feeName | (_feeName,feeAccureDates) <- _feeAccrueDates
                                           , _d <- feeAccureDates ]
    --liquidation facitliy 
    
    
    liqResetDates = case (liqProvider t) of 
                      Nothing -> []
                      Just mLiqProvider -> 
                          let 
                            _liqResetDates = CE.buildLiqResetAction (Map.elems mLiqProvider) _farEnoughDate []                    
                          in 
                            [ ResetLiqProvider _d _liqName |(_liqName,__liqResetDates) <- _liqResetDates
                                                          , _d <- __liqResetDates ]

    stopDate = find 
                 (\case
                   (AP.StopRunBy d) -> True
                   _ -> False)
                 assumps --  `debug` (">>Assumps"++show(assumps))

    _actionDates = sort $ bActionDates ++ 
                          pActionDates ++ 
                          iAccIntDates ++ 
                          feeAccrueDates ++
                          liqResetDates-- `debug` (">>pactionDates"++show feeAccrueDates)
    allActionDates = case stopDate of
                    Just (AP.StopRunBy d) 
                      -> filter
                           (\x -> actionDate x < d)
                           _actionDates
                    Nothing ->  _actionDates   -- `debug` (">>action dates done"++show(_actionDates))

    poolCf = P.aggPool $ P.runPool2 (pool t) assumps -- `debug` show (P.runPool2 (pool t) assumps) --  `debug` ("Init Pools"++show(pool t)) -- `debug` ("Assets Agged pool Cf->"++show(pool t))
    poolCfTs = filter (\txn -> CF.tsDate txn >= startDate)  $ CF.getTsCashFlowFrame poolCf --  `debug` ("projected & aggred pool cf"++show(poolCf))
    pCollectionCfAfterCutoff = CF.CashFlowFrame $  CF.aggTsByDates poolCfTs (actionDates pActionDates) -- `debug` ("poolCf "++show(poolCfTs)++">>"++show(actionDates pActionDates) ) -- `debug` ("pool cf ts"++show(poolCfTs))
    t_with_cf  = setFutureCF t pCollectionCfAfterCutoff --  `debug` ("aggedCf:->>"++show(pCollectionCfAfterCutoff))
    rateCurves = buildRateCurves [] assumps   -- [RateCurve LIBOR6M (FloatCurve [(TsPoint (T.fromGregorian 2022 1 1) 0.01)])]
    callOptions = buildCallOptions Nothing assumps -- `debug` ("Assump"++show(assumps))


queryDealRate :: TestDeal -> DealStats -> Micro
queryDealRate t s =
  case s of
    BondFactor ->
        fromRational $ (toRational (queryDeal t CurrentBondBalance)) / (toRational (queryDeal t OriginalBondBalance))

    PoolFactor ->
        fromRational $
           (toRational (queryDeal t CurrentPoolBalance))  / (toRational (queryDeal t OriginalPoolBalance))

    FutureCurrentPoolFactor asOfDay ->
        fromRational $
           (toRational (queryDeal t (FutureCurrentPoolBalance asOfDay))) / (toRational (queryDeal t OriginalPoolBalance) )


queryDeal :: TestDeal -> DealStats -> Balance
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
       -- foldl (\acc x -> (acc + (P.getOriginBal x))) 0.0 (P.assets (pool t))
       case (P.issuanceStat (pool t)) of
         Just m -> Map.findWithDefault (-1) P.IssuanceBalance m -- `debug` (">>>>"++show(m))
         Nothing -> (-1) -- `debug` ("Pool Stat"++show(pool t))

    AllAccBalance ->
        sum $ map A.accBalance $ Map.elems (accounts t) -- `debug` ("Summing acc balance")
    
    AccBalance ans -> 
        sum $ map A.accBalance $ Map.elems $ getAccountByName t (Just ans)

    ReserveAccGapAt d ans ->
        max 0 $
          (sum $ map 
                   (calcTargetAmount t d) $ 
                   Map.elems $ getAccountByName t (Just ans))
          - 
          (queryDeal t (AccBalance ans)) 
        -- 0.0 -- let t d 
    --FutureOriginalPoolBalance ->
    --  CF.mflowBalance $ head (CF.getTsCashFlowFrame _pool_cfs)
    -- where
    --  _pool_cfs = fromMaybe (CF.CashFlowFrame []) (P.futureCf (pool t))

    FutureCurrentPoolBalance asOfDay ->
         case _poolSnapshot of
            Just ts -> CF.mflowBalance ts  -- `debug` ("REsult 1 >>"++show(CF.mflowBalance ts))
            Nothing -> -0.1  -- `debug` ("REsult 2 >>"++show(_poolSnapshot))
        where
         _pool_cfs = fromMaybe (CF.CashFlowFrame []) (P.futureCf (pool t))
         _poolSnapshot = CF.getEarlierTsCashFlowFrame _pool_cfs asOfDay -- `debug` (">>CurrentPoolBal"++show(asOfDay)++">>Pool>>"++show(_pool_cfs))

    FutureCurrentPoolBegBalance asOfDay ->
         case _poolSnapshot of
            Just ts -> CF.mflowBegBalance ts
            Nothing -> -0.1
        where
         _pool_cfs = fromMaybe (CF.CashFlowFrame []) (P.futureCf (pool t))
         _poolSnapshot = CF.getEarlierTsCashFlowFrame _pool_cfs asOfDay -- `debug` (">>CurrentPoolBal"++show(asOfDay)++">>Pool>>"++show(_pool_cfs))

    PoolCollectionHistory incomeType fromDay asOfDay ->
      sum fieldAmts
      where
        fieldAmts = map
                      (case incomeType of 
                        CollectedInterest -> CF.mflowInterest 
                        CollectedPrincipal -> CF.mflowPrincipal 
                        CollectedPrepayment -> CF.mflowPrepayment
                        CollectedRecoveries -> CF.mflowRecovery)    
                      subflow  -- `debug` ("SDED"++ show fromDay ++ show asOfDay ++"Pool Collection Histroy"++show subflow)
        subflow = case (P.futureCf (pool t)) of
                    Nothing ->  []
                    Just _futureCf -> 
                        if fromDay == asOfDay then 
                            CF.getTxnBetween2 _futureCf II fromDay asOfDay
                        else 
                            CF.getTxnBetween2 _futureCf EI fromDay asOfDay

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
          sum $ map L.bndBalance $ Map.elems bSubMap

    BondsIntPaidAt d bns ->
       let
          bnSet = S.fromList bns
          bSubMap = Map.filterWithKey (\bn b -> S.member bn bnSet) (bonds t)
          stmts = map L.bndStmt $ Map.elems bSubMap
          ex s = case s of
                   Nothing -> 0
                   Just (Statement txns) 
                     -> sum $ map getTxnAmt $
                          filter (\y -> case (getTxnComment y) of 
                                          (PayInt _ _) -> True
                                          _ -> False)   $
                          filter (\x -> d == getTxnDate x) txns
                    -- ("INT PAY" == (getTxnComment x))
       in
          sum $ map ex stmts

    FeesPaidAt d fns ->
       let
          fnSet = S.fromList fns
          fSubMap = Map.filterWithKey (\fn f -> (S.member fn fnSet)) (fees t)
          stmts = map F.feeStmt $ Map.elems fSubMap
          ex s = case s of
                   Nothing -> 0
                   Just (Statement txns) -> sum $ map getTxnAmt $ filter (\x ->  d == getTxnDate x) txns
       in
          sum $ map ex stmts

    CurrentDueBondInt bns ->
        let
           bnSet = S.fromList bns
           bSubMap = Map.filterWithKey (\bn b -> (S.member bn bnSet)) (bonds t)
        in
           sum $ map L.bndDueInt $ Map.elems bSubMap

    CurrentDueFee fns ->
        let
           fnSet = S.fromList fns
           fSubMap = Map.filterWithKey (\fn f -> (S.member fn fnSet)) (fees t)
        in
           sum $ map F.feeDue $ Map.elems fSubMap
    Sum _s ->
        sum $ map (queryDeal t) _s

    Substract (ds:dss) -> 
        (queryDeal t ds) - (queryDeal t (Sum dss))

    Constant n -> fromRational n

    Max ds1 ds2 ->
        max (queryDeal t ds1) (queryDeal t ds2)
     
    Min ds1 ds2 ->
        min (queryDeal t ds1) (queryDeal t ds2)



getPoolFlows :: TestDeal -> Maybe Date -> Maybe Date -> RangeType -> [CF.TsRow]
getPoolFlows t sd ed rt =
  case (sd,ed) of
    (Nothing,Nothing) ->  _trs
    (Nothing,Just _ed) -> case rt of 
                             EI -> filter (\x -> CF.tsDate x <= _ed) _trs
    (Just _sd,Nothing) -> CF.getTxnAfter _projCf _sd   -- >= d
    (Just _sd,Just _ed) -> case rt of 
                             IE -> filter (\x -> (CF.tsDate x >= _sd) && (CF.tsDate x < _ed)) _trs
                             EI -> filter (\x -> (CF.tsDate x > _sd) && (CF.tsDate x <= _ed)) _trs
  where
    _projCf = fromMaybe (CF.CashFlowFrame []) (P.futureCf (pool t))
    _trs =  CF.getTsCashFlowFrame _projCf

--getBondStmts :: TestDeal -> [String] -> Maybe Date -> Maybe Date -> Map String [Txn]
--getBondStmts t bondNames startDate endDate
--  =
calcDayToPoolDate :: TestDeal -> Date -> Date 
calcDayToPoolDate t calcDay 
  = CF.mflowDate $ last pFlows -- `debug` ("calDayToPoolDate"++show calcDay ++">>>>>"++show pFlows)
    where 
      pFlows = getPoolFlows t Nothing (Just calcDay) EI  -- II here is not being used


calcDueFee :: TestDeal -> Date -> F.Fee -> F.Fee
calcDueFee t calcDay f@(F.Fee fn (F.FixFee amt) fs fd fdDay fa _ _)
  | isJust fdDay = f  
  | calcDay >= fs && (isNothing fdDay) = f{ F.feeDue = amt, F.feeDueDate = Just calcDay} -- `debug` ("DEBUG--> init with amt "++show(fd)++show amt)
  | otherwise = f

calcDueFee t calcDay f@(F.Fee fn (F.AnnualRateFee feeBase r) fs fd Nothing fa lpd _)
  | calcDay >= fs = calcDueFee t calcDay f {F.feeDueDate = Just fs }
  | otherwise = f -- `debug` ("Fee Equal ? "++show calcDay ++show fs)

calcDueFee t calcDay f@(F.Fee fn (F.AnnualRateFee feeBase r) fs fd (Just _fdDay) fa lpd _)
  = f{ F.feeDue=fd+newDue, F.feeDueDate = Just newDueDay }  -- `debug` ("Fee DUE new Due "++show calcDay ++show baseBal ++show(newDue))                   
      where 
        accrueStart = _fdDay
        collectionEndDay = calcDayToPoolDate t calcDay
        (baseBal,newDueDay) = case feeBase of
                                CurrentPoolBalance ->  (CF.mflowWeightAverageBalance accrueStart calcDay $ getPoolFlows t Nothing Nothing II,collectionEndDay)
                                -- CurrentPoolBegBalance ->  CF.mflowWeightAverageBalance accrueStart calcDay $ getPoolFlows t Nothing Nothing
                                OriginalPoolBalance -> (mulBR (P.getIssuanceField (pool t) P.IssuanceBalance) (yearCountFraction DC_ACT_365F accrueStart calcDay),collectionEndDay)
                                OriginalBondBalance -> (mulBR (queryDeal t OriginalBondBalance) (yearCountFraction DC_ACT_365F accrueStart calcDay),calcDay)
                                CurrentBondBalance -> (Map.foldr (\v a-> a + L.weightAverageBalance accrueStart calcDay v ) 0.0 (bonds t),calcDay)
                                CurrentBondBalanceOf bns -> (Map.foldr (\v a-> a + L.weightAverageBalance accrueStart calcDay v ) 0.0 (getBondByName t (Just bns)),calcDay)

        newDue = mulBR baseBal r

calcDueFee t calcDay f@(F.Fee fn (F.PctFee (PoolCollectionIncome it) r ) fs fd fdDay fa lpd _)
  = f { F.feeDue = newDueFee, F.feeDueDate = Just calcDay } -- `debug` ("BAL"++show baseBal++"New Fee Due"++ show newDueFee)
    where 
      baseBal = queryDeal t (PoolCollectionHistory it lastBegDay calcDay)  
                   -- `debug` ("PH query at "++ show calcDay ++ ">>>" ++ show it++">>"++show lastBegDay++">>"++show calcDay)
      newDueFee = fd + mulBR baseBal r
      lastBegDay = case fdDay of
                     (Just _fdDay) -> _fdDay
                     Nothing -> fs

calcDueFee t calcDay f@(F.Fee fn (F.PctFee ds r ) fs fd fdDay fa lpd _)
  = f { F.feeDue = fd + mulBR baseBal r, F.feeDueDate = Just calcDay }
    where 
      baseBal = queryDeal t ds 
      lastBegDay = case fdDay of
                     (Just _fdDay) -> _fdDay
                     Nothing -> fs


calcDueFee t calcDay f@(F.Fee fn (F.FeeFlow ts)  fs fd Nothing fa mflpd _)
  = f{ F.feeDue = newFeeDue
      ,F.feeDueDate = Just calcDay
      ,F.feeType = (F.FeeFlow futureDue)} -- `debug` ("New fee"++show(f))
    where
      newFeeDue =  cumulativeDue + fd
      (currentNewDue,futureDue) = splitTsByDate ts calcDay
      cumulativeDue = sumValTs currentNewDue

calcDueFee t calcDay f@(F.Fee fn (F.RecurFee p amt)  fs fd Nothing fa _ _)
  = f{ F.feeDue = amt * (fromIntegral (periodGaps - 1)), F.feeDueDate = Just calcDay } -- `debug` ("New fee"++show(f))
  where
    periodGaps = length $ projDatesByPattern p fs calcDay  -- `debug` ("###"++show (projDatesByPattern p fs calcDay))

calcDueFee t calcDay f@(F.Fee fn (F.RecurFee p amt)  fs fd (Just _fdDay) fa _ _)
  | _fdDay == calcDay = f
  | periodGap == 0 = f
  | otherwise = f { F.feeDue = (fd+(amt*(fromIntegral (periodGap - 1)))) , F.feeDueDate = Just calcDay } -- `debug` ("Gap->"++show(fromIntegral periodGap))
  where
    periodGap =  length $ projDatesByPattern p _fdDay calcDay

updateLiqProvider :: TestDeal -> Date -> CE.LiqFacility -> CE.LiqFacility
updateLiqProvider t d liq@(CE.LiqFacility _ (CE.ReplenishSupport _ b) curBal _ curCredit stmt)
  = liq { CE.liqBalance = (max b curBal)}
updateLiqProvider t d liq = liq


calcDueInt :: TestDeal -> Date -> L.Bond -> L.Bond
calcDueInt t calc_date b@(L.Bond _ _ _ _ _ _ _ _ Nothing _ _ _) 
  = calcDueInt t calc_date (b {L.bndDueIntDate = Just (getClosingDate (dates t))})

calcDueInt t calc_date b@(L.Bond bn L.Z bo bi bond_bal bond_rate _ _ _ lstIntPay _ _) 
  = b {L.bndDueInt = 0 }

calcDueInt t calc_date b@(L.Bond bn _ bo (L.InterestByYield y) bond_bal _ _ int_due _ lstIntPay _ mStmt)
  = b {L.bndDueInt = newDue+ int_due }
  where
  newDue = L.backoutDueIntByYield calc_date b

calcDueInt t calc_date b@(L.Bond bn bt bo bi bond_bal bond_rate _ int_due (Just int_due_date) lstIntPay _ _) 
  | calc_date == int_due_date = b
  | otherwise = b {L.bndDueInt = (new_due_int+int_due),L.bndDueIntDate = Just calc_date }   -- `debug` ("Due INT"++show calc_date ++">>"++show(bn)++">>"++show int_due++">>"++show(new_due_int))
              where
                lastIntPayDay = case lstIntPay of
                                  Just pd -> pd
                                  Nothing -> getClosingDate (dates t)
                dc = case bi of 
                       L.Floater _ _ _ _dc _ _ -> _dc 
                       L.Fix _ _dc -> _dc 
                     
                new_due_int = calcInt bond_bal lastIntPayDay calc_date bond_rate DC_ACT_365F -- `debug` ("Bond bal"++show bond_bal++">>"++show lastIntPayDay++">>"++ show calc_date++">>"++show bond_rate)


calcDuePrin :: TestDeal -> T.Day -> L.Bond -> L.Bond
calcDuePrin t calc_date b@(L.Bond bn L.Sequential bo bi bond_bal _ prin_arr int_arrears _ _ _ _) =
  b {L.bndDuePrin = duePrin} 
  where
    duePrin = bond_bal 

calcDuePrin t calc_date b@(L.Bond bn (L.Lockout cd) bo bi bond_bal _ prin_arr int_arrears _ _ _ _) =
  if cd > calc_date then 
    b {L.bndDuePrin = 0}
  else
    b {L.bndDuePrin = duePrin}
  where
    duePrin = bond_bal 

calcDuePrin t calc_date b@(L.Bond bn (L.PAC schedule) bo bi bond_bal _ prin_arr int_arrears _ _ _ _) =
  b {L.bndDuePrin = duePrin} -- `debug` ("bn >> "++bn++"Due Prin set=>"++show(duePrin) )
  where
    scheduleDue = getValOnByDate schedule calc_date  
    duePrin = max (bond_bal - scheduleDue) 0 -- `debug` ("In PAC ,target balance"++show(schedule)++show(calc_date)++show(scheduleDue))

calcDuePrin t calc_date b@(L.Bond bn (L.PAC_Anchor schedule bns) bo bi bond_bal _ prin_arr int_arrears _ _ _ _) =
  b {L.bndDuePrin = duePrin} -- `debug` ("bn >> "++bn++"Due Prin set=>"++show(duePrin) )
  where
    scheduleDue = getValOnByDate schedule calc_date
    anchor_bond_balance = (queryDeal t (CurrentBondBalanceOf bns))
    duePrin = if (anchor_bond_balance > 0) then
                 max (bond_bal - scheduleDue) 0
              else
                 bond_bal

calcDuePrin t calc_date b@(L.Bond bn L.Z bo bi bond_bal bond_rate prin_arr int_arrears _ lstIntPay _ _) =
  if (all (\x -> (isZbond x)) activeBnds) then
      b {L.bndDuePrin = bond_bal} -- `debug` ("bn >> "++bn++"Due Prin set=>"++show(duePrin) )
  else 
      b {L.bndDuePrin = 0, L.bndBalance = new_bal, L.bndLastIntPay=Just calc_date} -- `debug` ("bn >> "++bn++"Due Prin set=>"++show(duePrin) )
  where
    isZbond (L.Bond _ bt _ _ _ _ _ _ _ _ _ _) 
      = case bt of
          L.Z -> True
          _ -> False
    activeBnds = filter (\x -> (L.bndBalance x) > 0) (Map.elems (bonds t))
    new_bal = bond_bal + dueInt
    lastIntPayDay = case lstIntPay of
                      Just pd -> pd
                      Nothing -> getClosingDate (dates t)
    dueInt = calcInt bond_bal lastIntPayDay calc_date bond_rate DC_ACT_365F

calcDuePrin t calc_date b@(L.Bond bn L.Equity bo bi bond_bal _ prin_arr int_arrears _ _ _ _) =
  b {L.bndDuePrin = bond_bal }

patchDateToStats :: Date -> DealStats -> DealStats
patchDateToStats d t
   = case t of
         CurrentPoolBalance -> FutureCurrentPoolBalance d
         LastBondIntPaid bns -> BondsIntPaidAt d bns
         LastFeePaid fns -> FeesPaidAt d fns
         _ -> t

calcTargetAmount :: TestDeal -> Date -> A.Account -> Balance
calcTargetAmount t d (A.Account _ n i Nothing _ ) = 0
calcTargetAmount t d (A.Account _ n i (Just r) _ ) =
   eval r -- `debug` ("$$$$ Evaluating" ++show(r)++" result:==>>"++show((eval r)))
   where
     eval ra = case ra of
       A.PctReserve (Sum ds) _rate -> mulBR (queryDeal t (Sum (map (patchDateToStats d) ds))) _rate  -- `debug` ("In multiple query spot"++show(ds))
       A.PctReserve ds _rate -> mulBR (queryDeal t (patchDateToStats d ds))  _rate
       A.FixReserve amt -> amt
       A.Max ra1 ra2 -> max (eval ra1) (eval ra2)  -- `debug` ("Max result here ->>> left "++show(eval ra1)++" right "++show(eval ra2))
       A.Min ra1 ra2 -> min (eval ra1) (eval ra2)

--calcTargetAmount :: TestDeal -> A.Account -> Balance
--calcTargetAmount t (A.Account _ n i (Just r) _ ) =
--   eval r -- `debug` ("$$$$ Evaluating" ++show(r)++" result:==>>"++show((eval r)))
--   where
--     eval ra = case ra of
--       A.PctReserve (Sum ds) _rate -> mulBR (queryDeal t (Sum ds)) _rate  -- `debug` ("In multiple query spot"++show(ds))
--       A.PctReserve ds _rate -> mulBR (queryDeal t ds)  _rate
--       A.FixReserve amt -> amt
--       A.Max ra1 ra2 -> max (eval ra1) (eval ra2)  -- `debug` ("Max result here ->>> left "++show(eval ra1)++" right "++show(eval ra2))
--       A.Min ra1 ra2 -> min (eval ra1) (eval ra2)


depositPoolInflow :: [W.CollectionRule] -> T.Day -> Maybe CF.CashFlowFrame -> Map.Map String A.Account -> Map.Map String A.Account
depositPoolInflow rules d Nothing amap = amap -- `debug` ("Deposit inflow Nothing")
depositPoolInflow rules d (Just (CF.CashFlowFrame txn)) amap =
  foldl fn amap rules  -- `debug` ("Deposit inflow  at "++show(d)++"txn"++show(txn))
  where
      currentPoolInflow = txn

      fn _acc _r@(W.Collect _poolSource _accName) =
          Map.adjust (A.deposit collectedCash d (PoolInflow _poolSource)) _accName _acc
          where
              collectedCash = sum $ map (collectCash _r) currentPoolInflow

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
  ,dates = PatternInterval $
             Map.fromList [(ClosingDate ,(T.fromGregorian 2022 1 1,MonthFirst,toDate("20300101")))
                         ,(CutoffDate,(T.fromGregorian 2022 1 1,MonthFirst,toDate("20300101")))
                         ,(FirstPayDate,(T.fromGregorian 2022 2 25,DayOfMonth 25,toDate("20300101") ))
                          ]
  ,status = Amortizing
  ,accounts = (Map.fromList
  [("General", (A.Account { A.accName="General" ,A.accBalance=0.0 ,A.accType=Nothing, A.accInterest=Nothing ,A.accStmt=Nothing
  })),
   ("Reserve", (A.Account { A.accName="General" ,A.accBalance=0.0 ,A.accType=Just (A.FixReserve 500), A.accInterest=Nothing ,A.accStmt=Nothing
  }))
   ,("ReservePCT", (A.Account { A.accName="General" ,A.accBalance=0.0
   ,A.accType=Just (A.PctReserve (FutureCurrentPoolBalance (T.fromGregorian 2022 2 25) ) 500)
   , A.accInterest=Nothing ,A.accStmt=Nothing
  }))
  ])
  ,fees = (Map.fromList [("Service-Fee"
                         ,F.Fee{F.feeName="service-fee"
                                ,F.feeType = F.FixFee 50
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
                 ,P.asOfDate=T.fromGregorian 2022 1 1
                 ,P.issuanceStat= Just (Map.fromList [(P.IssuanceBalance,4000)])
                 }
   ,waterfall = Map.fromList [(W.DistributionDay Amortizing, [
                                 (Nothing, W.PayFee ["General"] ["Service-Fee"])
                                 ,(Nothing, W.PayFeeBy (W.DuePct 0.5) ["General"] ["Service-Fee"])
                                 ,(Nothing, W.TransferReserve W.Source  "General" "General" Nothing)
                                 ,(Nothing, W.TransferReserve W.Target  "General" "General" Nothing)
                                 ,(Nothing, W.PayInt "General" ["A"])
                                 ,(Nothing, W.PayPrin "General" ["A"])])
                               ,(W.CleanUp, [(Nothing, W.LiquidatePool (C.BalanceFactor 1.0 0.2) "A")])]
 ,collects = [W.Collect W.CollectedInterest "General"
             ,W.Collect W.CollectedPrincipal "General"]
 ,call = Just [C.PoolFactor 0.08]
 ,liqProvider = Nothing
 ,triggers = Just $ 
               Map.fromList $
                 [((BeginDistributionWF,AfterDate (toDate "20220301")) ,DealStatusTo Revolving)
                  ,((EndCollection,Always True),DoAccrueFee ["Service-Fee"]) 
                   ]
 ,overrides = Just [ CustomActionOnDates 
                      [RunWaterfall (toDate "20220101") "base"
                      ,PoolCollection (toDate "20221101") "collection"] ]  
}



$(deriveJSON defaultOptions ''TestDeal)
