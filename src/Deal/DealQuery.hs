{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Deal.DealQuery (queryDealBool,queryDeal,queryDealInt,queryDealRate
                       ,patchDateToStats, testPre, calcTargetAmount) 
  where

import Deal.DealBase
import Types
import qualified Asset as P
import Data.List
import Data.Fixed
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as S
import qualified Liability as L
import qualified Cashflow as CF
import qualified Data.Time as T
import qualified Accounts as A
import qualified Ledger as LD
import qualified Expense as F
import qualified Triggers as Trg
import qualified CreditEnhancement as CE
import Stmt
import Util
import Lib

import Debug.Trace
debug = flip trace

-- | calcuate target balance for a reserve account, 0 for a non-reserve account
calcTargetAmount :: P.Asset a => TestDeal a -> Date -> A.Account -> Balance
calcTargetAmount t d (A.Account _ n i Nothing _ ) = 0
calcTargetAmount t d (A.Account _ n i (Just r) _ ) =
   eval r 
   where
     eval ra = case ra of
       A.PctReserve (Sum ds) _rate -> mulBR (queryDeal t (Sum (map (patchDateToStats d) ds))) _rate  -- `debug` ("In multiple query spot"++show(ds))
       A.PctReserve ds _rate -> mulBR (queryDeal t (patchDateToStats d ds))  _rate
       A.FixReserve amt -> amt
       A.Either p ra1 ra2 -> if testPre d t p then 
                                eval ra1
                            else 
                                eval ra2 
       A.Max ras -> maximum' $ eval <$> ras
       A.Min ras -> minimum' $ eval <$> ras

patchDateToStats :: Date -> DealStats -> DealStats
patchDateToStats d t
   = case t of
         CurrentPoolBalance -> FutureCurrentPoolBalance
         LastBondIntPaid bns -> BondsIntPaidAt d bns
         LastFeePaid fns -> FeesPaidAt d fns
         LastBondPrinPaid bns -> BondsPrinPaidAt d bns
         LastPoolDefaultedBal -> PoolNewDefaultedAt d
         BondBalanceGap bn -> BondBalanceGapAt d bn
         ReserveAccGap ans -> ReserveAccGapAt d ans
         ReserveExcess ans -> ReserveExcessAt d ans
         Sum _ds -> Sum $ map (patchDateToStats d) _ds
         Substract _ds -> Substract $ map (patchDateToStats d) _ds
         Min dss -> Min $ [ patchDateToStats d ds | ds <- dss ] 
         Max dss -> Max $ [ patchDateToStats d ds | ds <- dss ]
         Factor _ds r -> Factor (patchDateToStats d _ds) r
         UseCustomData n -> CustomData n d
         CurrentPoolBorrowerNum -> FutureCurrentPoolBorrowerNum d
         FeeTxnAmt ns mCmt -> FeeTxnAmtBy d ns mCmt
         BondTxnAmt ns mCmt -> BondTxnAmtBy d ns mCmt
         AccTxnAmt ns mCmt -> AccTxnAmtBy d ns mCmt
         _ -> t



queryDealRate :: P.Asset a => TestDeal a -> DealStats -> Micro
queryDealRate t s =
  fromRational $ 
    case s of
      BondFactor ->
           toRational (queryDeal t CurrentBondBalance) / toRational (queryDeal t OriginalBondBalance)

      PoolFactor ->
           toRational (queryDeal t CurrentPoolBalance)  / toRational (queryDeal t OriginalPoolBalance)

      FutureCurrentPoolFactor asOfDay ->
           toRational (queryDeal t FutureCurrentPoolBalance) / toRational (queryDeal t OriginalPoolBalance)
      
      CumulativePoolDefaultedRate ->
          let 
            originPoolBal = toRational (queryDeal t OriginalPoolBalance) -- `debug` ("A")-- `debug` (">>Pool Bal"++show (queryDeal t OriginalPoolBalance))
            cumuPoolDefBal = toRational (queryDeal t CumulativePoolDefaultedBalance) -- `debug` ("B") -- `debug` (">>CUMU"++show (queryDeal t CumulativePoolDefaultedBalance))
          in 
            cumuPoolDefBal / originPoolBal -- `debug` ("cumulative p def rate"++show cumuPoolDefBal++">>"++show originPoolBal)
      BondRate bn -> 
        toRational $ L.bndRate $ bonds t Map.! bn
      
      PoolWaRate -> 
        toRational $ 
          case P.futureCf (pool t) of 
            Nothing -> 0
            Just (CF.CashFlowFrame trs) -> CF.mflowRate $ last trs
      Constant r -> r
      Max ss -> toRational $ maximum' [ queryDealRate t s | s <- ss ]
      Min ss -> toRational $ minimum' [ queryDealRate t s | s <- ss ]
      Substract (s1:ss) -> toRational $ (queryDealRate t s1) - (queryDealRate t (Sum ss))
      Sum ss -> toRational $ sum $ (queryDealRate t) <$> ss  
      FloorAndCap floor cap s ->  
        let 
          [_f,_c,_s] = toRational <$> (queryDealRate t) <$> [floor,cap,s]
        in 
          max _f (min _c _s)
      FloorWith s floor -> toRational $ max (queryDealRate t s) (queryDealRate t floor)
      FloorWithZero s -> toRational $ max (queryDealRate t s) 0
      CapWith s cap -> toRational $ min (queryDealRate t s) (queryDealRate t cap)

queryDealInt :: P.Asset a => TestDeal a -> DealStats -> Date -> Int 
queryDealInt t s d = 
  case s of 
    FutureCurrentPoolBorrowerNum d ->
      case P.futureCf (pool t) of 
        Nothing -> 0
        Just (CF.CashFlowFrame trs) -> 
            let 
              (_cf,_) = splitByDate trs d EqToLeft
            in 
              fromMaybe 0 $ CF.mflowBorrowerNum $ last _cf

    MonthsTillMaturity bn -> 
        case mm of 
          Nothing -> error "Should not happend"
          Just md -> fromInteger $ T.cdMonths $ T.diffGregorianDurationClip md d
        where
            (L.Bond _ _ (L.OriginalInfo _ _ _ mm) _ _ _ _ _ _ _ _ _) = (bonds t) Map.! bn  
    FloorAndCap floor cap s -> max (queryDealInt t floor d) $ min (queryDealInt t cap d ) (queryDealInt t s d)
    FloorWith s floor -> max (queryDealInt t s d) (queryDealInt t floor d)
    FloorWithZero s -> max (queryDealInt t s d) 0
    CapWith s cap -> min (queryDealInt t s d) (queryDealInt t cap d)

    Max ss -> maximum' $ [ queryDealInt t s d | s <- ss ]
    Min ss -> minimum' $ [ queryDealInt t s d | s <- ss ]

poolSourceToIssuanceField :: PoolSource -> IssuanceFields
poolSourceToIssuanceField CollectedInterest = HistoryInterest
poolSourceToIssuanceField CollectedPrincipal = HistoryPrincipal
poolSourceToIssuanceField CollectedRecoveries = HistoryRecoveries
poolSourceToIssuanceField CollectedPrepayment = HistoryPrepayment
poolSourceToIssuanceField CollectedRental = HistoryRental


queryDeal :: P.Asset a => TestDeal a -> DealStats -> Balance
queryDeal t@TestDeal{accounts = accMap, bonds = bndMap, fees= feeMap} s = 
  case s of
    CurrentBondBalance ->
      Map.foldr (\x acc -> L.bndBalance x + acc) 0.0 bndMap
    OriginalBondBalance ->
      Map.foldr (\x acc -> L.originBalance (L.bndOriginInfo x) + acc) 0.0 bndMap
    CurrentPoolBalance ->
      foldl (\acc x -> acc + P.getCurrentBal x) 0.0 (P.assets (pool t)) -- `debug` ("Qurey loan level asset balance")
    CurrentPoolDefaultedBalance ->
      foldl (\acc x -> acc + P.getCurrentBal x)
            0.0 $
            filter P.isDefaulted (P.assets (pool t))

    OriginalPoolBalance ->
      case P.issuanceStat (pool t) of
        Just m -> Map.findWithDefault (-1) IssuanceBalance m -- `debug` (">>>>"++show(m))
        Nothing -> foldl (\acc x -> acc + P.getOriginBal x) 0.0 (P.assets (pool t)) 
    
    CurrentPoolBorrowerNum ->
      fromRational $ toRational $ foldl (\acc x -> acc + P.getBorrowerNum x) 0 (P.assets (pool t)) -- `debug` ("Qurey loan level asset balance")
 
    AllAccBalance -> sum $ map A.accBalance $ Map.elems accMap 
    
    AccBalance ans -> sum $ A.accBalance <$> (accMap Map.!) <$> ans
    
    LedgerBalance ans ->
      case ledgers t of 
        Nothing -> 0 
        Just ledgersM -> sum $ LD.ledgBalance . (ledgersM Map.!) <$> ans
    
    ReserveExcessAt d ans ->
      max 
        0
        $ (-) (queryDeal t (AccBalance ans)) (sum $ (calcTargetAmount t d) <$> ((accMap Map.!) <$> ans))

    ReserveAccGapAt d ans ->
      max 
        0 
        $ (-) (sum $ (calcTargetAmount t d) <$> (accMap Map.!) <$> ans ) (queryDeal t (AccBalance ans)) 

    FutureCurrentPoolBalance ->
       case P.futureCf (pool t) of 
         Nothing -> 0.0
         Just (CF.CashFlowFrame trs) -> CF.mflowBalance $ last trs
    
    FutureCurrentPoolBegBalance ->
       case P.futureCf (pool t) of 
         Nothing -> 0.0
         Just (CF.CashFlowFrame trs) -> CF.mflowBegBalance $ last trs

    -- FutureCurrentPoolBegBalance asOfDay ->
    --      case _poolSnapshot of
    --         Just ts -> CF.mflowBegBalance ts
    --         Nothing -> error $ "Pool begin balance not found at"++show asOfDay
    --     where
    --      _pool_cfs = fromMaybe (CF.CashFlowFrame []) (P.futureCf (pool t))
    --      _poolSnapshot = CF.getEarlierTsCashFlowFrame _pool_cfs asOfDay -- `debug` (">>CurrentPoolBal"++show(asOfDay)++">>Pool>>"++show(_pool_cfs))

    PoolCollectionHistory incomeType fromDay asOfDay ->
      sum fieldAmts
      where
        fieldAmts = map
                      (case incomeType of 
                        CollectedInterest -> CF.mflowInterest 
                        CollectedPrincipal -> CF.mflowPrincipal 
                        CollectedPrepayment -> CF.mflowPrepayment
                        CollectedRecoveries -> CF.mflowRecovery
                        CollectedPrepaymentPenalty -> CF.mflowPrepaymentPenalty)
                      subflow  
        subflow = case P.futureCf (pool t) of
                    Nothing ->  []
                    Just _futureCf -> 
                        if fromDay == asOfDay then 
                            CF.getTxnBetween2 _futureCf II fromDay asOfDay
                        else 
                            CF.getTxnBetween2 _futureCf EI fromDay asOfDay

    CumulativePoolDefaultedBalance ->
        let
          futureDefaults = case P.futureCf (pool t) of
                             Just (CF.CashFlowFrame _historyTxn) -> sum $ CF.tsDefaultBal <$> _historyTxn
                             Nothing -> 0.0  -- `debug` ("Geting future defaults"++show futureDefaults)
          currentDefaults = queryDeal t CurrentPoolDefaultedBalance
        in
          futureDefaults + currentDefaults

    CumulativePoolRecoveriesBalance ->
        let 
          futureRecoveries = case P.futureCf (pool t) of
                               Just (CF.CashFlowFrame _historyTxn) -> sum $ CF.mflowRecovery <$> _historyTxn
                               Nothing -> 0.0
          historyRecoveries = case P.issuanceStat (pool t) of
                                Just m -> Map.findWithDefault 0.0 HistoryRecoveries m 
                                Nothing -> 0.0
        in
          futureRecoveries + historyRecoveries
    
    PoolCumCollection ps ->
        let 
          futureVals = case P.futureCf (pool t) of
                         Just cf -> sum $ CF.sumPoolFlow cf <$> ps
                         Nothing -> 0.0

          historyVals = case P.issuanceStat (pool t) of
                                Just m -> sum [ Map.findWithDefault 0.0 (poolSourceToIssuanceField p) m | p <- ps ]
                                Nothing -> 0.0
        in 
          futureVals + historyVals

    CurrentBondBalanceOf bns -> sum $ L.bndBalance <$> (bndMap Map.!) <$> bns

    BondsIntPaidAt d bns ->
       let
          bSubMap =  getBondByName t (Just bns)   -- Map.filterWithKey (\bn b -> S.member bn bnSet) (bonds t)
          stmts = map L.bndStmt $ Map.elems bSubMap
          ex s = case s of
                   Nothing -> 0
                   Just (Statement txns) 
                     -> sum $ map getTxnAmt $
                          filter (\y -> case getTxnComment y of 
                                          (PayInt _ ) -> True
                                          _ -> False)   $
                          filter (\x -> d == getDate x) txns
       in
          sum $ map ex stmts

    BondsPrinPaidAt d bns ->
       let
          bSubMap =  getBondByName t (Just bns)   -- Map.filterWithKey (\bn b -> S.member bn bnSet) (bonds t)
          stmts = map L.bndStmt $ Map.elems bSubMap
          ex s = case s of
                   Nothing -> 0
                   Just (Statement txns) 
                     -> sum $ map getTxnAmt $
                          filter (\y -> case getTxnComment y of 
                                          (PayPrin _ ) -> True
                                          _ -> False)   $
                          filter (\x -> d == getDate x) txns
       in
          sum $ map ex stmts
    
    FeeTxnAmtBy d fns mCmt -> 
      let 
        fees = (feeMap Map.!) <$> fns -- Map.elems $ getFeeByName t (Just fns)
      in  
        case mCmt of 
          Just cmt -> sum [ queryTxnAmtAsOf fee d cmt | fee <- fees ]
          Nothing -> 
            let 
              _txn = concat [ getTxns (F.feeStmt fee) | fee <- fees ]
            in 
              sumTxn (beforeOnDate _txn d)
    
    BondTxnAmtBy d bns mCmt -> 
      let 
        bnds = (bndMap Map.!) <$> bns -- Map.elems $ getBondByName t (Just bns)
      in 
        case mCmt of
          Just cmt -> sum [ queryTxnAmtAsOf bnd d cmt | bnd <- bnds ]
          Nothing ->
            let 
              _txn = concat [ getTxns (L.bndStmt bnd) | bnd <- bnds ]
            in 
              sumTxn (beforeOnDate _txn d)

    AccTxnAmtBy d ans mCmt -> 
      let 
        accs = (accMap Map.!) <$> ans
      in 
        case mCmt of
          Just cmt -> sum [ queryTxnAmtAsOf acc d cmt | acc <- accs ]
          Nothing ->
            let 
              _txn = concat [ getTxns (A.accStmt acc) | acc <- accs ]
            in 
              sumTxn (beforeOnDate _txn d)

    BondBalanceGapAt d bName -> 
        let 
           bn@L.Bond{L.bndType = L.PAC _target} = bndMap Map.! bName
           bal = L.bndBalance bn
           targetBal = getValOnByDate _target d
        in 
           max 0 $ bal - targetBal 

    FeesPaidAt d fns ->
      let
        fSubMap = getFeeByName t (Just fns)
        stmts = map F.feeStmt $ Map.elems fSubMap
        ex s = case s of
                 Nothing -> 0
                 Just (Statement txns) -> sum $ getTxnAmt <$> filter (\x ->  d == getDate x) txns
      in
        sum $ map ex stmts

    CurrentDueBondInt bns -> sum $ L.bndDueInt <$> (bndMap Map.!) <$> bns

    CurrentDueFee fns -> sum $ F.feeDue <$> (feeMap Map.!) <$> fns

    LiqCredit lqNames -> 
      case liqProvider t of
        Nothing -> 0
        Just liqProviderM -> sum $ [ fromMaybe 0 (CE.liqCredit liq) | (k,liq) <- Map.assocs liqProviderM
                                     , S.member k (S.fromList lqNames) ]
    LiqBalance lqNames -> 
      case liqProvider t of
        Nothing -> 0
        Just liqProviderM -> sum $ [ CE.liqBalance liq | (k,liq) <- Map.assocs liqProviderM
                                     , S.member k (S.fromList lqNames) ]

    Sum _s -> sum $ map (queryDeal t) _s

    Substract (ds:dss) -> 
        let 
          a  = queryDeal t ds 
          bs = queryDeal t (Sum dss) 
        in 
          a - bs 

    Constant n -> fromRational n

    Max ss -> maximum' [ queryDeal t s | s <- ss ]
    Min ss -> minimum' [ queryDeal t s | s <- ss ]

    Divide ds1 ds2 -> queryDeal t ds1 / queryDeal t ds2

    CustomData s d ->
        case custom t of 
          Nothing -> 0 
          Just mCustom ->
              case mCustom Map.! s of 
                CustomConstant v -> fromRational v 
                CustomCurve cv -> getValOnByDate cv d
                CustomDS ds -> queryDeal t (patchDateToStats d ds )

    FloorAndCap floor cap s -> max (queryDeal t floor) $ min (queryDeal t cap) (queryDeal t s)
    
    Factor s f -> mulBR (queryDeal t s) f

    FloorWith s floor -> max (queryDeal t s) (queryDeal t floor)
    FloorWithZero s -> max (queryDeal t s) 0
    CapWith s cap -> min (queryDeal t s) (queryDeal t cap)
    Round ds rb -> roundingBy rb (queryDeal t ds)
    
    _ -> error ("Failed to query balance of -> "++ show s)

queryDealBool :: P.Asset a => TestDeal a -> DealStats -> Bool
queryDealBool t@TestDeal{triggers= trgs,bonds = bndMap} ds = 
  case ds of 
    TriggersStatusAt dealcycle idx -> 
      case trgs of 
        Just _trgs -> Trg.trgStatus $ (_trgs Map.! dealcycle) !! idx
        Nothing -> error "no trigger for this deal"
    IsMostSenior bn bns ->
      let 
        bn1:bns1 =  (bndMap Map.!) <$> (bn:bns)
      in
        case (L.isPaidOff bn1,all L.isPaidOff bns1) of
          (False,True) -> True
          _ -> False

    _ -> error ("Failed to query bool type formula"++ show ds)

testPre :: P.Asset a => Date -> TestDeal a -> Pre -> Bool
testPre d t p =
  case p of
    Types.All pds -> all (testPre d t) pds 
    Types.Any pds -> any (testPre d t) pds 
    IfZero s -> queryDeal t s == 0.0 -- `debug` ("S->"++show(s)++">>"++show((queryDeal t s)))
    
    If cmp s amt -> (toCmp cmp) (queryDeal t (ps s))  amt
    IfRate cmp s amt -> (toCmp cmp) (queryDealRate t (ps s)) amt
    IfInt cmp s amt -> (toCmp cmp) (queryDealInt t (ps s) d) amt
    IfDate cmp _d -> (toCmp cmp) d _d
    IfCurve cmp s _ts -> (toCmp cmp) (queryDeal t (ps s)) (fromRational (getValByDate _ts Inc d))
    IfRateCurve cmp s _ts -> (toCmp cmp) (queryDealRate t (ps s)) (fromRational (getValByDate _ts Inc d))
    IfBool s True -> queryDealBool t s
    IfBool s False -> not (queryDealBool t s)
    If2 cmp s1 s2 -> (toCmp cmp) (queryDeal t (ps s1)) (queryDeal t (ps s2))
    IfRate2 cmp s1 s2 -> (toCmp cmp) (queryDealRate t (ps s1)) (queryDealRate t (ps s2))
    IfInt2 cmp s1 s2 -> (toCmp cmp) (queryDealInt t (ps s1) d) (queryDealInt t (ps s2) d)
    IfDealStatus st -> status t == st   --  `debug` ("current date"++show d++">> stutus"++show (status t )++"=="++show st)
    Always b -> b
    where 
      toCmp x = case x of 
                  G -> (>)
                  GE -> (>=)
                  L -> (<)
                  LE -> (<=)
                  E -> (==)
      ps = patchDateToStats d