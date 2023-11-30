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
import qualified Hedge as H
import Stmt
import Util
import DateUtil
import Lib
import Control.Lens hiding (element)
import Control.Lens.TH
import Debug.Trace
import Cashflow (CashFlowFrame(CashFlowFrame))
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
         PoolFactor -> FutureCurrentPoolFactor d
         LastBondIntPaid bns -> BondsIntPaidAt d bns
         LastFeePaid fns -> FeesPaidAt d fns
         LastBondPrinPaid bns -> BondsPrinPaidAt d bns
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
      
      CumulativeNetLossRatio ->
        toRational $ (queryDeal t CumulativeNetLoss) / (queryDeal t OriginalPoolBalance)

      CumulativePoolDefaultedRateTill idx -> 
        let 
          originPoolBal = toRational (queryDeal t OriginalPoolBalance) -- `debug` ("A")-- `debug` (">>Pool Bal"++show (queryDeal t OriginalPoolBalance))
          cumuPoolDefBal = toRational (queryDeal t (PoolCumCollectionTill idx [NewDefaults])) -- `debug` ("B") -- `debug` (">>CUMU"++show (queryDeal t CumulativePoolDefaultedBalance))
        in 
          cumuPoolDefBal / originPoolBal -- `debug` ("cumulative p def rate"++show cumuPoolDefBal++">>"++show originPoolBal)
        

      BondRate bn -> 
        toRational $ L.bndRate $ bonds t Map.! bn
      
      BondWaRate bns -> 
        let 
          rs = toRational <$> (\bn -> queryDealRate t (BondRate bn)) <$> bns
          ws = toRational <$> (\bn -> queryDeal t (CurrentBondBalanceOf [bn])) <$> bns
        in 
          toRational $ sum (zipWith (+) ws rs) / sum ws

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
      Avg ss -> toRational (queryDealRate t (Sum ss)) / (toRational (length ss))

      FloorAndCap floor cap s ->  
        let 
          [_f,_c,_s] = toRational <$> (queryDealRate t) <$> [floor,cap,s]
        in 
          max _f (min _c _s)
      FloorWith s floor -> toRational $ max (queryDealRate t s) (queryDealRate t floor)
      FloorWithZero s -> toRational $ max (queryDealRate t s) 0
      CapWith s cap -> toRational $ min (queryDealRate t s) (queryDealRate t cap)

queryDealInt :: P.Asset a => TestDeal a -> DealStats -> Date -> Int 
queryDealInt t@TestDeal{ pool = p ,bonds = bndMap } s d = 
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
            (L.Bond _ _ (L.OriginalInfo _ _ _ mm) _ _ _ _ _ _ _ _ _ _) = bndMap Map.! bn  

    ProjCollectPeriodNum -> length $ maybe [] CF.getTsCashFlowFrame $ view P.poolFutureCf p -- `debug` ("Hit query")

    FloorAndCap floor cap s -> max (queryDealInt t floor d) $ min (queryDealInt t cap d ) (queryDealInt t s d)
    FloorWith s floor -> max (queryDealInt t s d) (queryDealInt t floor d)
    FloorWithZero s -> max (queryDealInt t s d) 0
    CapWith s cap -> min (queryDealInt t s d) (queryDealInt t cap d)

    Max ss -> maximum' $ [ queryDealInt t s d | s <- ss ]
    Min ss -> minimum' $ [ queryDealInt t s d | s <- ss ]

poolSourceToIssuanceField :: PoolSource -> CutoffFields
poolSourceToIssuanceField CollectedInterest = HistoryInterest
poolSourceToIssuanceField CollectedPrincipal = HistoryPrincipal
poolSourceToIssuanceField CollectedRecoveries = HistoryRecoveries
poolSourceToIssuanceField CollectedPrepayment = HistoryPrepayment
poolSourceToIssuanceField CollectedRental = HistoryRental
poolSourceToIssuanceField CollectedCash = HistoryCash
poolSourceToIssuanceField a = error ("Failed to match pool source when mapping to issuance field"++show a)


queryDeal :: P.Asset a => TestDeal a -> DealStats -> Balance
queryDeal t@TestDeal{accounts=accMap, bonds=bndMap, fees=feeMap, ledgers=ledgerM, pool=poolM} s = 
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
        -- use issuance balance from map if the map exists
        Just m -> 
          case Map.lookup IssuanceBalance m of 
            Just v -> v
            Nothing -> error "No issuance balance found in the pool, pls specify it in the pool stats map `issuanceStat`"
        Nothing -> error ("No stat found in the pool, pls specify it in the pool stats map `issuanceStat` Deal:" ++ show (name t))
    
    CurrentPoolBorrowerNum ->
      fromRational $ toRational $ foldl (\acc x -> acc + P.getBorrowerNum x) 0 (P.assets (pool t)) -- `debug` ("Qurey loan level asset balance")
 
    AllAccBalance -> sum $ map A.accBalance $ Map.elems accMap 
    
    AccBalance ans -> sum $ A.accBalance . (accMap Map.!) <$> ans
    
    LedgerBalance ans ->
      case ledgerM of 
        Nothing -> error ("No ledgers were modeled , failed to find ledger:"++show ans )
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
                    -- Just _futureCf -> 
                    Just (CashFlowFrame trs) -> 
                        if fromDay == asOfDay then 
                          sliceBy II fromDay asOfDay trs
                        else 
                          sliceBy EI fromDay asOfDay trs
                    _ -> []

    CumulativePoolDefaultedBalance ->
        let
          futureDefaults = case P.futureCf poolM of
                             Just (CF.CashFlowFrame historyTxn) -> CF.tsCumDefaultBal $ last historyTxn
                             Nothing -> 0.0  -- `debug` ("Geting future defaults"++show futureDefaults)

          historyDefaults = case P.issuanceStat poolM of
                                Just m -> Map.findWithDefault 0.0 HistoryDefaults m 
                                Nothing -> 0.0
        in
          futureDefaults + historyDefaults -- `debug` ("history defaults"++ show historyDefaults)

    CumulativePoolRecoveriesBalance ->
        let 
          futureRecoveries = case P.futureCf poolM of
                               Just (CF.CashFlowFrame historyTxn) -> CF.tsCumRecoveriesBal $ last historyTxn
                               Nothing -> 0.0
          historyRecoveries = case P.issuanceStat poolM of
                                Just m -> Map.findWithDefault 0.0 HistoryRecoveries m 
                                Nothing -> 0.0
        in
          futureRecoveries + historyRecoveries
    
    CumulativeNetLoss ->
         queryDeal t CumulativePoolDefaultedBalance - queryDeal t CumulativePoolRecoveriesBalance
    
    PoolCumCollection ps ->
        let 
          futureVals = case P.futureCf poolM of
                         Just cf -> sum $ CF.sumPoolFlow cf <$> ps
                         Nothing -> 0.0

          historyVals = case P.issuanceStat poolM of
                                Just m -> sum [ Map.findWithDefault 0.0 (poolSourceToIssuanceField p) m | p <- ps ]
                                Nothing -> 0.0
        in 
          futureVals + historyVals
    
    PoolCumCollectionTill idx ps -> 
        let 
          futureVals = case P.futureCf poolM of
                         Just (CashFlowFrame _trs) -> 
                          let 
                            lengthTrs = length _trs
                            trs = slice 0 (max 0 (lengthTrs + idx)) _trs
                          in 
                            sum $ (CF.lookupSource <$> trs) <*> ps
                         Nothing -> 0.0

          historyVals = case P.issuanceStat poolM of
                                Just m -> sum [ Map.findWithDefault 0.0 (poolSourceToIssuanceField p) m | p <- ps ]
                                Nothing -> 0.0
        in 
          futureVals + historyVals
 
    PoolCurCollection ps ->
      case P.futureCf poolM of
            Just (CF.CashFlowFrame trs) -> sum $ CF.lookupSource (last trs) <$> ps
            Nothing -> 0.0

    PoolCollectionStats idx ps -> 
      case P.futureCf poolM of
            Just (CF.CashFlowFrame trs) 
              ->let
                  theCollection = trs!!((length trs) + idx)
                in  
                  sum $ CF.lookupSource theCollection <$> ps
            Nothing -> 0.0


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
              sumTxn $ cutBy Inc Past d _txn 
    
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
              sumTxn $ cutBy Inc Past d _txn

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
              sumTxn $ cutBy Inc Past d _txn 

    LedgerTxnAmt lns mCmt ->
      case ledgerM of 
        Nothing -> error ("No ledgers were modeled , failed to find ledger:"++show lns )
        Just ledgerm ->
          let 
            lgs = (ledgerm Map.!) <$> lns
          in
            case mCmt of
              Just cmt -> sum [ queryTxnAmt lg cmt | lg <- lgs ]
              Nothing -> sum [ LD.ledgBalance lg | lg <- lgs ]

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

    CurrentDueBondInt bns -> 
      sum $ L.bndDueInt <$> (bndMap Map.!) <$> bns -- `debug` ("bond due int" ++ show ((bndMap Map.!) <$> bns ))

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

    RateCapNet rcName -> case rateCap t of
                           Nothing -> error "No rate cap in the deal"
                           Just rm -> case Map.lookup rcName rm of
                                        Nothing -> error $ "No "++ rcName ++" Found in rate cap map with key"++ show (Map.keys rm)
                                        Just rc -> H.rcNetCash rc
    
    RateSwapNet rsName -> case rateCap t of
                           Nothing -> error "No rate swap in the deal"
                           Just rm -> case Map.lookup rsName rm of
                                        Nothing -> error $ "No "++ rsName ++" Found in rate swap map with key"++ show (Map.keys rm)
                                        Just rc -> H.rcNetCash rc

    Sum _s -> sum $ map (queryDeal t) _s

    Subtract (ds:dss) -> 
        let 
          a  = queryDeal t ds 
          bs = queryDeal t (Sum dss) 
        in 
          a - bs
          
    Substract s -> queryDeal t (Subtract s)
    
    Avg dss ->  divideBI (sum ( queryDeal t <$> dss ))  (length dss)

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
    Abs s -> abs $ queryDeal t s
    Round ds rb -> roundingBy rb (queryDeal t ds)
    
    _ -> error ("Failed to query balance of -> "++ show s)

queryDealBool :: P.Asset a => TestDeal a -> DealStats -> Bool
queryDealBool t@TestDeal{triggers= trgs,bonds = bndMap} ds = 
  case ds of 
    TriggersStatus dealcycle tName -> 
      case trgs of 
        Just _trgsM -> case Map.lookup dealcycle _trgsM of 
                         Nothing -> error ("no trigger cycle for this deal" ++ show dealcycle)
                         Just triggerMatCycle -> 
                           case Map.lookup tName triggerMatCycle of 
                             Nothing -> error ("no trigger for this deal" ++ show tName ++ " in cycle " ++ show triggerMatCycle)
                             Just trigger -> Trg.trgStatus trigger 
        Nothing -> error "no trigger for this deal"
    
    IsMostSenior bn bns ->
      let 
        bn1:bns1 =  (bndMap Map.!) <$> (bn:bns)
      in
        case (isPaidOff bn1,all isPaidOff bns1) of
          (False,True) -> True
          _ -> False

    IsPaidOff bns -> all isPaidOff $ (bndMap Map.!) <$> bns
    
    TestRate ds cmp r -> let
                           testRate = queryDealRate t ds
                         in  
                           case cmp of 
                             G ->  testRate > r
                             GE -> testRate >= r
                             L ->  testRate < r
                             LE -> testRate <= r
                             E ->  testRate == r
    
    IsDealStatus st -> status t == st

    TestAny b dss -> b `elem` [ queryDealBool t ds | ds <- dss ]
    TestAll b dss -> all (== b) [ queryDealBool t ds | ds <- dss ] 

    _ -> error ("Failed to query bool type formula"++ show ds)

testPre :: P.Asset a => Date -> TestDeal a -> Pre -> Bool
testPre d t p =
  case p of
    Types.All pds -> all (testPre d t) pds 
    Types.Any pds -> any (testPre d t) pds 
    IfZero s -> queryDeal t s == 0.0 -- `debug` ("S->"++show(s)++">>"++show((queryDeal t s)))
    
    If cmp s amt -> toCmp cmp (queryDeal t (ps s))  amt
    IfRate cmp s amt -> toCmp cmp (queryDealRate t (ps s)) amt
    IfInt cmp s amt -> toCmp cmp (queryDealInt t (ps s) d) amt
    IfDate cmp _d -> toCmp cmp d _d
    IfCurve cmp s _ts -> toCmp cmp (queryDeal t (ps s)) (fromRational (getValByDate _ts Inc d))
    IfRateCurve cmp s _ts -> toCmp cmp (queryDealRate t (ps s)) (fromRational (getValByDate _ts Inc d))
    IfBool s True -> queryDealBool t s
    IfBool s False -> not (queryDealBool t s)
    If2 cmp s1 s2 -> toCmp cmp (queryDeal t (ps s1)) (queryDeal t (ps s2))
    IfRate2 cmp s1 s2 -> toCmp cmp (queryDealRate t (ps s1)) (queryDealRate t (ps s2))
    IfInt2 cmp s1 s2 -> toCmp cmp (queryDealInt t (ps s1) d) (queryDealInt t (ps s2) d)
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
