{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Deal.DealQuery (queryDealBool,queryDeal,queryDealInt,queryDealRate
                       ,patchDateToStats,patchDatesToStats,testPre, calcTargetAmount, testPre2) 
  where

import Deal.DealBase
import Types
import qualified Asset as P
import Data.List
import Data.Fixed
import Data.Maybe
import Data.Text (replace, pack, unpack)
import Numeric.Limits
import GHC.Real
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
import qualified Analytics as A
import Stmt
import Util
import DateUtil
import Control.Lens hiding (element)
import Control.Lens.TH
import Debug.Trace
import Lib
import Cashflow (CashFlowFrame(CashFlowFrame))
import qualified Cashflow as P
import qualified Util as CF
debug = flip trace

-- | calcuate target balance for a reserve account, 0 for a non-reserve account
calcTargetAmount :: P.Asset a => TestDeal a -> Date -> A.Account -> Balance
calcTargetAmount t d (A.Account _ _ _ Nothing _ ) = 0
calcTargetAmount t d (A.Account _ _ _ (Just r) _ ) =
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
         CurrentPoolBalance mPns -> FutureCurrentPoolBalance mPns
         CurrentPoolBegBalance mPns -> FutureCurrentPoolBegBalance mPns
         PoolFactor mPns -> FutureCurrentPoolFactor d mPns
         LastBondIntPaid bns -> BondsIntPaidAt d bns
         LastFeePaid fns -> FeesPaidAt d fns
         LastBondPrinPaid bns -> BondsPrinPaidAt d bns
         BondBalanceGap bn -> BondBalanceGapAt d bn
         ReserveAccGap ans -> ReserveAccGapAt d ans
         ReserveExcess ans -> ReserveExcessAt d ans
         Sum _ds -> Sum $ map (patchDateToStats d) _ds
         Substract _ds -> Substract $ map (patchDateToStats d) _ds
         Subtract _ds -> Subtract $ map (patchDateToStats d) _ds
         Min dss -> Min $ [ patchDateToStats d ds | ds <- dss ] 
         Max dss -> Max $ [ patchDateToStats d ds | ds <- dss ]
         Factor _ds r -> Factor (patchDateToStats d _ds) r
         FloorWithZero ds -> FloorWithZero (patchDateToStats d ds) 
         UseCustomData n -> CustomData n d
         CurrentPoolBorrowerNum mPns -> FutureCurrentPoolBorrowerNum d mPns
         FeeTxnAmt ns mCmt -> FeeTxnAmtBy d ns mCmt
         BondTxnAmt ns mCmt -> BondTxnAmtBy d ns mCmt
         AccTxnAmt ns mCmt -> AccTxnAmtBy d ns mCmt -- `debug` ("Hitttt")
         PoolScheduleCfPv pm pns -> FuturePoolScheduleCfPv d pm pns
         Excess dss -> Excess $ [ patchDateToStats d ds | ds <- dss ]
         Abs ds -> Abs $ patchDateToStats d ds
         Avg dss -> Avg $ [ patchDateToStats d ds | ds <- dss ]
         Divide ds1 ds2 -> Divide (patchDateToStats d ds1) (patchDateToStats d ds2)
         FloorAndCap f c s -> FloorAndCap (patchDateToStats d f) (patchDateToStats d c) (patchDateToStats d s)
         Multiply dss -> Multiply $ [ patchDateToStats d ds | ds <- dss ]
         FloorWith ds f -> FloorWith (patchDateToStats d ds) (patchDateToStats d f)
         CapWith ds c -> CapWith (patchDateToStats d ds) (patchDateToStats d c)
         Round ds rb -> Round (patchDateToStats d ds) rb
         DivideRatio ds1 ds2 -> DivideRatio (patchDateToStats d ds1) (patchDateToStats d ds2)
         AvgRatio ss -> AvgRatio $ [ patchDateToStats d ds | ds <- ss ]
         _ -> t -- `debug` ("Failed to patch date to stats"++show t)

patchDatesToStats :: P.Asset a => TestDeal a -> Date -> Date -> DealStats -> DealStats
patchDatesToStats t d1 d2 ds 
  = case ds of 
      CurrentBondBalanceOf bns -> WeightedAvgCurrentBondBalance d1 d2 bns
      OriginalBondBalanceOf bns -> WeightedAvgOriginalBondBalance d1 d2 bns
      CurrentPoolBalance mPns -> WeightedAvgCurrentPoolBalance d1 d2 mPns
      OriginalPoolBalance mPns -> WeightedAvgOriginalPoolBalance d1 d2 mPns
      CurrentBondBalance -> WeightedAvgCurrentBondBalance d1 d2 (Map.keys $ bonds t)
      OriginalBondBalance -> WeightedAvgOriginalBondBalance d1 d2 (Map.keys $ bonds t)
      Excess dss -> Excess $ [ patchDatesToStats t d1 d2 ds | ds <- dss ]
      Abs ds -> Abs $ patchDatesToStats t d1 d2 ds
      Avg dss -> Avg $ [ patchDatesToStats t d1 d2 ds | ds <- dss ]
      Divide ds1 ds2 -> Divide (patchDatesToStats t d1 d2 ds1) (patchDatesToStats t d1 d2 ds2)
      FloorAndCap f c s -> FloorAndCap (patchDatesToStats t d1 d2 f) (patchDatesToStats t d1 d2 c) (patchDatesToStats t d1 d2 s)
      Multiply dss -> Multiply $ [ patchDatesToStats t d1 d2 ds | ds <- dss ]
      FloorWith ds f -> FloorWith (patchDatesToStats t d1 d2 ds) (patchDatesToStats t d1 d2 f)
      CapWith ds c -> CapWith (patchDatesToStats t d1 d2 ds) (patchDatesToStats t d1 d2 c)
      Round ds rb -> Round (patchDatesToStats t d1 d2 ds) rb
      Sum dss -> Sum $ [ patchDatesToStats t d1 d2 ds | ds <- dss ]
      DivideRatio ds1 ds2 -> DivideRatio (patchDatesToStats t d1 d2 ds1) (patchDatesToStats t d1 d2 ds2)
      AvgRatio ss -> AvgRatio $ [ patchDatesToStats t d1 d2 ds | ds <- ss ]
      x -> x


queryDealRate :: P.Asset a => TestDeal a -> DealStats -> Micro
queryDealRate t s =
  fromRational $ 
    case s of
      BondFactor ->
        toRational (queryDeal t CurrentBondBalance) / toRational (queryDeal t OriginalBondBalance)

      PoolFactor mPns ->
        toRational (queryDeal t (CurrentPoolBalance mPns))  / toRational (queryDeal t (OriginalPoolBalance mPns))

      FutureCurrentPoolFactor asOfDay mPns ->
        toRational (queryDeal t (FutureCurrentPoolBalance mPns)) / toRational (queryDeal t (OriginalPoolBalance mPns))
      
      CumulativePoolDefaultedRate mPns ->
        let 
          originPoolBal = toRational $ queryDeal t (OriginalPoolBalance mPns) -- `debug` ("A")-- `debug` (">>Pool Bal"++show (queryDeal t OriginalPoolBalance))
          cumuPoolDefBal = toRational $ queryDeal t (CumulativePoolDefaultedBalance mPns) -- `debug` ("B") -- `debug` (">>CUMU"++show (queryDeal t CumulativePoolDefaultedBalance))
        in 
          cumuPoolDefBal / originPoolBal  -- `debug` ("cumulative p def rate"++show cumuPoolDefBal++">>"++show originPoolBal)
      
      CumulativeNetLossRatio mPns ->
        toRational $ queryDeal t (CumulativeNetLoss mPns) / queryDeal t (OriginalPoolBalance mPns)

      CumulativePoolDefaultedRateTill idx mPns -> 
        let 
          originPoolBal = toRational (queryDeal t (OriginalPoolBalance mPns)) -- `debug` ("A")-- `debug` (">>Pool Bal"++show (queryDeal t OriginalPoolBalance))
          cumuPoolDefBal = toRational (queryDeal t (PoolCumCollectionTill idx [NewDefaults] mPns)) -- `debug` ("B") -- `debug` (">>CUMU"++show (queryDeal t CumulativePoolDefaultedBalance))
        in 
          cumuPoolDefBal / originPoolBal -- `debug` (show idx ++" cumulative p def rate"++show cumuPoolDefBal++">>"++show originPoolBal)
        

      BondRate bn -> case Map.lookup bn (bonds t) of 
                      Just b@(L.Bond {}) -> toRational $ L.bndRate b 
                      Just b@(L.BondGroup bSubMap) -> 
                        let 
                          bnds = Map.elems bSubMap
                          rates = toRational <$> L.bndRate <$> bnds
                          bals = L.getCurBalance <$> bnds
                        in 
                          weightedBy bals rates
                      Nothing -> 
                        case viewDealBondsByNames t [bn] of 
                          [b] -> toRational $ L.bndRate b
                          _ -> error ("Failed to find bond by name"++bn)
      
      BondWaRate bns -> 
        let 
          rs = toRational <$> (\bn -> queryDealRate t (BondRate bn)) <$> bns
          ws = (\bn -> queryDeal t (CurrentBondBalanceOf [bn])) <$> bns
        in 
          -- toRational $ safeDivide $ sum (zipWith (*) ws rs) $ sum ws
          weightedBy ws rs

      PoolWaRate mPns -> 
        let 
          latestCfs = filter isJust $ Map.elems $ getLatestCollectFrame t mPns
          rates = toRational <$> maybe 0.0 CF.mflowRate  <$> latestCfs
          bals = maybe 0.0 CF.mflowBalance  <$> latestCfs
        in 
          weightedBy bals rates

      Constant r -> r
      -- DivideRatio ds1 ds2 ->  toRational (queryDeal t ds1) / toRational (queryDeal t ds2)
      DivideRatio ds1 ds2 -> if (queryDeal t ds2) == 0 then 
                              toRational Numeric.Limits.infinity
                            else
                              (toRational (queryDeal t ds1)) / (toRational (queryDeal t ds2)) 
      AvgRatio ss -> toRational (queryDealRate t (Sum ss)) / toRational (length ss)
      Max ss -> toRational $ maximum' [ queryDealRate t s | s <- ss ]
      Min ss -> toRational $ minimum' [ queryDealRate t s | s <- ss ]
      Subtract (s1:ss) -> toRational $ (queryDealRate t s1) - queryDealRate t (Sum ss)
      Substract (s1:ss) -> toRational $ (queryDealRate t s1) - queryDealRate t (Sum ss)
      Sum ss -> toRational $ sum $ (queryDealRate t) <$> ss  
      Avg ss -> toRational (queryDealRate t (Sum ss)) / (toRational (length ss))

      FloorAndCap floor cap s ->  
        let 
          [_f,_c,_s] = toRational <$> (queryDealRate t) <$> [floor,cap,s]
        in 
          max _f (min _c _s)
      Factor s r -> toRational $ (queryDealRate t s) * fromRational r
      Multiply ss -> toRational $ product (queryDealRate t <$> ss)
      FloorWith s floor -> toRational $ max (queryDealRate t s) (queryDealRate t floor)
      FloorWithZero s -> toRational $ max (queryDealRate t s) 0
      Excess (s1:ss) -> toRational $ max 0 $ queryDealRate t s1 - queryDealRate t (Sum ss) -- `debug` ("Excess"++show (queryDeal t s1)++"ss"++show ( queryDeal t (Sum ss)))
      CapWith s cap -> toRational $ min (queryDealRate t s) (queryDealRate t cap)
      Abs s -> toRational . abs $ queryDealRate t s
      

queryDealInt :: P.Asset a => TestDeal a -> DealStats -> Date -> Int 
queryDealInt t@TestDeal{ pool = p ,bonds = bndMap } s d = 
  case s of 
    FutureCurrentPoolBorrowerNum d mPns ->   --TODO may use date as cutoff date 
      let 
        poolCfs = Map.elems $ getLatestCollectFrame t mPns
        poolBn =  maybe 0 (\x -> fromMaybe 0 (CF.mflowBorrowerNum x))   <$> poolCfs
      in 
        sum poolBn

    CurrentPoolBorrowerNum mPns ->
      let 
        assetM = getAllAssetList t 
      in 
        sum $ P.getBorrowerNum <$> assetM -- `debug` ("Qurey loan level asset balance"        

    MonthsTillMaturity bn -> 
        case mm of 
          Nothing -> error "Should not happend"
          Just md -> fromInteger $ T.cdMonths $ T.diffGregorianDurationClip md d
        where
            (L.Bond _ _ (L.OriginalInfo _ _ _ mm) _ _ _ _ _ _ _ _ _ _ _) = bndMap Map.! bn  

    ProjCollectPeriodNum -> maximum' $ Map.elems $ Map.map (maybe 0 CF.sizeCashFlowFrame) $ getAllCollectedFrame t Nothing

    FloorAndCap floor cap s -> max (queryDealInt t floor d) $ min (queryDealInt t cap d ) (queryDealInt t s d)
    FloorWith s floor -> max (queryDealInt t s d) (queryDealInt t floor d)
    FloorWithZero s -> max (queryDealInt t s d) 0
    CapWith s cap -> min (queryDealInt t s d) (queryDealInt t cap d)
    Max ss -> maximum' $ [ queryDealInt t s d | s <- ss ]
    Min ss -> minimum' $ [ queryDealInt t s d | s <- ss ]

-- ^ map from Pool Source to Pool CutoffFields in Pool Map
poolSourceToIssuanceField :: PoolSource -> CutoffFields
poolSourceToIssuanceField CollectedInterest = HistoryInterest
poolSourceToIssuanceField CollectedPrincipal = HistoryPrincipal
poolSourceToIssuanceField CollectedRecoveries = HistoryRecoveries
poolSourceToIssuanceField CollectedPrepayment = HistoryPrepayment
poolSourceToIssuanceField CollectedRental = HistoryRental
poolSourceToIssuanceField CollectedCash = HistoryCash
poolSourceToIssuanceField NewLosses = HistoryLoss
poolSourceToIssuanceField NewDefaults = HistoryDefaults
poolSourceToIssuanceField a = error ("Failed to match pool source when mapping to issuance field"++show a)


queryDeal :: P.Asset a => TestDeal a -> DealStats -> Balance
queryDeal t@TestDeal{accounts=accMap, bonds=bndMap, fees=feeMap, ledgers=ledgerM, pool=pt } s = 
  case s of
    CurrentBondBalance -> Map.foldr (\x acc -> getCurBalance x + acc) 0.0 bndMap
    
    OriginalBondBalance -> Map.foldr (\x acc -> getOriginBalance x + acc) 0.0 bndMap
    
    BondDuePrin bnds -> sum $ L.bndDuePrin <$> viewDealBondsByNames t bnds
    
    OriginalBondBalanceOf bnds -> sum $ getOriginBalance <$> viewDealBondsByNames t bnds

    CurrentBondBalanceOf bns -> sum $ getCurBalance <$> viewDealBondsByNames t bns
    
    CurrentPoolBalance mPns ->
      foldl (\acc x -> acc + P.getCurrentBal x) 0.0 (getAllAssetList t) --TODO TOBE FIX: mPns is not used
    
    CurrentPoolDefaultedBalance ->
      foldl (\acc x -> acc + P.getCurrentBal x)
            0.0 $
            filter P.isDefaulted (getAllAssetList t)

    DealIssuanceBalance mPns -> 
      sum $ Map.findWithDefault 0.0 IssuanceBalance <$> Map.elems (getIssuanceStats t mPns)

    OriginalPoolBalance mPns -> 
      let 
        statsConsol = getIssuanceStatsConsol t mPns 
      in 
        case Map.lookup IssuanceBalance statsConsol of 
          Just v -> v
          Nothing -> error "No issuance balance found in the pool, pls specify it in the pool stats map `issuanceStat`"
    
    UnderlyingBondBalance mBndNames -> 0
 
    AllAccBalance -> sum $ map A.accBalance $ Map.elems accMap 
    
    AccBalance ans -> sum $ A.accBalance . (accMap Map.!) <$> ans
    
    LedgerBalance ans ->
      case ledgerM of 
        Nothing -> error ("No ledgers were modeled , failed to find ledger:"++show ans )
        Just ledgersM -> sum $ LD.ledgBalance . (ledgersM Map.!) <$> ans
    
    ReserveExcessAt d ans ->
      max 
        0
        $ (-) (queryDeal t (AccBalance ans)) (sum $ calcTargetAmount t d <$> ((accMap Map.!) <$> ans))

    ReserveAccGapAt d ans ->
      max 
        0 
        $ (-) (sum $ calcTargetAmount t d . (accMap Map.!) <$> ans ) (queryDeal t (AccBalance ans)) 

    FutureCurrentPoolBalance mPns ->
      let 
        ltc = getLatestCollectFrame t mPns
      in 
        sum $ maybe 0 CF.mflowBalance <$> ltc 

    FutureCurrentSchedulePoolBalance mPns ->
      let 
        scheduleFlowM = Map.elems $ view dealScheduledCashflow t
      in 
        sum $ maybe 0 (CF.mflowBalance . head . view CF.cashflowTxn) <$> scheduleFlowM
    
    FutureCurrentSchedulePoolBegBalance mPns ->
      let 
        scheduleFlowM = Map.elems $ view dealScheduledCashflow t
      in 
        sum $ maybe 0 (CF.mflowBegBalance . head . view CF.cashflowTxn) <$> scheduleFlowM
    
    FutureCurrentPoolBegBalance mPns ->
      let 
        ltc = getLatestCollectFrame t mPns
      in 
        sum $ maybe 0 CF.mflowBegBalance <$> ltc 

    PoolCollectionHistory incomeType fromDay asOfDay mPns ->
      sum fieldAmts
      where
        mTxns = Map.elems $ getAllCollectedTxns t mPns
        subflow = sliceBy EI fromDay asOfDay $ concat $ fromMaybe [] <$> mTxns
        fieldAmts = map (`CF.lookupSource` incomeType) subflow  

    CumulativePoolDefaultedBalance mPns ->
        let
          latestCollect = getLatestCollectFrame t mPns
          futureDefaults = sum $ Map.elems $ Map.map (maybe 0 (fromMaybe 0 . CF.tsCumDefaultBal )) $ latestCollect 
        in
          futureDefaults -- `debug` ("future Defaults"++ show futureDefaults++ show latestCollect)

    CumulativePoolRecoveriesBalance mPns ->
        let
          latestCollect = getLatestCollectFrame t mPns
          futureRecoveries = sum $ Map.elems $ Map.map (maybe 0 (fromMaybe 0 . CF.tsCumRecoveriesBal)) $ latestCollect 
        in
          futureRecoveries
    
    CumulativeNetLoss mPns ->
         queryDeal t (CumulativePoolDefaultedBalance mPns) - queryDeal t (CumulativePoolRecoveriesBalance mPns)
    
    PoolCumCollection ps mPns ->
        let 
          collectedTxns = concat . Map.elems $ Map.map (fromMaybe []) $ getAllCollectedTxns t mPns
          --xxx = sum (  <$> ps ) <*> collectedFrames
          --mBals = (\cf -> sum (fromMaybe [] cf) <$> ps ) <$> collectedFrames
          futureVals = sum $ (CF.lookupSource <$> collectedTxns) <*> ps
          
          poolStats = Map.elems $ getIssuanceStats t mPns
          historyVals = sum $ (Map.findWithDefault 0.0 . poolSourceToIssuanceField <$> ps) <*> poolStats
          -- historyVals = case P.issuanceStat poolM of
          --                       Just m -> sum [ Map.findWithDefault 0.0 (poolSourceToIssuanceField p) m | p <- ps ]
          --                       Nothing -> 0.0
        in 
          futureVals + historyVals
    
    PoolCumCollectionTill idx ps mPns -> 
        let 
          txnMap = Map.map (dropLastN (negate idx) . fromMaybe []) $ getAllCollectedTxns t mPns 
          txnList = concat $ Map.elems txnMap 
          lookupList = CF.lookupSource <$> txnList
          futureVals = sum $ lookupList <*> ps
          sumMap = getIssuanceStatsConsol t mPns
          historyVals = sum $ Map.findWithDefault 0 . poolSourceToIssuanceField <$> ps <*> [sumMap]
        in 
          futureVals + historyVals
 
    PoolCurCollection ps mPns ->
      let 
        pCf = getLatestCollectFrame t mPns -- `debug` ("mPns"++ show mPns)
        lastRows = Map.map (maybe 0 (\r -> sum (CF.lookupSource r <$> ps))) pCf -- `debug` ("Latest collect frame"++ show pCf)
      in 
        sum $ Map.elems lastRows -- `debug` ("lst row found"++ show lastRows)

    PoolCollectionStats idx ps mPns -> 
      let 
        pTxns::[[CF.TsRow]] = fromMaybe [] <$> (view CF.cashflowTxn <$>) <$> Map.elems (getAllCollectedFrame t mPns)
        
        pRows::[Maybe CF.TsRow] 
          = (\x -> let 
                      lookupIndx = length x + idx - 1
                   in 
                      if (( lookupIndx >= length x )||  (lookupIndx <0)) then 
                        Nothing
                      else
                        Just (x!!lookupIndx)) <$>  pTxns
      in 
        sum $ CF.lookupSourceM <$> pRows <*> ps

    FuturePoolScheduleCfPv asOfDay pm mPns -> 
      let 
        pScheduleFlow = view dealScheduledCashflow t
        pCfTxns = Map.map (maybe [] CF.getTsCashFlowFrame) $
                    case mPns of 
                      Nothing -> pScheduleFlow
                      Just pIds -> Map.filterWithKey (\k _ -> S.member k (S.fromList pIds)) pScheduleFlow
        txns = cutBy Exc Future asOfDay $ concat $ Map.elems pCfTxns
        txnsCfs = CF.tsTotalCash <$> txns -- `debug` ("schedule cf as of "++ show asOfDay ++ ">>" ++ show txns)
        txnsDs = getDate <$> txns
        txnsRates = CF.mflowRate <$> txns
        scheduleBal = queryDeal t (FutureCurrentSchedulePoolBegBalance mPns)
        curBal = queryDeal t (FutureCurrentPoolBalance mPns) 
        factor = case scheduleBal of 
                   0.00 -> 0  
                   _ -> curBal / scheduleBal -- `debug` ("cur Bal"++show curBal ++">> sheduleBal"++ show scheduleBal)
        cfForPv = (factor *) <$> txnsCfs -- `debug` (">>> factor"++ show factor)
        pvs = case pm of
                PvRate r -> uncurry (A.pv2 (fromRational r) asOfDay) <$> zip txnsDs cfForPv
                PvByRef ds -> uncurry (A.pv2 (queryDealRate t ds) asOfDay) <$> zip txnsDs cfForPv
                _ -> error $ "Failed to use pricing method on pool" ++ show pm ++"on pool id"++ show mPns
      in 
        sum pvs -- `debug` ("pvs"++ show pvs)

    BondsIntPaidAt d bns ->
       let
          stmts = map L.bndStmt $ viewDealBondsByNames t bns
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
          stmts = map L.bndStmt $ viewDealBondsByNames t bns
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
        -- bnds = (bndMap Map.!) <$> bns -- Map.elems $ getBondByName t (Just bns)
        bnds = viewDealBondsByNames t bns
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
      sum $ L.bndDueInt <$> viewDealBondsByNames t bns  

    CurrentDueBondIntOverInt bns -> 
      sum $ L.bndDueIntOverInt <$> viewDealBondsByNames t bns  

    CurrentDueBondIntTotal bns -> sum (queryDeal t <$> [CurrentDueBondInt bns,CurrentDueBondIntOverInt bns])

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

    WeightedAvgCurrentBondBalance d1 d2 bns ->
      Map.foldr (\v a-> a + (L.weightAverageBalance d1 d2 v)) -- `debug` (" Avg Bal for bond"++ show (L.weightAverageBalance d1 d2 v)) )
                0.0 
                (getBondsByName t (Just bns))

    WeightedAvgCurrentPoolBalance d1 d2 mPns ->
      let 
        txnsByPool = getAllCollectedTxns t mPns
        waBalByPool = Map.map (CF.mflowWeightAverageBalance d1 d2 <$>) txnsByPool
      in 
        sum $ fromMaybe 0  <$> Map.elems waBalByPool

    WeightedAvgOriginalBondBalance d1 d2 bns ->
      let 
        bnds = viewDealBondsByNames t bns
        oBals = getOriginBalance <$> bnds
        bgDates = L.originDate . L.bndOriginInfo <$> bnds -- `debug` ("bals"++show oBals++">>"++ show d1++"-"++show d2)
      in 
        sum $ (\(b,sd) -> mulBR b (yearCountFraction DC_ACT_365F (max d1 sd) d2)) <$> (zip oBals bgDates) -- `debug` ("bgDates"++show bgDates)

    WeightedAvgOriginalPoolBalance d1 d2 mPns ->
      mulBR 
        (Map.findWithDefault 0.0 IssuanceBalance (getIssuanceStatsConsol t mPns))
        (yearCountFraction DC_ACT_365F d1 d2)

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

    Divide ds1 ds2 -> if (queryDeal t ds2) == 0 then 
                        (fromRational . toRational) Numeric.Limits.infinity
                      else
                        queryDeal t ds1 / queryDeal t ds2

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
    Multiply ss -> product (queryDeal t <$> ss)
    FloorWith s floor -> max (queryDeal t s) (queryDeal t floor)
    FloorWithZero s -> max (queryDeal t s) 0
    Excess (s1:ss) -> max 0 $ queryDeal t s1 - queryDeal t (Sum ss) -- `debug` ("Excess"++show (queryDeal t s1)++"ss"++show ( queryDeal t (Sum ss)))
    CapWith s cap -> min (queryDeal t s) (queryDeal t cap)
    Abs s -> abs $ queryDeal t s
    Round ds rb -> roundingBy rb (queryDeal t ds)
    DivideRatio s1 s2 -> fromRational . toRational $ queryDealRate t (DivideRatio s1 s2)
    
    _ -> error ("Failed to query balance of -> "++ show s)

queryDealBool :: P.Asset a => TestDeal a -> DealStats -> Date -> Bool
queryDealBool t@TestDeal{triggers= trgs,bonds = bndMap} ds d = 
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

    IsOutstanding bns -> all (not . isPaidOff) $ (bndMap Map.!) <$> bns
    
    TestRate ds cmp r -> let
                           testRate = queryDealRate t ds
                         in  
                           case cmp of 
                             G ->  testRate > r
                             GE -> testRate >= r
                             L ->  testRate < r
                             LE -> testRate <= r
                             E ->  testRate == r
    
    HasPassedMaturity bns -> let 
                               oustandingBnds = filter (not . isPaidOff) $ (bndMap Map.!) <$> bns
                               monthsToMaturity = (\bn -> queryDealInt t (MonthsTillMaturity bn) d) <$> L.bndName <$> oustandingBnds
                             in 
                               all (<= 0) monthsToMaturity

    IsDealStatus st -> status t == st

    TestNot ds -> not (queryDealBool t ds d)

    TestAny b dss -> b `elem` [ queryDealBool t ds d | ds <- dss ]
    TestAll b dss -> all (== b) [ queryDealBool t ds d | ds <- dss ] 

    _ -> error ("Failed to query bool type formula"++ show ds)

-- ^ test a condition with a deal and a date
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
    IfBool s True -> queryDealBool t s d
    IfBool s False -> not (queryDealBool t s d)
    If2 cmp s1 s2 -> toCmp cmp (queryDeal t (ps s1)) (queryDeal t (ps s2))
    IfRate2 cmp s1 s2 -> toCmp cmp (queryDealRate t (ps s1)) (queryDealRate t (ps s2))
    IfInt2 cmp s1 s2 -> toCmp cmp (queryDealInt t (ps s1) d) (queryDealInt t (ps s2) d)
    IfDealStatus st -> status t == st   --  `debug` ("current date"++show d++">> stutus"++show (status t )++"=="++show st)
    Always b -> b
    IfNot _p -> not $ testPre d t _p
    where 
      toCmp x = case x of 
                  G -> (>)
                  GE -> (>=)
                  L -> (<)
                  LE -> (<=)
                  E -> (==)
      ps = patchDateToStats d

replaceToInf :: String -> String
replaceToInf x = unpack $ Data.Text.replace nInf "-inf" $ Data.Text.replace inf "inf" c
                  where 
                    c = pack x
                    inf = pack "179769313486231590772930519078902473361797697894230657273430081157732675805500963132708477322407536021120113879871393357658789768814416622492847430639474124377767893424865485276302219601246094119453082952085005768838150682342462881473913110540827237163350510684586298239947245938479716304835356329624224137216.00" 
                    nInf = pack "-179769313486231590772930519078902473361797697894230657273430081157732675805500963132708477322407536021120113879871393357658789768814416622492847430639474124377767893424865485276302219601246094119453082952085005768838150682342462881473913110540827237163350510684586298239947245938479716304835356329624224137216.00"


-- ^ convert a condition to string in a deal context
preToStr :: P.Asset a => TestDeal a -> Date -> Pre -> String
preToStr t d p =
  case p of 
    (IfZero ds) ->  "0 == " ++ show (queryDeal t (ps ds))
    (If cmp ds bal) -> show (queryDeal t (ps ds)) ++" "++ show cmp ++" " ++show bal -- `debug` (">>> left"++ show (queryDeal t (ps ds)))
    (IfRate cmp ds r) -> show (queryDealRate t (ps ds)) ++" "++ show cmp ++" " ++show r
    (IfInt cmp ds r) -> show (queryDealInt t (ps ds) d) ++" "++ show cmp ++" " ++show r
    (IfCurve cmp ds ts) -> show (queryDeal t (ps ds)) ++" "++ show cmp ++" " ++show (fromRational (getValByDate ts Inc d))
    (IfDate cmp _d) -> show d ++" "++ show cmp ++" " ++show _d
    (IfBool ds b) -> show (queryDealBool t ds d) ++" == "++ show b
    (If2 cmp ds1 ds2) -> show (queryDeal t (ps ds1)) ++" "++ show cmp ++" " ++show (queryDeal t (ps ds2))
    (IfRate2 cmp ds1 ds2) -> show (queryDealRate t (ps ds1)) ++" "++ show cmp ++" " ++show (queryDealRate t (ps ds2))
    (IfInt2 cmp ds1 ds2) -> show (queryDealInt t (ps ds1) d) ++" "++ show cmp ++" " ++show (queryDealInt t (ps ds2) d)
    (IfDealStatus st) -> show (status t) ++" == "++ show st
    (Always b) -> show b
    (IfNot _p) -> "Not "++ preToStr t d _p
    (Types.All pds) -> "All:"++ intercalate "|" (map (preToStr t d) pds)
    (Types.Any pds) -> "Any:"++ intercalate "|" (map (preToStr t d) pds)
    _ -> "Failed to read condition"++ show p

  where 
    ps = patchDateToStats d

testPre2 :: P.Asset a => Date -> TestDeal a -> Pre -> (String, Bool)
testPre2 d t p = 
  let 
    r = testPre d t p 
  in 
    ( replaceToInf (preToStr t d p), r)
