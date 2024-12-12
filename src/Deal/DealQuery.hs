{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Deal.DealQuery (queryDealBool ,patchDateToStats,patchDatesToStats,testPre
                      ,calcTargetAmount, testPre2
                      ,queryCompound) 
  where

import Deal.DealBase
import Types
import qualified Asset as P
import Data.List
import Data.Fixed
import Data.Maybe
import Data.Text (replace, pack, unpack)
import Numeric.Limits
import Control.Monad.Loops
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
import qualified Pool as Pl
import Stmt
import Util
import Errors
import DateUtil
import Control.Lens hiding (element)
import Control.Lens.TH
import Control.Applicative
import Data.Map.Lens
import Debug.Trace
import Lib
import Cashflow (CashFlowFrame(CashFlowFrame))
import qualified Cashflow as P
debug = flip trace

-- | calcuate target balance for a reserve account, 0 for a non-reserve account
calcTargetAmount :: P.Asset a => TestDeal a -> Date -> A.Account -> Either String Balance
calcTargetAmount t d (A.Account _ _ _ Nothing _ ) = Right 0
calcTargetAmount t d (A.Account _ _ _ (Just r) _ ) =
   eval r 
   where
     eval :: A.ReserveAmount -> Either String Balance
     eval ra = case ra of
       A.PctReserve ds _rate -> do 
                                  v <- queryCompound t d (patchDateToStats d ds)
                                  return (fromRational (v * _rate))
       A.FixReserve amt -> Right amt
       A.Either p ra1 ra2 -> do 
                               q <- testPre d t p
                               if q then 
                                 eval ra1
                               else 
                                 eval ra2 
       A.Max ras -> maximum' <$> sequenceA (eval <$> ras)
       A.Min ras -> minimum' <$> sequenceA (eval <$> ras)

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
      ReserveGap ans -> ReserveGapAt d ans
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

      
-- ^ map from Pool Source to Pool CutoffFields in Pool Map
poolSourceToIssuanceField :: PoolSource -> CutoffFields
poolSourceToIssuanceField CollectedInterest = HistoryInterest
poolSourceToIssuanceField CollectedPrincipal = HistoryPrincipal
poolSourceToIssuanceField CollectedRecoveries = HistoryRecoveries
poolSourceToIssuanceField CollectedPrepayment = HistoryPrepayment
poolSourceToIssuanceField CollectedPrepaymentPenalty = HistoryPrepaymentPentalty
poolSourceToIssuanceField CollectedRental = HistoryRental
poolSourceToIssuanceField CollectedFeePaid = HistoryFeePaid
poolSourceToIssuanceField CollectedCash = HistoryCash
poolSourceToIssuanceField NewLosses = HistoryLoss
poolSourceToIssuanceField NewDefaults = HistoryDefaults
poolSourceToIssuanceField NewDelinquencies = HistoryDelinquency
poolSourceToIssuanceField a = error ("Failed to match pool source when mapping to issuance field"++show a)



queryCompound :: P.Asset a => TestDeal a -> Date -> DealStats -> Either String Rational 
queryCompound t@TestDeal{accounts=accMap, bonds=bndMap, ledgers=ledgersM, fees=feeMap, pool=pt}
              d s =
  case s of
    Sum _s -> sum <$> sequenceA [ queryCompound t d __s | __s <- _s]
    Substract dss -> queryCompound t d (Subtract dss)
    Subtract (ds:dss) -> 
      do
        a <- queryCompound t d ds 
        bs <- queryCompound t d (Sum dss) 
        return $ a - bs
    Avg dss ->  (/ (toRational (length dss))) <$> (sum <$> sequenceA (queryCompound t d <$> dss )) 
    Max ss -> maximum' [ queryCompound t d s | s <- ss ]
    Min ss -> minimum' [ queryCompound t d s | s <- ss ]
    Divide ds1 ds2 -> if (queryCompound t d ds2) == Right 0 then 
                        Left $ "Date:"++show d++"Can not divide zero on ds: "++ show ds2
                      else
                        liftA2 (/) (queryCompound t d ds1) (queryCompound t d ds2)
    Factor s f -> (* f) <$> queryCompound t d s
    FloorAndCap floor cap s -> max (queryCompound t d floor) $ min (queryCompound t d cap) (queryCompound t d s)
    Multiply ss -> product <$> sequenceA [ queryCompound t d _s | _s <- ss]
    FloorWith s floor -> liftA2 max (queryCompound t d s) (queryCompound t d floor)
    FloorWithZero s -> max 0 <$> queryCompound t d s
    Excess (s1:ss) -> do 
                        q1 <- queryCompound t d s1 
                        q2 <- queryCompound t d (Sum ss) -- `debug` ("Excess"++show (queryCompound t s1)++"ss"++show ( queryCompound t (Sum ss)))
                        return (max 0 (q1 -q2))
    CapWith s cap -> min (queryCompound t d s) (queryCompound t d cap)
    Abs s -> abs <$> queryCompound t d s
    Round ds rb -> do 
                     q <- queryCompound t d ds
                     return $ roundingBy rb q
    DivideRatio s1 s2 -> queryCompound t d (Divide s1 s2)
    AvgRatio ss -> queryCompound t d (Avg ss)
    Constant v -> Right v
    -- rate query
    BondFactor -> queryCompound t d (Divide CurrentBondBalance  OriginalBondBalance) 
    BondFactorOf bn -> 
      queryCompound t d (Divide (CurrentBondBalanceOf [bn]) (OriginalBondBalanceOf [bn])) 
    PoolFactor mPns -> 
      queryCompound t d (Divide (CurrentPoolBalance mPns) (OriginalPoolBalance mPns))
    FutureCurrentPoolFactor asOfDay mPns -> 
      queryCompound t d (Divide (FutureCurrentPoolBalance mPns) (OriginalPoolBalance mPns))
    CumulativePoolDefaultedRate mPns -> 
      queryCompound t d (Divide (PoolCumCollection [NewDefaults] mPns) (OriginalPoolBalance mPns))
    CumulativeNetLossRatio mPns -> 
      queryCompound t d (Divide (CumulativeNetLoss mPns) (OriginalPoolBalance mPns))
    CumulativePoolDefaultedRateTill idx mPns ->
      queryCompound t d (Divide (PoolCumCollectionTill idx [NewDefaults] mPns) (OriginalPoolBalance mPns))
    
    BondRate bn -> 
      case Map.lookup bn (bonds t) of 
        Just b@(L.Bond {}) -> Right . toRational $ L.bndRate b 
        Just b@(L.BondGroup bSubMap) -> 
          let 
            bnds = Map.elems bSubMap
            rates = toRational . L.bndRate <$> bnds
            bals = L.getCurBalance <$> bnds
          in 
            Right $ weightedBy bals rates
        Nothing -> 
          case viewDealBondsByNames t [bn] of 
            [b] -> Right $ toRational $ L.bndRate b

    BondWaRate bns ->
      do 
        rs <- sequenceA $ (\bn -> queryCompound t d (BondRate bn)) <$> bns
        ws <- sequenceA $ (\bn -> queryCompound t d (CurrentBondBalanceOf [bn])) <$> bns
        return $ weightedBy (fromRational <$> ws) rs
    PoolWaRate mPns -> 
      let 
        latestCfs = filter isJust $ Map.elems $ getLatestCollectFrame t mPns
        rates = toRational . maybe 0.0 CF.mflowRate  <$> latestCfs
        bals = maybe 0.0 CF.mflowBalance  <$> latestCfs
      in 
        Right $ weightedBy bals rates

    -- int query
    FutureCurrentPoolBorrowerNum _d mPns ->
      let 
        poolCfs = Map.elems $ getLatestCollectFrame t mPns
        poolBn =  maybe 0 (fromMaybe 0 . CF.mflowBorrowerNum) <$> poolCfs
      in 
        Right . toRational $ sum poolBn
    CurrentPoolBorrowerNum mPns ->
      let 
        assetM = concat $ Map.elems $ getAllAsset t mPns
      in 
        Right . toRational $ sum $ P.getBorrowerNum <$> assetM 

    MonthsTillMaturity bn -> 
      do 
        (L.OriginalInfo _ _ _ mm) <- lookupAndApply L.bndOriginInfo "Get Months till maturity" bn bndMap 
        case mm of
          Nothing -> Left $ "Date:"++show d++"There is maturity date for bond " ++ bn
          Just md -> Right . toRational $ T.cdMonths $ T.diffGregorianDurationClip md d
          

    ProjCollectPeriodNum -> Right . toRational $ maximum' $ Map.elems $ Map.map (maybe 0 CF.sizeCashFlowFrame) $ getAllCollectedFrame t Nothing

    ReserveBalance ans -> 
      do 
        accBal <- lookupAndApplies (calcTargetAmount t d) ("Date:"++show d++"Cal Reserve Balance") ans accMap
        vs <- sequenceA accBal
        return $ toRational (sum vs)


    ReserveExcessAt _d ans ->
      do 
        q1 <- queryCompound t d (AccBalance ans)
        q2 <- queryCompound t d (ReserveBalance ans)
        return $ max 0 (q1 - q2)

    ReserveGapAt _d ans ->
      do 
        q1 <- queryCompound t d (AccBalance ans)
        q2 <- queryCompound t d (ReserveBalance ans)
        return $ max 0 (q2 - q1)

    CurrentBondBalance -> Right . toRational $ Map.foldr (\x acc -> getCurBalance x + acc) 0.0 bndMap
    
    OriginalBondBalance -> Right . toRational $ Map.foldr (\x acc -> getOriginBalance x + acc) 0.0 bndMap
    
    BondDuePrin bnds -> Right . toRational $ sum $ L.bndDuePrin <$> viewDealBondsByNames t bnds
    
    OriginalBondBalanceOf bnds -> Right . toRational $ sum $ getOriginBalance <$> viewDealBondsByNames t bnds

    CurrentBondBalanceOf bns -> Right . toRational $ sum $ getCurBalance <$> viewDealBondsByNames t bns
    
    CurrentPoolBalance mPns ->
      let
        assetM = concat $ Map.elems $ getAllAsset t mPns
      in 
        Right . toRational $ sum $ P.getCurrentBal <$> assetM 
    
    CurrentPoolDefaultedBalance ->
      Right . toRational $ 
        foldl (\acc x -> acc + P.getCurrentBal x)
              0.0 $
              filter P.isDefaulted (getAllAssetList t)

    DealIssuanceBalance mPns -> 
      Right . toRational $ 
        sum $ Map.findWithDefault 0.0 IssuanceBalance <$> Map.elems (getIssuanceStats t mPns)

    OriginalPoolBalance mPns -> 
      let 
        statsConsol = getIssuanceStatsConsol t mPns 
      in 
        case Map.lookup IssuanceBalance statsConsol of 
          Just v -> Right . toRational $ v
          Nothing -> Left $ "Date:"++show d++"No issuance balance found in the pool, pls specify it in the pool stats map `issuanceStat`"
    
    UnderlyingBondBalance mBndNames -> Left $ "Date:"++show d++"Not implemented for underlying bond balance"
 
    AllAccBalance -> Right . toRational $ sum $ map A.accBalance $ Map.elems accMap 
    
    AccBalance ans -> 
      do 
        accBals <- lookupAndApplies A.accBalance "AccBalance" ans accMap
        return $ (toRational . sum) accBals

    -- ^ negatave -> credit balance , postive -> debit balance
    LedgerBalance ans ->
      case ledgersM of 
        Nothing -> Left ("Date:"++show d++"No ledgers were modeled , failed to find ledger:"++show ans )
        Just ledgersM -> 
          do 
            lgBals <- lookupAndApplies LD.ledgBalance "Ledger Balance" ans ledgersM
            return $ (toRational . sum) lgBals
    
    LedgerBalanceBy dr ans ->
      case ledgersM of 
        Nothing -> Left ("Date:"++show d++"No ledgers were modeled , failed to find ledger:"++show ans )
        Just ledgersM ->
          do 
            lgdsM <- selectInMap "Look up ledgers" ans ledgersM
            let ldgL = Map.elems lgdsM
            let bs Credit = filter (\x -> LD.ledgBalance x < 0) ldgL
            let bs Debit = filter (\x -> LD.ledgBalance x >= 0) ldgL
            return $ toRational $ abs $ sum $ LD.ledgBalance <$> bs dr -- `debug` ("dr"++show dr++">> bs dr"++ show (bs dr))

    FutureCurrentPoolBalance mPns ->
      case (mPns,pt) of 
        (Nothing, MultiPool pm ) -> queryCompound t d (FutureCurrentPoolBalance (Just $ Map.keys pm))
        (Just pids, MultiPool pm) -> 
          if S.isSubsetOf  (S.fromList pids) (S.fromList (Map.keys pm)) then 
            let 
              m = Map.filterWithKey (\k _ -> S.member k (S.fromList pids)) pm
            in 
              Right . toRational $ sum $ Map.elems $ Map.map (`Pl.getIssuanceField` RuntimeCurrentPoolBalance) m 
          else 
            Left $ "Date:"++show d++"Failed to find pool balance" ++ show pids ++ " from deal "++ show (Map.keys pm)
        _ -> Left $ "Date:"++show d++"Failed to find pool" ++ show (mPns) ++","++ show pt

    FutureCurrentSchedulePoolBalance mPns ->
      let 
        scheduleFlowM = Map.elems $ view dealScheduledCashflow t
      in 
        Right . toRational $ sum $ maybe 0 (CF.mflowBalance . head . view CF.cashflowTxn) <$> scheduleFlowM
    
    FutureCurrentSchedulePoolBegBalance mPns ->
      let 
        scheduleFlowM = Map.elems $ view dealScheduledCashflow t
      in 
        Right . toRational $ sum $ maybe 0 (CF.mflowBegBalance . head . view CF.cashflowTxn) <$> scheduleFlowM
    
    FutureCurrentPoolBegBalance mPns ->
      let 
        ltc = getLatestCollectFrame t mPns
      in 
        Right . toRational $ sum $ maybe 0 CF.mflowBegBalance <$> ltc 

    PoolCollectionHistory incomeType fromDay asOfDay mPns ->
      Right . toRational $ sum fieldAmts
        where
          mTxns = Map.elems $ getAllCollectedTxns t mPns
          subflow = sliceBy EI fromDay asOfDay $ concat $ fromMaybe [] <$> mTxns
          fieldAmts = map (`CF.lookupSource` incomeType) subflow  

    CumulativePoolDefaultedBalance mPns ->
      let
        latestCollect = getLatestCollectFrame t mPns
        futureDefaults = sum $ Map.elems $ Map.map (maybe 0 (fromMaybe 0 . CF.tsCumDefaultBal )) $ latestCollect 
      in
        Right . toRational $ futureDefaults -- `debug` ("future Defaults at"++ show futureDefaults ++ show latestCollect)

    CumulativePoolRecoveriesBalance mPns ->
      let
        latestCollect = getLatestCollectFrame t mPns
        futureRecoveries = sum $ Map.elems $ Map.map (maybe 0 (fromMaybe 0 . CF.tsCumRecoveriesBal)) $ latestCollect 
      in
        Right . toRational $ futureRecoveries
    
    CumulativeNetLoss mPns ->
      liftA2 
        (-)
        (queryCompound t d (CumulativePoolDefaultedBalance mPns))
        (queryCompound t d (CumulativePoolRecoveriesBalance mPns))
    
    PoolCumCollection ps mPns ->
      let 
        collectedTxns = concat . Map.elems $ Map.map (fromMaybe []) $ getAllCollectedTxns t mPns
        futureVals = sum $ (CF.lookupSource <$> collectedTxns) <*> ps
        
        poolStats = Map.elems $ getIssuanceStats t mPns
        historyVals = sum $ (Map.findWithDefault 0.0 . poolSourceToIssuanceField <$> ps) <*> poolStats
      in 
        Right . toRational $ futureVals + historyVals
    
    PoolCumCollectionTill idx ps mPns -> 
      let 
        txnMap = Map.map (dropLastN (negate idx) . fromMaybe []) $ getAllCollectedTxns t mPns 
        txnList = concat $ Map.elems txnMap
        lookupList = CF.lookupSource <$> txnList
        futureVals = sum $ lookupList <*> ps
        sumMap = getIssuanceStatsConsol t mPns
        historyVals = sum $ Map.findWithDefault 0 . poolSourceToIssuanceField <$> ps <*> [sumMap]
      in 
        Right . toRational $ futureVals + historyVals

    PoolCurCollection ps mPns ->
      let 
        pCf = getLatestCollectFrame t mPns -- `debug` ("mPns"++ show mPns)
        lastRows = Map.map (maybe 0 (\r -> sum (CF.lookupSource r <$> ps))) pCf -- `debug` ("Latest collect frame"++ show pCf)
      in 
        Right . toRational $ sum $ Map.elems lastRows -- `debug   ` ("lst row found"++ show lastRows)

    PoolCollectionStats idx ps mPns -> 
      let 
        pCollectedTxns = getAllCollectedTxns t mPns 
        pStat = Map.map
                  (\_x -> 
                    let
                      lookupIndx = length x + idx - 1
                      x = fromMaybe [] _x
                    in
                      if (( lookupIndx >= length x ) ||  (lookupIndx <0)) then 
                        Nothing
                      else
                        Just (x!!lookupIndx))
                  pCollectedTxns -- `debug` ("date"++show d++"Pool collection: "++ show pCollectedTxns)
      in
        do
          curPoolBalM <- sequenceA $
                           Map.mapWithKey
                             (\k v -> queryCompound t d (FutureCurrentPoolBalance (Just [k]))) 
                             pStat -- `debug` ("date"++show d++"Pool stats collection: "++ show pStat)
          let poolStat = Map.mapWithKey
                           (\k v -> 
                              case v of
                                Just _v -> sum $ CF.lookupSource _v <$> ps
                                Nothing -> sum $ CF.lookupSourceM (fromRational (curPoolBalM Map.! k)) Nothing <$> ps)
                           pStat  -- `debug` ("date"++show d++"query pool current pool stat 2" ++ show pStat )
          return $ sum $ Map.elems $ toRational <$> poolStat -- `debug` ("query pool current stats"++ show poolStat)

    FuturePoolScheduleCfPv asOfDay pm mPns -> 
      let 
        pScheduleFlow = view dealScheduledCashflow t
        pCfTxns = Map.map (maybe [] (view CF.cashflowTxn)) $
                    case mPns of 
                      Nothing -> pScheduleFlow
                      Just pIds -> Map.filterWithKey (\k _ -> S.member k (S.fromList pIds)) pScheduleFlow
        txns = cutBy Exc Future asOfDay $ concat $ Map.elems pCfTxns
        txnsCfs = CF.tsTotalCash <$> txns -- `debug` ("schedule cf as of "++ show asOfDay ++ ">>" ++ show txns)
        txnsDs = getDate <$> txns
        txnsRates = CF.mflowRate <$> txns
      in
        do 
          scheduleBal <- queryCompound t d (FutureCurrentSchedulePoolBegBalance mPns)
          curBal <- queryCompound t d (FutureCurrentPoolBalance mPns) 
          let factor = case scheduleBal of
                         0.00 -> 0  
                         _ -> curBal / scheduleBal -- `debug` ("cur Bal"++show curBal ++">> sheduleBal"++ show scheduleBal)
          let cfForPv = (`mulBR` factor) <$> txnsCfs -- `debug` (">>> factor"++ show factor)
          let pvs = case pm of
                      PvRate r -> uncurry (A.pv2 r asOfDay) <$> zip txnsDs cfForPv
                      -- _ -> Left $ "Date:"++ show asOfDay ++ "Failed to use pricing method on pool" ++ show pm ++"on pool id"++ show mPns
          return $ toRational $ sum pvs

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
          Right . toRational $ sum $ map ex stmts

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
          Right . toRational $ sum $ map ex stmts
    
    FeeTxnAmtBy d fns mCmt -> 
      let 
        fees = (feeMap Map.!) <$> fns -- Map.elems $ getFeeByName t (Just fns)
      in  
        Right . toRational $
          case mCmt of 
            Just cmt -> sum [ queryTxnAmtAsOf fee d cmt | fee <- fees ]
            Nothing -> 
              let 
                _txn = concat [ getTxns (F.feeStmt fee) | fee <- fees ]
              in 
                sumTxn $ cutBy Inc Past d _txn 
    
    BondTxnAmtBy d bns mCmt -> 
      let 
        bnds = viewDealBondsByNames t bns
      in 
        Right . toRational $
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
        Right . toRational $
          case mCmt of
            Just cmt -> sum [ queryTxnAmtAsOf acc d cmt | acc <- accs ]
            Nothing ->
              let 
                _txn = concat [ getTxns (A.accStmt acc) | acc <- accs ]
              in 
                sumTxn $ cutBy Inc Past d _txn 

    LedgerTxnAmt lns mCmt ->
      case ledgersM of 
        Nothing -> Left $ ("Date:"++show d++"No ledgers were modeled , failed to find ledger:"++show lns )
        Just ledgerm ->
          let 
            lgs = (ledgerm Map.!) <$> lns
          in
            case mCmt of
              Just cmt -> Right . toRational $ sum [ queryTxnAmt lg cmt | lg <- lgs ]
              Nothing -> Right . toRational $ sum [ LD.ledgBalance lg | lg <- lgs ]

    BondBalanceGapAt d bName -> 
      toRational <$>
        lookupAndApply 
          (\b@L.Bond{L.bndBalance = bal ,L.bndType = L.PAC _target} 
            -> max 0 ( bal - getValOnByDate _target d))
          ("Failed to find bond "++ bName)
          bName
          bndMap

    FeesPaidAt d fns ->
      let
        fSubMap = getFeeByName t (Just fns)
        stmts = map F.feeStmt $ Map.elems fSubMap
        ex s = case s of
                 Nothing -> 0
                 Just (Statement txns) -> sum $ getTxnAmt <$> filter (\x ->  d == getDate x) txns
      in
        Right . toRational $ sum $ map ex stmts

    CurrentDueBondInt bns -> 
      Right . toRational $ sum $ L.bndDueInt <$> viewDealBondsByNames t bns  

    CurrentDueBondIntOverInt bns -> 
      Right . toRational $ sum $ L.bndDueIntOverInt <$> viewDealBondsByNames t bns  

    CurrentDueBondIntTotal bns -> 
      sum <$> sequenceA (queryCompound t d <$> [CurrentDueBondInt bns,CurrentDueBondIntOverInt bns])

    CurrentDueFee fns -> 
      do 
        vs <- lookupAndApplies F.feeDue "Get Current Due Fee" fns feeMap
        return $ toRational (sum vs)

    LiqCredit lqNames -> 
      case liqProvider t of
        Nothing -> Left $ "Date:"++show d++"No Liquidation Provider modeled when looking for " ++ show s
        Just liqProviderM -> Right . toRational $
                               sum $ [ fromMaybe 0 (CE.liqCredit liq) | (k,liq) <- Map.assocs liqProviderM
                                     , S.member k (S.fromList lqNames) ]

    LiqBalance lqNames -> 
      case liqProvider t of
        Nothing -> Left $ "Date:"++show d++"No Liquidation Provider modeled when looking for " ++ show s
        Just liqProviderM -> Right . toRational $
                               sum $ [ CE.liqBalance liq | (k,liq) <- Map.assocs liqProviderM
                                     , S.member k (S.fromList lqNames) ]

    RateCapNet rcName -> case rateCap t of
                           Nothing -> Left $ "Date:"++show d++"No Rate Cap modeled when looking for " ++ show s
                           Just rm -> case Map.lookup rcName rm of
                                        Nothing -> Left $ "Date:"++show d++"No Rate Cap modeled when looking for " ++ show s
                                        Just rc -> Right . toRational $ H.rcNetCash rc
    
    RateSwapNet rsName -> case rateCap t of
                           Nothing -> Left $ "Date:"++show d++"No Rate Swap modeled when looking for " ++ show s
                           Just rm -> case Map.lookup rsName rm of
                                        Nothing -> Left $ "Date:"++show d++"No Rate Swap modeled when looking for " ++ show s
                                        Just rc -> Right . toRational $ H.rcNetCash rc

    WeightedAvgCurrentBondBalance d1 d2 bns ->
      Right . toRational $ 
        Map.foldr (\v a-> a + (L.weightAverageBalance d1 d2 v)) -- `debug` (" Avg Bal for bond"++ show (L.weightAverageBalance d1 d2 v)) )
                  0.0 
                  (getBondsByName t (Just bns))

    WeightedAvgCurrentPoolBalance d1 d2 mPns ->
      let 
        txnsByPool = getAllCollectedTxns t mPns
        waBalByPool = Map.map (CF.mflowWeightAverageBalance d1 d2 <$>) txnsByPool
      in 
        Right . toRational $ 
          sum $ fromMaybe 0  <$> Map.elems waBalByPool

    WeightedAvgOriginalBondBalance d1 d2 bns ->
      let 
        bnds = viewDealBondsByNames t bns
        oBals = getOriginBalance <$> bnds
        bgDates = L.originDate . L.bndOriginInfo <$> bnds -- `debug` ("bals"++show oBals++">>"++ show d1++"-"++show d2)
      in 
        Right . toRational $ 
          sum $ (\(b,sd) -> mulBR b (yearCountFraction DC_ACT_365F (max d1 sd) d2)) <$> (zip oBals bgDates) -- `debug` ("bgDates"++show bgDates)

    WeightedAvgOriginalPoolBalance d1 d2 mPns ->
      Right . toRational $
         mulBR
          (Map.findWithDefault 0.0 IssuanceBalance (getIssuanceStatsConsol t mPns))
          (yearCountFraction DC_ACT_365F d1 d2)

    CustomData s d ->
        case custom t of 
          Nothing -> Left $ "Date:"++show d++"No Custom data to query" ++ show s
          Just mCustom ->
              case Map.lookup s mCustom of 
                Just (CustomConstant v) -> Right . toRational $ v 
                Just (CustomCurve cv) -> Right . toRational $ getValOnByDate cv d
                Just (CustomDS ds) -> queryCompound t d (patchDateToStats d ds )
                _ -> Left $ "Date:"++show d++"Unsupported custom data found for key " ++ show s

    _ -> Left ("Date:"++show d++"Failed to query formula of -> "++ show s)
    



queryDealBool :: P.Asset a => TestDeal a -> DealStats -> Date -> Either String Bool
queryDealBool t@TestDeal{triggers= trgs,bonds = bndMap} ds d = 
  case ds of 
    TriggersStatus dealcycle tName -> 
      case trgs of 
        Just _trgsM -> case Map.lookup dealcycle _trgsM of 
                         Nothing -> Left ("Date:"++show d++"no trigger cycle for this deal" ++ show dealcycle)
                         Just triggerMatCycle -> 
                           case Map.lookup tName triggerMatCycle of 
                             Nothing -> Left ("Date:"++show d++"no trigger for this deal" ++ show tName ++ " in cycle " ++ show triggerMatCycle)
                             Just trigger -> Right $ Trg.trgStatus trigger 
        Nothing -> Left $ "Date:"++show d++"no trigger for this deal"
    
    IsMostSenior bn bns ->
      do 
        bn1 <- lookupAndApply isPaidOff "Is Most Senior" bn bndMap
        bns1 <- lookupAndApplies isPaidOff "Is Most Senior" bns bndMap
        return $
          case (bn1, and bns1) of
            (False,True) -> True
            _ -> False

    IsPaidOff bns -> 
      do 
        vs <- lookupAndApplies isPaidOff "Is Paid Off" bns bndMap
        return $ and vs

    IsOutstanding bns -> 
      do 
        vs <- lookupAndApplies (not . isPaidOff) "Is Outstanding" bns bndMap
        return $ and vs 
    
    TestRate ds cmp _r -> do
                            testRate <- queryCompound t d ds
                            let r = toRational r
                            return $ case cmp of 
                                       G ->  testRate > r
                                       GE -> testRate >= r
                                       L ->  testRate < r
                                       LE -> testRate <= r
                                       E ->  testRate == r
    
    HasPassedMaturity bns -> do 
                               bMap <- selectInMap "Bond Pass Maturity" bns bndMap
                               let oustandingBnds = Map.filter (not . isPaidOff) bMap
                               ms <- sequenceA $ (\bn -> queryCompound t d (MonthsTillMaturity bn)) <$> L.bndName <$> oustandingBnds
                               return $ all (<= 0) ms

    IsDealStatus st -> Right $ status t == st

    TestNot ds -> do not <$> (queryDealBool t ds d)
    -- TestAny b dss -> b `elem` [ queryDealBool t ds d | ds <- dss ]
    TestAny b dss -> anyM (\ x -> (== b) <$> queryDealBool t x d ) dss
    TestAll b dss -> allM (\ x -> (== b) <$> queryDealBool t x d ) dss

    _ -> Left ("Date:"++show d++"Failed to query bool type formula"++ show ds)

-- ^ test a condition with a deal and a date
testPre :: P.Asset a => Date -> TestDeal a -> Pre -> Either String Bool
testPre d t p =
  case p of
    Types.All pds -> allM (testPre d t) pds 
    -- Types.Any pds -> return $ any (testPre d t) pds 
    Types.Any pds -> anyM (testPre d t) pds 
    IfZero s -> do 
                  q <- queryCompound t d s 
                  return $ (round q) == 0
    
    If cmp s amt -> do 
                      q <- (queryCompound t d (ps s))
                      return $ toCmp cmp q (toRational amt) -- `debug` (show d++"if cmp "++show (queryDeal t (ps s))++"amt"++show amt)
    IfRate cmp s amt -> do 
                          q <- (queryCompound t d (ps s))
                          return $ toCmp cmp q (toRational amt) -- `debug` (show d++"rate"++show (queryDealRate t (ps s))++"amt"++show amt)
    IfInt cmp s amt -> do 
                         q <- (queryCompound t d (ps s))
                         return $ toCmp cmp q (toRational amt)
    
    -- Integer test
    IfIntIn s iset -> do 
                        q <- (queryCompound t d (ps s))
                        return $ (round q) `elem` iset
    IfIntBetween s rt i1 i2 ->
      do
        v <- queryCompound t d (ps s)
        case rt of 
          II -> return $ (round v) >= i1 && (round v) <= i2
          IE -> return $ (round v) >= i1 && (round v) < i2
          EI -> return $ (round v) > i1 && (round v) <= i2
          EE -> return $ (round v) > i1 && (round v) < i2 
    -- IfIntBetween cmp1 s1 cmp2 s2 amt -> toCmp cmp1 (queryDealInt t (ps s1) d) amt && toCmp cmp2 (queryDealInt t (ps s2) d) amt
    IfDate cmp _d -> return $ toCmp cmp d _d
    IfDateBetween II d1 d2 -> return $ d >= d1 && (d <= d2)
    IfDateBetween EI d1 d2 -> return $ d > d1 && (d <= d2)
    IfDateBetween IE d1 d2 -> return $ d >= d1 && (d < d2)
    IfDateBetween EE d1 d2 -> return $ d > d1 && (d < d2)
    IfDateIn ds -> return $ d `elem` ds

    IfCurve cmp s _ts -> do 
                           q <- (queryCompound t d (ps s))
                           return $ toCmp cmp q (getValByDate _ts Inc d)
    IfRateCurve cmp s _ts -> do v <- (queryCompound t d (ps s))
                                return $ (toCmp cmp) v (getValByDate _ts Inc d)
    IfBool s True -> queryDealBool t s d
    IfBool s False -> do 
                        q <- (queryDealBool t s d)
                        return q
    If2 cmp s1 s2 -> do 
                       q1 <- (queryCompound t d (ps s1))
                       q2 <- (queryCompound t d (ps s2))
                       return (toCmp cmp q1 q2)  
    IfRate2 cmp s1 s2 -> do 
                          q1 <- (queryCompound t d (ps s1))
                          q2 <- (queryCompound t d (ps s2))
                          return (toCmp cmp q1 q2)  
    IfInt2 cmp s1 s2 -> do 
                          q1 <- (queryCompound t d (ps s1))
                          q2 <- (queryCompound t d (ps s2))
                          return (toCmp cmp q1 q2)  
    IfDealStatus st -> Right $ status t == st   --  `debug` ("current date"++show d++">> stutus"++show (status t )++"=="++show st)
    
    Always b -> Right b
    IfNot _p -> not <$> testPre d t _p
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
    (IfZero ds) ->  "0 == " ++ show (fromRational <$> (queryCompound t d (ps ds)))
    (If cmp ds bal) -> show (fromRational <$> (queryCompound t d (ps ds))) ++" "++ show cmp ++" " ++show bal -- `debug` (">>> left"++ show (queryDeal t (ps ds)))
    (IfRate cmp ds r) -> show (fromRational <$> (queryCompound t d (ps ds))) ++" "++ show cmp ++" " ++show r
    (IfInt cmp ds r) -> show (fromRational <$> (queryCompound t d (ps ds))) ++" "++ show cmp ++" " ++show r
    (IfCurve cmp ds ts) -> show (fromRational <$> (queryCompound t d (ps ds))) ++" "++ show cmp ++" " ++show (fromRational (getValByDate ts Inc d))
    (IfDate cmp _d) -> show d ++" "++ show cmp ++" " ++show _d
    (IfBool ds b) -> show (fromRational <$> (queryCompound t d ds)) ++" == "++ show b
    (If2 cmp ds1 ds2) -> show (fromRational <$> (queryCompound t d (ps ds1))) ++" "++ show cmp ++" " ++show (fromRational <$> (queryCompound t d (ps ds2)))
    (IfRate2 cmp ds1 ds2) -> show (fromRational <$> (queryCompound t d (ps ds1))) ++" "++ show cmp ++" " ++show (fromRational <$> (queryCompound t d (ps ds2)))
    (IfInt2 cmp ds1 ds2) -> show (fromRational <$> (queryCompound t d (ps ds1))) ++" "++ show cmp ++" " ++show (fromRational <$> (queryCompound t d (ps ds2)))
    (IfDealStatus st) -> show (status t) ++" == "++ show st
    (Always b) -> show b
    (IfNot _p) -> "Not "++ preToStr t d _p
    (Types.All pds) -> "All:["++ intercalate "|" (map (preToStr t d) pds)++"]"
    (Types.Any pds) -> "Any:["++ intercalate "|" (map (preToStr t d) pds)++"]"
    _ -> "Failed to read condition"++ show p

  where 
    ps = patchDateToStats d

testPre2 :: P.Asset a => Date -> TestDeal a -> Pre -> (String, Either String Bool)
testPre2 d t p = (replaceToInf (preToStr t d p), testPre d t p)
