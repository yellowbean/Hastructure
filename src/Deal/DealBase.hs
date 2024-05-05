{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module Deal.DealBase (TestDeal(..),SPV(..),dealBonds,dealFees,dealAccounts,dealPool,PoolType(..),getIssuanceStats
                     ,getAllAsset,getAllAssetList,getAllCollectedFrame,getLatestCollectFrame,getAllCollectedTxns
                     ,getIssuanceStatsConsol,getAllCollectedTxnsList,dealScheduledCashflow
                     ,getPoolIds,getBondByName, UnderlyingDeal(..),dealCashflow, uDealFutureTxn) 
  where
import qualified Accounts as A
import qualified Ledger as LD
import qualified Asset as Ast
import qualified Expense as F
import qualified Liability as L
import qualified CreditEnhancement as CE
import qualified Hedge as HE
import qualified Waterfall as W
import qualified Cashflow as CF
import qualified Assumptions as AP
import qualified AssetClass.AssetBase as ACM
import qualified Call as C
import qualified InterestRate as IR
import Stmt
import Lib
import Util
import Types
import Revolving
import Triggers

import qualified Data.Map as Map
import qualified Data.Time as T
import qualified Data.Set as S
import Data.List
import Data.Fixed
import Data.Maybe
import Data.Ratio
import Data.Aeson hiding (json)
import qualified Data.Aeson.Encode.Pretty as Pretty
import Language.Haskell.TH
import Data.Aeson.TH
import Data.Aeson.Types
import GHC.Generics
import Control.Lens hiding (element)
import Control.Lens.TH
import Data.IntMap (filterWithKey)
import qualified Data.Text as T
import Text.Read (readMaybe)
import qualified Pool as P
import qualified Types as CF

import Debug.Trace
import qualified Control.Lens as P
debug = flip trace
-- import Data.Aeson.Types (Parser)
-- import qualified Data.HashMap.Strict as HM
-- import Data.Text (unpack)
-- import Control.Monad.IO.Class (liftIO)

class SPV a where
  getBondsByName :: a -> Maybe [String] -> Map.Map String L.Bond
  getBondBegBal :: a -> String -> Balance
  getBondStmtByName :: a -> Maybe [String] -> Map.Map String (Maybe Statement)
  getFeeByName :: a -> Maybe [String] -> Map.Map String F.Fee
  getAccountByName :: a -> Maybe [String] -> Map.Map String A.Account
  isResec :: a -> Bool


data UnderlyingDeal a = UnderlyingDeal {
  deal :: TestDeal a
  ,futureCf :: Maybe CF.CashFlowFrame
  ,futureScheduleCf :: Maybe CF.CashFlowFrame
  ,issuanceStat :: Maybe (Map.Map CutoffFields Balance)
} deriving (Generic,Eq,Ord,Show)

uDealFutureScheduleCf :: Ast.Asset a => Lens' (UnderlyingDeal a) (Maybe CF.CashFlowFrame)
uDealFutureScheduleCf = lens getter setter
  where 
    getter = futureScheduleCf
    setter ud newCf = ud {futureScheduleCf = newCf}

uDealFutureCf :: Ast.Asset a => Lens' (UnderlyingDeal a) (Maybe CF.CashFlowFrame)
uDealFutureCf = lens getter setter
  where 
    getter = futureCf
    setter ud newCf = ud {futureCf = newCf}

uDealFutureTxn :: Ast.Asset a => Lens' (UnderlyingDeal a) [CF.TsRow]
uDealFutureTxn = lens getter setter
  where 
    getter ud = fromMaybe [] $ CF.getTsCashFlowFrame <$> futureCf ud
    setter ud newTxn = ud {futureCf = Just (CF.CashFlowFrame newTxn)}


data PoolType a = SoloPool (P.Pool a)
                | MultiPool (Map.Map PoolId (P.Pool a))
                | ResecDeal (Map.Map PoolId (UnderlyingDeal a))
                deriving (Generic, Eq, Ord, Show)

-- instance Show (PoolType a) where
--   show (SoloPool x) = "SoloPool:"++ show x
--   show (MultiPool x) = "MultiPool:"++ show x
--   show (ResecDeal x) = "ResecDeal:"++ show x
-- 
-- instance Read (PoolType a) where
--   readsPrec _ "SoloPool" = [(SoloPool Map.empty,"")]
--   readsPrec _ "MultiPool" = [(MultiPool Map.empty,"")]
--   readsPrec _ "ResecDeal" = [(ResecDeal Map.empty,"")]
--   readsPrec _ _ = []

--instance Read UnderlyingBond where 
--  -- readsPrec _ "UnderlyingBond" = [(UnderlyingBond ("",0,0),"")]
--  readsPrec _ str =
--    case T.splitOn "_" (T.pack str) of
--      [bn, hp, d] -> [(UnderlyingBond (T.unpack bn, read (T.unpack hp)::Rational, read (T.unpack d)::Date), "")] `debug` ("Read success" )
--      _ -> [] `debug` ("read not match with "++ str)
--      --[bn, hp, d] -> case (reads (T.unpack hp), reads (T.unpack d)) of
--      --                 ((hpVal, _):_, (dVal, _):_) -> [(UnderlyingBond (T.unpack bn, hpVal, dVal), "")] `debug` ("Read success")
--      --                 _ -> [] `debug` ("read not match with "++ show (T.splitOn "_" (T.pack str)))
--
--instance Show UnderlyingBond where 
--  show (UnderlyingBond (bn,hp,d)) = bn ++ "_" ++ show hp ++ "_" ++ show d
--
--
--instance ToJSONKey UnderlyingBond where 
--  toJSONKey :: ToJSONKeyFunction UnderlyingBond
--  toJSONKey = toJSONKeyText $ \(UnderlyingBond (bn,hp,d)) -> T.pack $ bn ++ "_" ++ show hp ++ "_" ++ show d
--
--instance FromJSONKey UnderlyingBond where
----   fromJSONKey = FromJSONKeyTextParser $ \case
----     "name" -> pure $ (,) <$> parseJSONKey <*> parseJSONKey <*> parseJSONKey
----     _ -> fail "Expected \"name\" key"
--     fromJSONKey = FromJSONKeyTextParser $ 
--       \t -> case readMaybe (T.unpack t) of
--               Just k -> pure k   `debug` ("parsed with "++ show k)
--               Nothing -> fail ("Invalid key: " ++ show t++">>"++ show (T.unpack t))


-- buildPoolIdFromDeal ::  P.Asset a => PoolType a -> Map.Map (BondName, HoldingPct, Date) PoolId
-- buildPoolIdFromDeal (ResecDeal resecM) 
--   = Map.foldrWithKey 
--       (\(bn,hp,d) deal m 
--          -> Map.insert (bn,hp,d) (UnderlyingDeal (name deal) bn) m) 
--       Map.empty
--       resecM
-- 
-- buildPoolIdFromDeal _ = error "Not implemented for non-resec deal"

--                | ResecDeal (Map.Map UnderlyingBond (UnderlyingDeal a))

-- poolTypePool :: P.Asset a => Lens' (PoolType a) (Map.Map PoolId (P.Pool a))
-- poolTypePool = lens getter setter
--   where 
--     getter (SoloPool p) = Map.fromList [(PoolConsol,p)]
--     -- getter (ResecDeal uds) = Map.map (\(UnderlyingDeal d _ _) ud -> ud   ) uds
--     getter (MultiPool pm) = pm
--     getter (ResecDeal uds) = Map.map (\(UnderlyingDeal d _ _) -> d) uds
--     setter (SoloPool p) newPool = case Map.lookup PoolConsol newPool of
--                                     Just p -> SoloPool p
--                                     Nothing -> error $ "Can't set a solo pool to a multi pool"
--     setter (MultiPool pm) newPool = MultiPool newPool

data TestDeal a = TestDeal { name :: DealName
                             ,status :: DealStatus
                             ,dates :: DateDesp
                             ,accounts :: Map.Map AccountName A.Account
                             ,fees :: Map.Map FeeName F.Fee
                             ,bonds :: Map.Map BondName L.Bond
                             ,pool ::  PoolType a 
                             ,waterfall :: Map.Map W.ActionWhen W.DistributionSeq
                             ,collects :: [W.CollectionRule]
                             ,call :: Maybe [C.CallOption]
                             ,liqProvider :: Maybe (Map.Map String CE.LiqFacility)
                             ,rateSwap :: Maybe (Map.Map String HE.RateSwap)
                             ,rateCap :: Maybe (Map.Map String HE.RateCap)
                             ,currencySwap :: Maybe (Map.Map String HE.CurrencySwap)
                             ,custom:: Maybe (Map.Map String CustomDataType)
                             ,triggers :: Maybe (Map.Map DealCycle (Map.Map String Trigger))
                             ,overrides :: Maybe [OverrideType]
                             ,ledgers :: Maybe (Map.Map String LD.Ledger)
                           } deriving (Show,Generic,Eq,Ord)

instance SPV (TestDeal a) where
  getBondsByName t bns
    = case bns of
         Nothing -> bonds t
         Just _bns -> Map.filterWithKey (\k _ -> S.member k (S.fromList _bns)) (bonds t)

  getBondStmtByName t bns
    = Map.map L.bndStmt bndsM
      where
      bndsM = Map.map L.consolStmt $ getBondsByName t bns

  getBondBegBal t bn 
    = case L.bndStmt b of
        Just (Statement []) -> L.bndBalance b -- `debug` ("Getting beg bal"++bn++"Last smt"++show (head stmts))
        Just (Statement stmts) -> getTxnBegBalance $ head stmts -- `debug` ("Getting beg bal"++bn++"Last smt"++show (head stmts))
        Nothing -> L.bndBalance b  -- `debug` ("Getting beg bal nothing"++bn)
        where
            b = bonds t Map.! bn

  getFeeByName t fns
    = case fns of
         Nothing -> fees t
         Just _fns -> Map.filterWithKey (\k _ ->  S.member k (S.fromList _fns)) (fees t)
  
  getAccountByName t ans
    = case ans of
         Nothing -> accounts t
         Just _ans -> Map.filterWithKey (\k _ ->  S.member k (S.fromList _ans)) (accounts t)
  
  isResec t = case pool t of
                 ResecDeal _ -> True
                 _ -> False


dealBonds :: Ast.Asset a => Lens' (TestDeal a) (Map.Map BondName L.Bond)
dealBonds = lens getter setter 
  where 
    getter d = bonds d 
    setter d newBndMap = d {bonds = newBndMap}

dealAccounts :: Ast.Asset a => Lens' (TestDeal a) (Map.Map AccountName A.Account) 
dealAccounts = lens getter setter 
  where 
    getter d = accounts d 
    setter d newAccMap = d {accounts = newAccMap}

dealFees :: Ast.Asset a => Lens' (TestDeal a) (Map.Map FeeName F.Fee) 
dealFees = lens getter setter 
  where 
    getter d = fees d 
    setter d newFeeMap = d {fees = newFeeMap}

dealPool :: Ast.Asset a => Lens' (TestDeal a) (PoolType a)
dealPool = lens getter setter 
  where 
    getter d = pool d
    setter d newPool = d {pool = newPool}

dealScheduledCashflow :: Ast.Asset a => Lens' (TestDeal a) (Map.Map PoolId (Maybe CF.CashFlowFrame))
dealScheduledCashflow = lens getter setter
  where
    getter d = case pool d of
                SoloPool p -> Map.fromList [(PoolConsol,P.futureScheduleCf p)]
                MultiPool pm -> Map.map P.futureScheduleCf pm
                ResecDeal uds -> Map.map futureScheduleCf uds
                x -> error $ "Failed to match :" ++ show x
    setter d newCfMap = case pool d of
                          SoloPool p -> case Map.lookup PoolConsol newCfMap of
                                          Just cf -> set dealPool (SoloPool (p {P.futureScheduleCf = cf})) d
                                          Nothing -> error $ "can't set multi pool cf to a solo pool"
                          MultiPool pm -> let 
                                            newPm = Map.mapWithKey (\k p -> set P.poolFutureScheduleCf (newCfMap Map.! k) p) pm
                                          in
                                            set dealPool (MultiPool newPm) d
                          ResecDeal pm -> 
                            let 
                              newPm = Map.mapWithKey (\k ud -> 
                                                        set uDealFutureScheduleCf (newCfMap Map.! k) ud) pm
                            in
                              set dealPool (ResecDeal newPm) d

dealCashflow :: Ast.Asset a => Lens' (TestDeal a) (Map.Map PoolId (Maybe CF.CashFlowFrame))
dealCashflow = lens getter setter
  where 
    getter d = case pool d of
                SoloPool p -> Map.fromList [(PoolConsol,P.futureCf p)]
                MultiPool pm -> Map.map P.futureCf pm
                ResecDeal uds -> Map.map futureCf uds
    setter d newCfMap = case pool d of 
                          SoloPool p -> case Map.lookup PoolConsol newCfMap of
                                          Just cf -> set dealPool (SoloPool (p {P.futureCf = cf})) d
                                          Nothing -> error $ "can't set multi pool cf to a solo pool"
                          MultiPool pm -> let 
                                            newPm = Map.mapWithKey (\k p -> set P.poolFutureCf (newCfMap Map.! k) p) pm
                                          in 
                                            set dealPool (MultiPool newPm) d
                          ResecDeal pm ->
                            let 
                              newPm = Map.mapWithKey (\k ud -> 
                                                        set uDealFutureCf (newCfMap Map.! k) ud) pm
                            in
                              set dealPool (ResecDeal newPm) d


getPoolIds :: Ast.Asset a => TestDeal a -> [PoolId]
getPoolIds t@TestDeal{pool = pt} 
  = case pt of
      SoloPool _ -> [PoolConsol] 
      MultiPool pm -> Map.keys pm
      ResecDeal pm -> Map.keys pm
      _ -> error "failed to match pool type in pool ids"
                         

getBondByName :: Ast.Asset a => TestDeal a -> BondName -> Maybe L.Bond
getBondByName t bName = Map.lookup bName (bonds t)

-- ^ get issuance pool stat from pool map
getIssuanceStats :: Ast.Asset a => TestDeal a  -> Maybe [PoolId] -> Map.Map PoolId (Map.Map CutoffFields Balance)
getIssuanceStats t@TestDeal{pool = pt} mPoolId
  = case pt of
      ResecDeal uDeals -> 
        let 
          selecteduDeals = case mPoolId of 
                            Nothing -> uDeals
                            Just pns -> Map.filterWithKey (\k _ -> k `elem` pns ) uDeals
        in
          Map.map (fromMaybe Map.empty . issuanceStat) selecteduDeals 
      SoloPool p -> Map.fromList [(PoolConsol, fromMaybe Map.empty (P.issuanceStat p))]
      MultiPool pm -> let 
                        selectedPools = case mPoolId of 
                                          Nothing -> pm
                                          Just pns -> Map.filterWithKey (\k _ -> k `elem` pns ) pm
                      in
                        Map.map (fromMaybe Map.empty . P.issuanceStat) selectedPools

getIssuanceStatsConsol :: Ast.Asset a => TestDeal a -> Maybe [PoolId] -> Map.Map CutoffFields Balance
getIssuanceStatsConsol t mPns 
  = let 
      ms = getIssuanceStats t mPns
    in 
      Map.unionsWith (+) $ Map.elems ms

getAllAsset :: TestDeal a -> Maybe [PoolId] -> Map.Map PoolId [a]
getAllAsset t@TestDeal{pool = pt} mPns = 
  let 
    assetMap = case pt of 
                 SoloPool p -> Map.fromList [(PoolConsol, P.assets p)]
                 MultiPool pm -> Map.map P.assets pm
                 ResecDeal _ -> Map.empty
                 -- ResecDeal pm -> Map.mapWithKey (\(UnderlyingBond (bn,hpct,sd), d) -> getAllAsset d Nothing) pm
  in
    case mPns of 
      Nothing -> assetMap 
      Just pns -> Map.filterWithKey (\k _ -> k `elem` pns ) assetMap
    
getAllAssetList :: Ast.Asset a => TestDeal a -> [a]
getAllAssetList t = concat $ Map.elems (getAllAsset t Nothing)

getAllCollectedFrame :: Ast.Asset a => TestDeal a -> Maybe [PoolId] -> Map.Map PoolId (Maybe CF.CashFlowFrame)
getAllCollectedFrame t mPid = 
  let 
    mCf = view dealCashflow t
  in 
    case mPid of 
      Nothing -> mCf -- `debug` ("Nothing when collecting cfs"++show mCf)
      Just pids -> Map.filterWithKey (\k _ -> k `elem` pids) mCf -- `debug` ("Just when collecting cfs"++show mCf)

getLatestCollectFrame :: Ast.Asset a => TestDeal a -> Maybe [PoolId] -> Map.Map PoolId (Maybe CF.TsRow)
getLatestCollectFrame t mPns = Map.map (last . view CF.cashflowTxn <$>) (getAllCollectedFrame t mPns)

getAllCollectedTxns :: Ast.Asset a => TestDeal a -> Maybe [PoolId] -> Map.Map PoolId (Maybe [CF.TsRow])
getAllCollectedTxns t mPns = Map.map (view CF.cashflowTxn <$>) (getAllCollectedFrame t mPns)

getAllCollectedTxnsList :: Ast.Asset a => TestDeal a -> Maybe [PoolId] -> [CF.TsRow]
getAllCollectedTxnsList t mPns 
  = concat $ fromMaybe [] <$>  listOfTxns
    where 
      listOfTxns = Map.elems $ getAllCollectedTxns t mPns


data UnderBond b = UnderBond BondName Rate (TestDeal b)


$(concat <$> traverse (deriveJSON defaultOptions) [''TestDeal, ''UnderlyingDeal, ''PoolType])
-- $(deriveJSON defaultOptions ''UnderlyingDeal)
-- $(deriveJSON defaultOptions ''PoolType)
-- $(deriveJSON defaultOptions ''TestDeal)