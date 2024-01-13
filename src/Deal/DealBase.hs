{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Deal.DealBase (TestDeal(..),SPV(..),dealBonds,dealFees,dealAccounts,dealPool,PoolType(..),getIssuanceStats
                     ,getAllAsset,getAllAssetList,getAllCollectedFrame,getLatestCollectFrame,getAllCollectedTxns
                     ,getIssuanceStatsConsol,getAllCollectedTxnsList,getPoolsByName,getScheduledCashflow,dealScheduledCashflow
                     ,getPoolIds,poolTypePool, UnderlyingBond(..)) 
  where
import qualified Accounts as A
import qualified Ledger as LD
import qualified Asset as P
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
import Asset (poolFutureCf)
import qualified Types as CF

import Debug.Trace
debug = flip trace
-- import Data.Aeson.Types (Parser)
-- import qualified Data.HashMap.Strict as HM
-- import Data.Text (unpack)
-- import Control.Monad.IO.Class (liftIO)

class SPV a where
  getBondByName :: a -> Maybe [String] -> Map.Map String L.Bond
  getBondBegBal :: a -> String -> Balance
  getBondStmtByName :: a -> Maybe [String] -> Map.Map String (Maybe Statement)
  getFeeByName :: a -> Maybe [String] -> Map.Map String F.Fee
  getAccountByName :: a -> Maybe [String] -> Map.Map String A.Account


type HoldingPct = Rate
-- type UnderlyingHolding =(BondName, HoldingPct, Date)

newtype UnderlyingBond = UnderlyingBond (BondName,HoldingPct,Date)
                         deriving (Generic,Eq,Ord)

data PoolType a = SoloPool (P.Pool a)
                | MultiPool (Map.Map PoolId (P.Pool a))
                | ResecDeal (Map.Map UnderlyingBond (TestDeal a))
                deriving (Generic,Eq,Ord,Show)

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

instance Read UnderlyingBond where 
  -- readsPrec _ "UnderlyingBond" = [(UnderlyingBond ("",0,0),"")]
  readsPrec _ str =
    case T.splitOn "_" (T.pack str) of
      [bn, hp, d] -> [(UnderlyingBond (T.unpack bn, read (T.unpack hp)::Rational, read (T.unpack d)::Date), "")] `debug` ("Read success" )
      _ -> [] `debug` ("read not match with "++ str)
      --[bn, hp, d] -> case (reads (T.unpack hp), reads (T.unpack d)) of
      --                 ((hpVal, _):_, (dVal, _):_) -> [(UnderlyingBond (T.unpack bn, hpVal, dVal), "")] `debug` ("Read success")
      --                 _ -> [] `debug` ("read not match with "++ show (T.splitOn "_" (T.pack str)))

instance Show UnderlyingBond where 
  show (UnderlyingBond (bn,hp,d)) = bn ++ "_" ++ show hp ++ "_" ++ show d


instance ToJSONKey UnderlyingBond where 
  toJSONKey :: ToJSONKeyFunction UnderlyingBond
  toJSONKey = toJSONKeyText $ \(UnderlyingBond (bn,hp,d)) -> T.pack $ bn ++ "_" ++ show hp ++ "_" ++ show d

instance FromJSONKey UnderlyingBond where
--   fromJSONKey = FromJSONKeyTextParser $ \case
--     "name" -> pure $ (,) <$> parseJSONKey <*> parseJSONKey <*> parseJSONKey
--     _ -> fail "Expected \"name\" key"
     fromJSONKey = FromJSONKeyTextParser $ 
       \t -> case readMaybe (T.unpack t) of
               Just k -> pure k   `debug` ("parsed with "++ show k)
               Nothing -> fail ("Invalid key: " ++ show t++">>"++ show (T.unpack t))


-- buildPoolIdFromDeal ::  P.Asset a => PoolType a -> Map.Map (BondName, HoldingPct, Date) PoolId
-- buildPoolIdFromDeal (ResecDeal resecM) 
--   = Map.foldrWithKey 
--       (\(bn,hp,d) deal m 
--          -> Map.insert (bn,hp,d) (UnderlyingDeal (name deal) bn) m) 
--       Map.empty
--       resecM
-- 
-- buildPoolIdFromDeal _ = error "Not implemented for non-resec deal"


poolTypePool :: P.Asset a => Lens' (PoolType a) (Map.Map PoolId (P.Pool a))
poolTypePool = lens getter setter
  where 
    getter (SoloPool p) = Map.fromList [(PoolConsol,p)]
    getter (MultiPool pm) = pm
    setter (SoloPool p) newPool = case Map.lookup PoolConsol newPool of
                                    Just p -> SoloPool p
                                    Nothing -> error $ "Can't set a solo pool to a multi pool"
    setter (MultiPool pm) newPool = MultiPool newPool

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
  getBondByName t bns
    = case bns of
         Nothing -> bonds t
         Just _bns -> Map.filterWithKey (\k _ -> S.member k (S.fromList _bns)) (bonds t)

  getBondStmtByName t bns
    = Map.map L.bndStmt bndsM
      where
      bndsM = Map.map L.consolStmt $ getBondByName t bns

  getBondBegBal t bn 
    = case L.bndStmt b of
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

dealBonds :: P.Asset a => Lens' (TestDeal a) (Map.Map BondName L.Bond)
dealBonds = lens getter setter 
  where 
    getter d = bonds d 
    setter d newBndMap = d {bonds = newBndMap}

dealAccounts :: P.Asset a => Lens' (TestDeal a) (Map.Map AccountName A.Account) 
dealAccounts = lens getter setter 
  where 
    getter d = accounts d 
    setter d newAccMap = d {accounts = newAccMap}

dealFees :: P.Asset a => Lens' (TestDeal a) (Map.Map FeeName F.Fee) 
dealFees = lens getter setter 
  where 
    getter d = fees d 
    setter d newFeeMap = d {fees = newFeeMap}

dealPool :: P.Asset a => Lens' (TestDeal a) (PoolType a)
dealPool = lens getter setter 
  where 
    getter d = pool d
    setter d newPool = d {pool = newPool}

dealScheduledCashflow :: P.Asset a => Lens' (TestDeal a) (Map.Map PoolId (Maybe CF.CashFlowFrame))
dealScheduledCashflow = lens getter setter
  where
    getter d = case pool d of
                 SoloPool p -> Map.fromList [(PoolConsol,P.futureScheduleCf p)]
                 MultiPool pm -> Map.map P.futureScheduleCf pm
    setter d newCfMap =  case pool d of
                           SoloPool p -> case Map.lookup PoolConsol newCfMap of
                                           Just cf -> set dealPool (SoloPool (p {P.futureScheduleCf = cf})) d
                                           Nothing -> error $ "can't set multi pool cf to a solo pool"
                           MultiPool pm -> let 
                                             newPm = Map.mapWithKey (\k p -> set P.poolFutureScheduleCf (newCfMap Map.! k) p) pm
                                           in
                                             set dealPool (MultiPool newPm) d

getPoolIds :: P.Asset a => TestDeal a -> [PoolId]
getPoolIds t@TestDeal{pool = pt} 
  = case pt of
      SoloPool _ -> [PoolConsol]
      MultiPool pm -> Map.keys pm
      -- ResecDeal pm -> [PoolConsol] --TODO 
                         


-- ^ get issuance pool stat from pool map
getIssuanceStats :: P.Asset a => TestDeal a  -> Maybe [PoolId] -> Map.Map PoolId (Map.Map CutoffFields Balance)
getIssuanceStats t@TestDeal{pool = pt} mPoolId
  = let 
      selectedPools = getPoolsByName t mPoolId 
    in
      Map.map (fromMaybe Map.empty . P.issuanceStat) selectedPools

getIssuanceStatsConsol :: P.Asset a => TestDeal a -> Maybe [PoolId] -> Map.Map CutoffFields Balance
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
                 ResecDeal pm -> Map.mapWithKey 
                                   (\(UnderlyingBond (bn,hpct,sd), d)
                                      ->  L.scaleBond hpct $ (bonds d Map.! bn))
                                   pm 
  in
    case mPns of 
      Nothing -> assetMap 
      Just pns -> Map.filterWithKey (\k _ -> k `elem` pns ) assetMap
    
getAllAssetList :: P.Asset a => TestDeal a -> [a]
getAllAssetList t = concat $ Map.elems (getAllAsset t Nothing)

getPoolsByName :: P.Asset a => TestDeal a -> Maybe [PoolId] -> Map.Map PoolId (P.Pool a)
getPoolsByName TestDeal{pool = (SoloPool p)} Nothing = Map.fromList [(PoolConsol,p)]
getPoolsByName TestDeal{pool = (MultiPool pm)} Nothing = pm
getPoolsByName t@TestDeal{pool = (SoloPool p ),name = n } (Just [PoolConsol]) = Map.fromList [(PoolConsol,p)]
getPoolsByName t@TestDeal{pool = (SoloPool _ ),name = n } (Just pNames) =  error $ "Can't lookup"++ show pNames ++"In a Solo Pool deal"++ show (pool t)
getPoolsByName TestDeal{pool = (MultiPool pm )} (Just pNames) = Map.filterWithKey (\k _ -> k `elem` pNames) pm

getAllCollectedFrame :: P.Asset a => TestDeal a -> Maybe [PoolId] -> Map.Map PoolId (Maybe CF.CashFlowFrame)
getAllCollectedFrame t@TestDeal{pool = pt} mPns = Map.map P.futureCf $ getPoolsByName t mPns 

getScheduledCashflow :: P.Asset a => TestDeal a -> Maybe [PoolId] -> Map.Map PoolId (Maybe CF.CashFlowFrame)
getScheduledCashflow t@TestDeal{pool = pt} mPns = Map.map P.futureScheduleCf $ getPoolsByName t mPns 

getLatestCollectFrame :: P.Asset a => TestDeal a -> Maybe [PoolId] -> Map.Map PoolId (Maybe CF.TsRow)
getLatestCollectFrame t mPns = Map.map (last . view CF.cashflowTxn <$>) (getAllCollectedFrame t mPns)

getAllCollectedTxns :: P.Asset a => TestDeal a -> Maybe [PoolId] -> Map.Map PoolId (Maybe [CF.TsRow])
getAllCollectedTxns t mPns = Map.map (view CF.cashflowTxn <$>) (getAllCollectedFrame t mPns)

getAllCollectedTxnsList :: P.Asset a => TestDeal a -> Maybe [PoolId] -> [CF.TsRow]
getAllCollectedTxnsList t mPns 
  = concat $ fromMaybe [] <$>  listOfTxns
    where 
      listOfTxns = Map.elems $ getAllCollectedTxns t mPns


$(deriveJSON defaultOptions ''UnderlyingBond)
$(deriveJSON defaultOptions ''PoolType)
$(deriveJSON defaultOptions ''TestDeal)

baseCase = TestDeal {
  name = "base case"
  ,status = Amortizing
  ,rateSwap = Nothing
  ,currencySwap = Nothing
  ,dates = PatternInterval $ 
               (Map.fromList [
                (ClosingDate,((T.fromGregorian 2022 1 1),MonthFirst,(toDate "20300101")))
                ,(CutoffDate,((T.fromGregorian 2022 1 1),MonthFirst,(toDate "20300101")))
                ,(FirstPayDate,((T.fromGregorian 2022 2 25),DayOfMonth 25,(toDate "20300101")))
               ])
  ,accounts = (Map.fromList
  [("General", (A.Account { A.accName="General" ,A.accBalance=1000.0 ,A.accType=Nothing, A.accInterest=Nothing ,A.accStmt=Nothing }))])
  ,fees = Map.empty
  ,bonds = (Map.fromList [("A"
                             ,L.Bond{
                              L.bndName="A"
                             ,L.bndType=L.Sequential
                             ,L.bndOriginInfo= L.OriginalInfo{
                                                L.originBalance=3000
                                                ,L.originDate= (T.fromGregorian 2022 1 1)
                                                ,L.originRate= 0.08
                                                ,L.maturityDate = Nothing}
                             ,L.bndInterestInfo= L.Fix 0.08 DC_ACT_365F
                             ,L.bndBalance=3000
                             ,L.bndRate=0.08
                             ,L.bndStepUp=Nothing
                             ,L.bndDuePrin=0.0
                             ,L.bndDueInt=0.0
                             ,L.bndDueIntDate=Nothing
                             ,L.bndLastIntPay = Just (T.fromGregorian 2022 1 1)
                             ,L.bndLastPrinPay = Just (T.fromGregorian 2022 1 1)
                             ,L.bndStmt=Nothing})
                         ]
           )
  ,pool = SoloPool (P.Pool {P.assets=[ACM.Mortgage
                                         ACM.MortgageOriginalInfo{
                                           ACM.originBalance=4000
                                           ,ACM.originRate=IR.Fix DC_ACT_365F 0.085
                                           ,ACM.originTerm=60
                                           ,ACM.period=Monthly
                                           ,ACM.startDate=T.fromGregorian 2022 1 1
                                           ,ACM.prinType= ACM.Level
                                           ,ACM.prepaymentPenalty = Nothing}
                                         4000
                                         0.085
                                         60
                                         Nothing
                                         ACM.Current]
                               ,P.futureCf=Just (CF.CashFlowFrame [])
                               ,P.futureScheduleCf=Just (CF.CashFlowFrame [])
                               ,P.asOfDate = T.fromGregorian 2022 1 1
                               ,P.issuanceStat = Nothing
                               ,P.extendPeriods = Nothing})
   ,waterfall = Map.fromList [(W.DistributionDay Amortizing, [
                                 (W.PayInt Nothing "General" ["A"] Nothing)
                                 ,(W.PayPrin Nothing "General" ["A"] Nothing)
   ])]
 ,collects = [W.Collect Nothing W.CollectedInterest "General"
             ,W.Collect Nothing W.CollectedPrincipal "General"]
 ,call = Nothing
 ,liqProvider = Nothing
 ,ledgers = Nothing
 ,rateCap = Nothing
 ,custom = Nothing
 ,triggers = Nothing
 ,overrides = Nothing
}

resecDeal = TestDeal {
  name = "Top Deal"
  ,status = Amortizing
  ,rateSwap = Nothing
  ,currencySwap = Nothing
  ,dates = PatternInterval $ 
               (Map.fromList [
                (ClosingDate,((T.fromGregorian 2022 1 1),MonthFirst,(toDate "20300101")))
                ,(CutoffDate,((T.fromGregorian 2022 1 1),MonthFirst,(toDate "20300101")))
                ,(FirstPayDate,((T.fromGregorian 2022 2 25),DayOfMonth 25,(toDate "20300101")))
               ])
  ,accounts = (Map.fromList
  [("General", (A.Account { A.accName="General" ,A.accBalance=1000.0 ,A.accType=Nothing, A.accInterest=Nothing ,A.accStmt=Nothing }))])
  ,fees = Map.empty
  ,bonds = (Map.fromList [("A"
                             ,L.Bond{
                              L.bndName="A"
                             ,L.bndType=L.Sequential
                             ,L.bndOriginInfo= L.OriginalInfo{
                                                L.originBalance=3000
                                                ,L.originDate= (T.fromGregorian 2022 1 1)
                                                ,L.originRate= 0.08
                                                ,L.maturityDate = Nothing}
                             ,L.bndInterestInfo= L.Fix 0.08 DC_ACT_365F
                             ,L.bndStepUp=Nothing
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
  ,pool = ResecDeal (Map.fromList [(UnderlyingBond ("A1",0.3,(toDate "20220201")), baseCase)])
   ,waterfall = Map.fromList [(W.DistributionDay Amortizing, [
                                 (W.PayInt Nothing "General" ["A"] Nothing)
                                 ,(W.PayPrin Nothing "General" ["A"] Nothing)
   ])]
 ,collects = [W.Collect Nothing W.CollectedInterest "General"
             ,W.Collect Nothing W.CollectedPrincipal "General"]
 ,call = Nothing
 ,liqProvider = Nothing
 ,ledgers = Nothing
 ,rateCap = Nothing
 ,custom = Nothing
 ,triggers = Nothing
 ,overrides = Nothing
}