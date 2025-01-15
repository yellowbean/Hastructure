{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where 

import Prelude ()
import Prelude.Compat
import System.Environment

import Control.Monad.Catch       (MonadCatch, MonadThrow (..))
import Control.Monad.IO.Class    (liftIO)
import Control.Monad (mapM)
import Control.Exception (Exception,throwIO,throw)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Lens
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.TH
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.List
import Data.Map
import Data.Proxy
import qualified Data.Text as T
import Data.Maybe
import Data.Yaml as Y
import Data.OpenApi hiding (Server,contentType)
import qualified Data.Map as Map
import Data.String.Conversions
import Data.Time.Calendar
import GHC.Generics
import GHC.Real
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.ByteString.Char8 as BS
import Lucid hiding (type_)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import qualified Data.Aeson.Parser
import Language.Haskell.TH

import Network.HTTP.Types.Status

import Servant.OpenApi
import Servant
import Servant.Types.SourceT (source)
import Servant.API.ContentTypes (contentType)


import Types 
import qualified Deal as D
import qualified Deal.DealBase as DB
import qualified Asset as Ast
import qualified Pool as P
import qualified Expense as F
import qualified Ledger as LD
import qualified AssetClass.Installment 
import qualified AssetClass.Mortgage 
import qualified AssetClass.Loan 
import qualified AssetClass.Lease 
import qualified AssetClass.ProjectedCashFlow
import qualified AssetClass.MixedAsset as MA
import qualified AssetClass.AssetBase as AB 
import qualified Assumptions as AP
import qualified Cashflow as CF
import qualified Accounts as A
import qualified Revolving 
import qualified Liability as L
import qualified Call as C
import qualified CreditEnhancement as CE
import qualified Hedge as HE
import qualified Waterfall as W
import qualified InterestRate as IR
import qualified Stmt
import qualified Triggers as TRG
import qualified Revolving as RV
import qualified Lib
import qualified Util as U
import qualified DateUtil as DU
-- import Servant.Checked.Exceptions (NoThrow, Throws)
-- import Servant.Checked.Exceptions.Internal.Servant.API (ErrStatus(toErrStatus))

import Data.Scientific (fromRationalRepetend,formatScientific, Scientific,FPFormat(Fixed))
import Control.Lens
import qualified Types as W
import Cashflow (patchCumulative)


import Debug.Trace
debug = flip Debug.Trace.trace


-- instance ToJSON (Ratio Integer) where
--     toJSON r = case fromRationalRepetend Nothing r of
--         Left (sci, _)         -> toJSON $ formatScientific Fixed (Just 8) sci
--         -- Right (sci, rep) -> object ["repetend" .= rep, "fraction" .= sci]
--         Right (sci, rep) -> toJSON $ formatScientific Fixed (Just 8) sci


data Version = Version 
  { _version :: String 
  } deriving (Eq, Show, Generic)

$(deriveJSON defaultOptions ''Version)
instance ToSchema Version

version1 :: Version 
version1 = Version "0.41.3"



data DealType = MDeal (DB.TestDeal AB.Mortgage)
              | LDeal (DB.TestDeal AB.Loan)
              | IDeal (DB.TestDeal AB.Installment) 
              | RDeal (DB.TestDeal AB.Lease) 
              | FDeal (DB.TestDeal AB.FixedAsset) 
              | VDeal (DB.TestDeal AB.Receivable)
              | PDeal (DB.TestDeal AB.ProjectedCashflow) 
              | UDeal (DB.TestDeal AB.AssetUnion)
              deriving(Show, Generic)

instance ToSchema CF.CashFlowFrame
instance ToSchema AB.Loan
instance ToSchema AB.Installment
instance ToSchema AB.AccrualPeriod
instance ToSchema AB.LeaseStepUp
instance ToSchema AB.Lease
instance ToSchema AB.FixedAsset
instance ToSchema AB.Receivable
instance ToSchema AB.ProjectedCashflow
instance ToSchema CutoffFields
instance ToSchema (P.Pool AB.Mortgage)
instance ToSchema (P.Pool AB.Loan)
instance ToSchema (P.Pool AB.Installment)
instance ToSchema (P.Pool AB.Lease)
instance ToSchema (P.Pool AB.FixedAsset)
instance ToSchema (P.Pool AB.Receivable)
instance ToSchema (P.Pool AB.AssetUnion)
instance ToSchema (P.Pool AB.ProjectedCashflow)
instance ToSchema AB.AssetUnion
instance ToSchema PoolId
instance ToSchema DealStatus
instance ToSchema DateType
instance ToSchema DB.DateDesp
instance ToSchema DB.ActionOnDate
instance ToSchema DealStats
instance ToSchema Cmp
instance ToSchema PricingMethod
instance ToSchema Stmt.TxnComment
instance ToSchema BookDirection
instance ToSchema Limit
instance ToSchema PoolSource
instance ToSchema (RoundingBy Rate)
instance ToSchema (RoundingBy Integer)
instance ToSchema (RoundingBy Balance)
instance ToSchema DealCycle
instance ToSchema (Table Balance Balance)
instance ToSchema (Table Float Spread)
instance ToSchema A.Account
instance ToSchema A.InterestInfo
instance ToSchema F.Fee
instance ToSchema F.FeeType
instance ToSchema HE.RateCap
instance ToSchema LD.Ledger
instance ToSchema A.ReserveAmount
instance ToSchema L.Bond
instance ToSchema L.StepUp
instance ToSchema L.BondType
instance ToSchema L.OriginalInfo
instance ToSchema L.InterestInfo
instance ToSchema L.InterestOverInterestType
instance ToSchema Pre
instance ToSchema W.PayOrderBy
instance ToSchema W.ActionWhen
instance ToSchema W.ExtraSupport
instance ToSchema W.Action
instance ToSchema W.BookType
instance ToSchema W.CollectionRule
instance ToSchema C.CallOption
instance ToSchema CE.LiqCreditCalc
instance ToSchema CE.LiqFacility
instance ToSchema HE.RateSwap
instance ToSchema HE.RateSwapType
instance ToSchema HE.RateSwapBase
instance ToSchema HE.CurrencySwap
instance ToSchema CE.LiqSupportType
instance ToSchema CE.LiqRepayType
instance ToSchema CE.LiqDrawType
instance ToSchema CustomDataType
instance ToSchema TRG.Trigger
instance ToSchema TRG.TriggerEffect
instance ToSchema DB.OverrideType
instance ToSchema Types.BalanceSheetReport
instance ToSchema Types.CashflowReport
instance ToSchema Types.BookItem
instance ToSchema Stmt.Statement
instance ToSchema Types.Txn
instance ToSchema AB.AssociateExp
instance ToSchema AB.AssociateIncome
instance ToSchema RV.RevolvingPool
instance ToSchema (TsPoint [AB.AssetUnion])
instance ToSchema AP.IssueBondEvent
instance ToSchema (TsPoint AP.IssueBondEvent)
instance ToSchema (TsPoint AP.RefiEvent)
instance ToSchema AP.RefiEvent
instance ToSchema AP.InspectType
instance ToSchema AP.CallOpt
instance ToSchema AP.NonPerfAssumption
instance ToSchema AP.BondPricingInput
instance ToSchema AP.RevolvingAssumption
instance ToSchema AP.TagMatchRule
instance ToSchema RangeType
instance ToSchema AP.FieldMatchRule
instance ToSchema AP.ObligorStrategy
instance ToSchema AP.ApplyAssumptionType
instance ToSchema AP.AssetPerfAssumption
instance ToSchema AP.AssetDelinqPerfAssumption
instance ToSchema AP.AssetDefaultedPerfAssumption
instance ToSchema AP.AssetDefaultAssumption
instance ToSchema AP.AssetPrepayAssumption
instance ToSchema AP.RecoveryAssumption
instance ToSchema RateAssumption
instance ToSchema AP.ExtraStress
instance ToSchema AP.AssetDelinquencyAssumption
instance ToSchema AP.LeaseAssetGapAssump
instance ToSchema AP.LeaseAssetRentAssump

instance ToSchema Threshold

instance ToSchema (DB.TestDeal AB.Mortgage)
instance ToSchema (DB.TestDeal AB.Loan)
instance ToSchema (DB.TestDeal AB.Installment)
instance ToSchema (DB.TestDeal AB.Lease)
instance ToSchema (DB.TestDeal AB.Receivable)
instance ToSchema (DB.TestDeal AB.ProjectedCashflow)
instance ToSchema (DB.TestDeal AB.AssetUnion)
instance ToSchema (DB.TestDeal AB.FixedAsset)

instance ToSchema (DB.PoolType AB.Mortgage)
instance ToSchema (DB.PoolType AB.Loan)
instance ToSchema (DB.PoolType AB.Installment)
instance ToSchema (DB.PoolType AB.Lease)
instance ToSchema (DB.PoolType AB.FixedAsset)
instance ToSchema (DB.PoolType AB.Receivable)
instance ToSchema (DB.PoolType AB.ProjectedCashflow)
instance ToSchema (DB.PoolType AB.AssetUnion)

instance ToSchema (DB.UnderlyingDeal AB.Mortgage)
instance ToSchema (DB.UnderlyingDeal AB.Loan)
instance ToSchema (DB.UnderlyingDeal AB.Installment)
instance ToSchema (DB.UnderlyingDeal AB.Lease)
instance ToSchema (DB.UnderlyingDeal AB.FixedAsset)
instance ToSchema (DB.UnderlyingDeal AB.Receivable)
instance ToSchema (DB.UnderlyingDeal AB.ProjectedCashflow)
instance ToSchema (DB.UnderlyingDeal AB.AssetUnion)

instance ToSchema ResultComponent
instance ToSchema L.PriceResult
instance ToSchema DealType

type RunResp = Either String (DealType , Maybe (Map.Map PoolId CF.CashFlowFrame), Maybe [ResultComponent],Maybe (Map.Map String L.PriceResult))

wrapRun :: DealType -> Maybe AP.ApplyAssumptionType -> AP.NonPerfAssumption -> RunResp
wrapRun (MDeal d) mAssump mNonPerfAssump 
  = do
      (_d,_pflow,_rs,_p) <- D.runDeal d D.DealPoolFlowPricing mAssump mNonPerfAssump 
      return (MDeal _d,_pflow,_rs,_p) -- `debug` ("Run Done with deal->"++ show _d)
wrapRun (RDeal d) mAssump mNonPerfAssump 
  = do 
      (_d,_pflow,_rs,_p) <- D.runDeal d D.DealPoolFlowPricing mAssump mNonPerfAssump
      return (RDeal _d,_pflow,_rs,_p)
wrapRun (IDeal d) mAssump mNonPerfAssump 
  = do
      (_d,_pflow,_rs,_p) <- D.runDeal d D.DealPoolFlowPricing mAssump mNonPerfAssump
      return (IDeal _d,_pflow,_rs,_p)
wrapRun (LDeal d) mAssump mNonPerfAssump 
  = do
      (_d,_pflow,_rs,_p) <- D.runDeal d D.DealPoolFlowPricing mAssump mNonPerfAssump
      return (LDeal _d,_pflow,_rs,_p)
wrapRun (FDeal d) mAssump mNonPerfAssump 
  = do
      (_d,_pflow,_rs,_p) <- D.runDeal d D.DealPoolFlowPricing mAssump mNonPerfAssump
      return (FDeal _d,_pflow,_rs,_p)
wrapRun (UDeal d) mAssump mNonPerfAssump 
  = do
      (_d,_pflow,_rs,_p) <- D.runDeal d D.DealPoolFlowPricing mAssump mNonPerfAssump 
      return (UDeal _d,_pflow,_rs,_p)                                       
wrapRun (VDeal d) mAssump mNonPerfAssump 
  = do
      (_d,_pflow,_rs,_p) <- D.runDeal d D.DealPoolFlowPricing mAssump mNonPerfAssump
      return (VDeal _d,_pflow,_rs,_p)                                       
wrapRun (PDeal d) mAssump mNonPerfAssump 
  = do
      (_d,_pflow,_rs,_p) <- D.runDeal d D.DealPoolFlowPricing mAssump mNonPerfAssump
      return (PDeal _d,_pflow,_rs,_p)

wrapRun x _ _ = Left $ "RunDeal Failed ,due to unsupport deal type "++ show x


data PoolTypeWrap = LPool (DB.PoolType AB.Loan)
                  | IPool (DB.PoolType AB.Installment)
                  | MPool (DB.PoolType AB.Mortgage)
                  | RPool (DB.PoolType AB.Lease)
                  | FPool (DB.PoolType AB.FixedAsset)
                  | VPool (DB.PoolType AB.Receivable)
                  | PPool (DB.PoolType AB.ProjectedCashflow)
                  | UPool (DB.PoolType AB.AssetUnion)
                  deriving(Show, Generic)


type RunPoolTypeRtn_ = Map.Map PoolId (CF.CashFlowFrame, Map.Map CutoffFields Balance)
type RunPoolTypeRtn = Either String RunPoolTypeRtn_

patchCumulativeToPoolRun :: RunPoolTypeRtn_ -> RunPoolTypeRtn_
patchCumulativeToPoolRun
  = Map.map
         (\(CF.CashFlowFrame _ txns,stats) -> 
            (CF.CashFlowFrame (0,Lib.toDate "19000101",Nothing) (CF.patchCumulative (0,0,0,0,0,0) [] txns),stats)
         )

wrapRunPoolType :: PoolTypeWrap -> Maybe AP.ApplyAssumptionType -> Maybe [RateAssumption] -> RunPoolTypeRtn
wrapRunPoolType (MPool pt) assump mRates = D.runPoolType pt assump $ Just (AP.NonPerfAssumption{AP.interest = mRates})
wrapRunPoolType (LPool pt) assump mRates = D.runPoolType pt assump $ Just (AP.NonPerfAssumption{AP.interest = mRates})
wrapRunPoolType (IPool pt) assump mRates = D.runPoolType pt assump $ Just (AP.NonPerfAssumption{AP.interest = mRates})
wrapRunPoolType (RPool pt) assump mRates = D.runPoolType pt assump $ Just (AP.NonPerfAssumption{AP.interest = mRates})
wrapRunPoolType (FPool pt) assump mRates = D.runPoolType pt assump $ Just (AP.NonPerfAssumption{AP.interest = mRates})
wrapRunPoolType (VPool pt) assump mRates = D.runPoolType pt assump $ Just (AP.NonPerfAssumption{AP.interest = mRates})
wrapRunPoolType (PPool pt) assump mRates = D.runPoolType pt assump $ Just (AP.NonPerfAssumption{AP.interest = mRates})
wrapRunPoolType (UPool pt) assump mRates = D.runPoolType pt assump $ Just (AP.NonPerfAssumption{AP.interest = mRates})
wrapRunPoolType x _ _ = Left $ "RunPool Failed ,due to unsupport pool type "++ show x


data RunAssetReq = RunAssetReq Date [AB.AssetUnion] (Maybe AP.ApplyAssumptionType) (Maybe [RateAssumption]) (Maybe PricingMethod)
                   deriving(Show, Generic)

instance ToSchema RunAssetReq

type RunAssetResp = Either String ((CF.CashFlowFrame, Map.Map CutoffFields Balance), Maybe [PriceResult])


wrapRunAsset :: RunAssetReq -> RunAssetResp
wrapRunAsset (RunAssetReq d assets Nothing mRates Nothing) 
  = do 
      cfs <- sequenceA $ (\a -> MA.calcAssetUnion a d mRates) <$> assets
      return (P.aggPool Nothing [(cf,Map.empty) | cf <- cfs], Nothing) 
wrapRunAsset (RunAssetReq d assets (Just (AP.PoolLevel assumps)) mRates Nothing) 
  = do 
      cfs <- sequenceA $ (\a -> MA.projAssetUnion a d assumps mRates) <$> assets
      return (P.aggPool Nothing [(cf,Map.empty) | (cf,_) <- cfs] , Nothing) 

wrapRunAsset (RunAssetReq d assets (Just (AP.PoolLevel assumps)) mRates (Just pm)) 
  = 
    do 
      cfs <- sequenceA $ (\a -> MA.projAssetUnion a d assumps mRates) <$> assets
      pricingResult <- sequenceA $ (\a -> D.priceAssetUnion a d pm assumps mRates) <$> assets
      let assetCf = P.aggPool Nothing cfs
      return (assetCf , Just pricingResult)

--TODO implement on running via ByIndex
type ScenarioName = String

data RunDealReq = SingleRunReq DealType (Maybe AP.ApplyAssumptionType) AP.NonPerfAssumption
                | MultiScenarioRunReq DealType (Map.Map ScenarioName AP.ApplyAssumptionType) AP.NonPerfAssumption --- multi pool perf
                | MultiDealRunReq (Map.Map ScenarioName DealType) (Maybe AP.ApplyAssumptionType) AP.NonPerfAssumption  -- multi deal struct
                | MultiRunAssumpReq DealType (Maybe AP.ApplyAssumptionType) (Map.Map ScenarioName AP.NonPerfAssumption) -- multi run assump 
                | MultiComboReq (Map.Map ScenarioName DealType)  (Map.Map ScenarioName (Maybe AP.ApplyAssumptionType))  (Map.Map ScenarioName AP.NonPerfAssumption)
                deriving(Show, Generic)

data RunSimDealReq = OASReq DealType (Map.Map ScenarioName AP.ApplyAssumptionType) AP.NonPerfAssumption
                    deriving(Show, Generic)

instance ToSchema PoolTypeWrap

instance ToSchema RunDealReq
data RunPoolReq = SingleRunPoolReq PoolTypeWrap (Maybe AP.ApplyAssumptionType) (Maybe [RateAssumption])
                | MultiScenarioRunPoolReq PoolTypeWrap (Map.Map ScenarioName AP.ApplyAssumptionType) (Maybe [RateAssumption])
                deriving(Show, Generic)

instance ToSchema RunPoolReq

data RunDateReq = RunDateReq Date DatePattern (Maybe Date)
                deriving(Show, Generic)
instance ToSchema RunDateReq


$(deriveJSON defaultOptions ''DealType)

$(concat <$> traverse (deriveJSON defaultOptions) [''RunDealReq, ''RunPoolReq,''RunAssetReq, ''RunDateReq,''PoolTypeWrap])

-- Swagger API
type SwaggerAPI = "swagger.json" :> Get '[JSON] OpenApi

type PoolRunResp = Either String (Map.Map PoolId (CF.CashFlowFrame, Map.Map CutoffFields Balance))

type EngineAPI = "version" :> Get '[JSON] Version
            :<|> "runAsset" :> ReqBody '[JSON] RunAssetReq :> Post '[JSON] RunAssetResp
            :<|> "runPool" :> ReqBody '[JSON] RunPoolReq :> Post '[JSON] PoolRunResp
            :<|> "runPoolByScenarios" :> ReqBody '[JSON] RunPoolReq :> Post '[JSON] (Map.Map ScenarioName PoolRunResp)
            :<|> "runDeal" :> ReqBody '[JSON] RunDealReq :> Post '[JSON] RunResp
            :<|> "runDealByScenarios" :> ReqBody '[JSON] RunDealReq :> Post '[JSON] (Map.Map ScenarioName RunResp)
            :<|> "runMultiDeals" :> ReqBody '[JSON] RunDealReq :> Post '[JSON] (Map.Map ScenarioName RunResp)
            :<|> "runDealByRunScenarios" :> ReqBody '[JSON] RunDealReq :> Post '[JSON] (Map.Map ScenarioName RunResp)
            :<|> "runByCombo" :> ReqBody '[JSON] RunDealReq :> Post '[JSON] (Map.Map String RunResp)
            :<|> "runDate" :> ReqBody '[JSON] RunDateReq :> Post '[JSON] [Date]


engineAPI :: Proxy EngineAPI
engineAPI = Proxy


type API = SwaggerAPI :<|> EngineAPI

engineSwagger:: OpenApi
engineSwagger = toOpenApi engineAPI
                    & info.title .~ "Hastructure API"
                    & info.version .~  T.pack (_version version1)
                    & info.description ?~ "Hastructure is a white-label friendly Cashflow & Analytics Engine for MBS/ABS and REITs"
                    & info.license ?~ "BSD 3"


-- showVersion :: Handler (Envelope '[] Version)
showVersion :: Handler Version
showVersion = return version1

runAsset :: RunAssetReq -> Handler RunAssetResp
runAsset req = return $ 
                 wrapRunAsset req

runPool :: RunPoolReq -> Handler PoolRunResp
runPool (SingleRunPoolReq pt passumption mRates) 
  = return $
      patchCumulativeToPoolRun <$> (wrapRunPoolType pt passumption mRates)

runPoolScenarios :: RunPoolReq -> Handler (Map.Map ScenarioName PoolRunResp)
runPoolScenarios (MultiScenarioRunPoolReq pt mAssumps mRates) 
  = return $ Map.map (\assump -> 
                       patchCumulativeToPoolRun <$> (wrapRunPoolType pt (Just assump) mRates)) 
                     mAssumps

runDeal :: RunDealReq -> Handler RunResp
runDeal (SingleRunReq dt assump nonPerfAssump) =  return $ wrapRun dt assump nonPerfAssump

runDealScenarios :: RunDealReq -> Handler (Map.Map ScenarioName RunResp)
runDealScenarios (MultiScenarioRunReq dt mAssumps nonPerfAssump) = return $ Map.map (\singleAssump -> wrapRun dt (Just singleAssump) nonPerfAssump) mAssumps

runMultiDeals :: RunDealReq -> Handler (Map.Map ScenarioName RunResp)
runMultiDeals (MultiDealRunReq mDts assump nonPerfAssump) = return $ Map.map (\singleDealType -> wrapRun singleDealType assump nonPerfAssump) mDts

runDate :: RunDateReq -> Handler [Date]
runDate (RunDateReq sd dp md) = return $ 
                                    case md of
                                      Nothing -> DU.genSerialDatesTill2 IE sd dp (Lib.toDate "20990101")
                                      Just d -> DU.genSerialDatesTill2 IE sd dp d

runDealByRunScenarios :: RunDealReq -> Handler (Map.Map ScenarioName RunResp)
runDealByRunScenarios (MultiRunAssumpReq dt mAssump nonPerfAssumpMap)
  = return $ Map.map (wrapRun dt mAssump) nonPerfAssumpMap


runDealByCombo :: RunDealReq -> Handler (Map.Map String RunResp)
runDealByCombo (MultiComboReq dMap assumpMap nonPerfAssumpMap)
  = let 
      dList = Map.toList dMap
      aList = Map.toList assumpMap
      nList = Map.toList nonPerfAssumpMap
      r = [ (intercalate "^" [dk,ak,nk], wrapRun d a n) | (dk,d) <- dList, (ak,a) <- aList, (nk,n) <- nList ]
      rMap = Map.fromList r
    in 
      return rMap -- `debug` ("RunDealByCombo->"++ show rMap)


myServer :: ServerT API Handler
myServer =  return engineSwagger
      :<|> showVersion 
      :<|> runAsset
      :<|> runPool
      :<|> runPoolScenarios
      :<|> runDeal
      :<|> runDealScenarios
      :<|> runMultiDeals
      :<|> runDealByRunScenarios
      :<|> runDealByCombo
      :<|> runDate


writeSwaggerJSON :: IO ()
writeSwaggerJSON = BL8.writeFile "swagger.json" (encodePretty engineSwagger)

data Config = Config { port :: Int} 
            deriving (Show,Generic)

instance FromJSON Config

app :: Application
-- app = serve (Proxy :: Proxy API) myServer
app = simpleCors $ serve (Proxy :: Proxy API) myServer


main :: IO ()
main = 
  do 
    writeSwaggerJSON
    config <- BS.readFile "config.yml"
    let mc = Y.decodeEither' config :: Either ParseException Config
    let (Config _p) = case mc of
                        Left exp -> Config 8081
                        Right c -> c
    print ("Engine start with version:"++ _version version1++";running at Port:"++ show _p)
    run _p app
-- $(deriveJSON defaultOptions ''DealType)
