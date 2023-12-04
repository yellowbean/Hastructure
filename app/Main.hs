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
--import Data.Swagger
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
import Network.Wai.Middleware.Servant.Errors (errorMw, HasErrorBody(..),errorMwDefJson)
import qualified Data.Aeson.Parser
import Language.Haskell.TH

--import Data.OpenApi hiding(Server) 
import Servant.OpenApi
import Servant
import Servant.Types.SourceT (source)
import Servant.API.ContentTypes (contentType)

import Types 
import qualified Deal as D
import qualified Deal.DealBase as DB
import qualified Asset as P
import qualified Expense as F
import qualified Ledger as LD
import qualified AssetClass.Installment 
import qualified AssetClass.Mortgage 
import qualified AssetClass.Loan 
import qualified AssetClass.Lease 
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


import Debug.Trace
debug = flip Debug.Trace.trace

data Version = Version 
  { _version :: String 
  } deriving (Eq, Show, Generic)

$(deriveJSON defaultOptions ''Version)
instance ToSchema Version

version1 :: Version 
version1 = Version "0.23.4"

data PoolType = MPool (P.Pool AB.Mortgage)
              | LPool (P.Pool AB.Loan)
              | IPool (P.Pool AB.Installment)
              | RPool (P.Pool AB.Lease)
              | FPool (P.Pool AB.FixedAsset)
              deriving(Show, Generic)

instance ToSchema PoolType
$(deriveJSON defaultOptions ''PoolType)

instance ToSchema (P.Pool AB.Mortgage)
instance ToSchema (P.Pool AB.Loan)
instance ToSchema (P.Pool AB.Installment)
instance ToSchema (P.Pool AB.Lease)
instance ToSchema (P.Pool AB.FixedAsset)

data DealType = MDeal (DB.TestDeal AB.Mortgage)
              | LDeal (DB.TestDeal AB.Loan)
              | IDeal (DB.TestDeal AB.Installment) 
              | RDeal (DB.TestDeal AB.Lease) 
              | FDeal (DB.TestDeal AB.FixedAsset) 
              deriving(Show, Generic)

instance ToSchema Ts
instance ToSchema ResultComponent
instance ToSchema CF.CashFlowFrame
instance ToSchema AP.ApplyAssumptionType
instance ToSchema AP.BondPricingInput
instance ToSchema L.PriceResult

instance ToSchema DealType

instance ToSchema AB.Mortgage
instance ToSchema IR.ARM
instance ToSchema AB.Loan
instance ToSchema AB.Installment
instance ToSchema AB.Lease
instance ToSchema AB.FixedAsset

instance ToSchema (DB.TestDeal AB.Mortgage)
instance ToSchema (DB.TestDeal AB.Loan)
instance ToSchema (DB.TestDeal AB.Installment)
instance ToSchema (DB.TestDeal AB.Lease)
instance ToSchema (DB.TestDeal AB.FixedAsset)
instance ToSchema (DB.PoolType AB.FixedAsset)
instance ToSchema (DB.PoolType AB.Mortgage)
instance ToSchema (DB.PoolType AB.Loan)
instance ToSchema (DB.PoolType AB.Installment)
instance ToSchema (DB.PoolType AB.Lease)
instance ToSchema HE.RateCap
instance ToSchema AB.LeaseStepUp 
instance ToSchema AB.AccrualPeriod
instance ToSchema AB.PrepayPenaltyType
instance ToSchema DateDesp
instance ToSchema DateType
instance ToSchema LD.Ledger
instance ToSchema A.Account
instance ToSchema A.InterestInfo
instance ToSchema A.ReserveAmount
instance ToSchema F.Fee
instance ToSchema F.FeeType
instance ToSchema L.Bond
instance ToSchema L.StepUp
instance ToSchema L.BondType
instance ToSchema L.OriginalInfo
instance ToSchema L.InterestInfo
instance ToSchema Pre
instance ToSchema W.ActionWhen
instance ToSchema W.ExtraSupport
instance ToSchema W.Action
instance ToSchema W.BookType
instance ToSchema W.CollectionRule
instance ToSchema Limit
instance ToSchema C.CallOption
instance ToSchema CE.LiqFacility
instance ToSchema HE.RateSwap
instance ToSchema HE.RateSwapType
instance ToSchema HE.RateSwapBase
instance ToSchema HE.CurrencySwap
instance ToSchema CE.LiqSupportType
instance ToSchema CE.LiqRepayType
instance ToSchema CE.LiqDrawType
instance ToSchema CustomDataType
instance ToSchema DealCycle
instance ToSchema TRG.Trigger
instance ToSchema TRG.TriggerEffect
instance ToSchema OverrideType
instance ToSchema ActionOnDate
instance ToSchema DealStats
instance ToSchema Period
instance ToSchema PoolId
instance ToSchema DayCount
instance ToSchema DealStatus
instance ToSchema DatePattern
instance ToSchema Cmp
instance ToSchema Types.Index
instance ToSchema Types.BalanceSheetReport
instance ToSchema Types.CashflowReport
instance ToSchema Types.BookItem
instance ToSchema Stmt.Statement
instance ToSchema Stmt.Txn
instance ToSchema Stmt.Direction
instance ToSchema Stmt.TxnComment
instance ToSchema CF.TsRow
instance ToSchema (TsPoint Balance)
instance ToSchema (TsPoint IRate)
instance ToSchema (TsPoint Rational)
instance ToSchema (TsPoint Bool)
instance ToSchema AB.Status
instance ToSchema AB.AmortRule
instance ToSchema AB.Capacity
instance ToSchema AB.AssociateExp
instance ToSchema AB.AssociateIncome
instance ToSchema AB.OriginalInfo
instance ToSchema IR.RateType
instance ToSchema AB.AmortPlan
instance ToSchema AB.AssetUnion
instance ToSchema CutoffFields
instance ToSchema PricingMethod
instance ToSchema RV.RevolvingPool
instance ToSchema (TsPoint [AB.AssetUnion])
instance ToSchema (RoundingBy IRate)
instance ToSchema (RoundingBy Balance)
instance ToSchema AP.NonPerfAssumption
instance ToSchema AP.RevolvingAssumption
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
instance ToSchema (Table Balance Balance)

instance ToSchema (Ratio Integer) where 
  declareNamedSchema _ = NamedSchema Nothing <$> declareSchema (Proxy :: Proxy Double)

instance ToSchema PoolSource
instance ToSchema Threshold

type RunResp = (DealType , Maybe (Map.Map PoolId CF.CashFlowFrame), Maybe [ResultComponent],Maybe (Map.Map String L.PriceResult))

wrapRun :: DealType -> Maybe AP.ApplyAssumptionType -> AP.NonPerfAssumption -> RunResp
wrapRun (MDeal d) mAssump mNonPerfAssump = let 
                                       (_d,_pflow,_rs,_p) = D.runDeal d D.DealPoolFlowPricing mAssump mNonPerfAssump 
                                     in 
                                       (MDeal _d,_pflow,_rs,_p) -- `debug` ("Run Done with deal->"++ show _d)
wrapRun (RDeal d) mAssump mNonPerfAssump = let 
                                       (_d,_pflow,_rs,_p) = D.runDeal d D.DealPoolFlowPricing mAssump mNonPerfAssump
                                     in 
                                       (RDeal _d,_pflow,_rs,_p)
wrapRun (IDeal d) mAssump mNonPerfAssump = let 
                                       (_d,_pflow,_rs,_p) = D.runDeal d D.DealPoolFlowPricing mAssump mNonPerfAssump
                                     in 
                                       (IDeal _d,_pflow,_rs,_p)
wrapRun (LDeal d) mAssump mNonPerfAssump = let 
                                       (_d,_pflow,_rs,_p) = D.runDeal d D.DealPoolFlowPricing mAssump mNonPerfAssump
                                     in 
                                       (LDeal _d,_pflow,_rs,_p)
wrapRun (FDeal d) mAssump mNonPerfAssump = let 
                                       (_d,_pflow,_rs,_p) = D.runDeal d D.DealPoolFlowPricing mAssump mNonPerfAssump
                                     in 
                                       (FDeal _d,_pflow,_rs,_p)
wrapRun x _ _ = error $ "RunDeal Failed ,due to unsupport deal type "++ show x

wrapRunPool :: PoolType -> Maybe AP.ApplyAssumptionType -> Maybe [RateAssumption] -> (CF.CashFlowFrame, Map CutoffFields Balance)
wrapRunPool (MPool p) assump mRates = P.aggPool Nothing $ D.runPool p assump mRates
wrapRunPool (LPool p) assump mRates = P.aggPool Nothing $ D.runPool p assump mRates
wrapRunPool (IPool p) assump mRates = P.aggPool Nothing $ D.runPool p assump mRates
wrapRunPool (RPool p) assump mRates = P.aggPool Nothing $ D.runPool p assump mRates
wrapRunPool x _ _ = error $ "RunPool Failed ,due to unsupport pool type "++ show x

data RunAssetReq = RunAssetReq Date [AB.AssetUnion] AP.ApplyAssumptionType (Maybe [RateAssumption]) (Maybe PricingMethod)
                   deriving(Show, Generic)

instance ToSchema RunAssetReq

wrapRunAsset :: RunAssetReq -> ((CF.CashFlowFrame, Map.Map CutoffFields Balance), Maybe [PriceResult])
wrapRunAsset (RunAssetReq d assets (AP.PoolLevel assumps) mRates Nothing) 
  = (P.aggPool Nothing ((\a -> D.projAssetUnion a d assumps mRates) <$> assets), Nothing) 

wrapRunAsset (RunAssetReq d assets (AP.PoolLevel assumps) mRates (Just pm)) 
  = let 
      assetCf = P.aggPool Nothing $ (\a -> D.projAssetUnion a d assumps mRates ) <$> assets 
      pricingResult = (\a -> D.priceAssetUnion a d pm assumps mRates) <$> assets
    in
      (assetCf , Just pricingResult)

--TODO implement on running via ByIndex

type ScenarioName = String

data RunDealReq = SingleRunReq DealType (Maybe AP.ApplyAssumptionType) AP.NonPerfAssumption
                | MultiScenarioRunReq DealType (Map.Map ScenarioName AP.ApplyAssumptionType) AP.NonPerfAssumption
                | MultiDealRunReq (Map.Map ScenarioName DealType) (Maybe AP.ApplyAssumptionType) AP.NonPerfAssumption
                deriving(Show, Generic)

instance ToSchema RunDealReq
data RunPoolReq = SingleRunPoolReq PoolType (Maybe AP.ApplyAssumptionType) (Maybe [RateAssumption])
                | MultiScenarioRunPoolReq PoolType (Map.Map ScenarioName AP.ApplyAssumptionType) (Maybe [RateAssumption])
                deriving(Show, Generic)

instance ToSchema RunPoolReq

data RunDateReq = RunDateReq Date DatePattern
                deriving(Show, Generic)
instance ToSchema RunDateReq

$(deriveJSON defaultOptions ''RunDealReq)
$(deriveJSON defaultOptions ''RunPoolReq)
$(deriveJSON defaultOptions ''RunAssetReq)
$(deriveJSON defaultOptions ''RunDateReq)

type EngineAPI = "version" :> Get '[JSON] Version
            :<|> "runAsset" :> ReqBody '[JSON] RunAssetReq :> Post '[JSON] ((CF.CashFlowFrame, Map.Map CutoffFields Balance),Maybe [PriceResult])
            :<|> "runPool" :> ReqBody '[JSON] RunPoolReq :> Post '[JSON] (CF.CashFlowFrame, Map.Map CutoffFields Balance)
            :<|> "runPoolByScenarios" :> ReqBody '[JSON] RunPoolReq :> Post '[JSON] (Map.Map ScenarioName (CF.CashFlowFrame,Map.Map CutoffFields Balance))
            :<|> "runDeal" :> ReqBody '[JSON] RunDealReq :> Post '[JSON] RunResp
            :<|> "runDealByScenarios" :> ReqBody '[JSON] RunDealReq :> Post '[JSON] (Map.Map ScenarioName RunResp)
            :<|> "runMultiDeals" :> ReqBody '[JSON] RunDealReq :> Post '[JSON] (Map.Map ScenarioName RunResp)
            :<|> "runDate" :> ReqBody '[JSON] RunDateReq :> Post '[JSON] [Date]


engineAPI :: Proxy EngineAPI
engineAPI = Proxy


-- Swagger API
type SwaggerAPI = "swagger.json" :> Get '[JSON] OpenApi
type API = SwaggerAPI :<|> EngineAPI

engineSwagger :: OpenApi
engineSwagger = toOpenApi engineAPI 
  & info.title .~ "Hastructure API"
  & info.version .~  T.pack (_version version1)
  & info.description ?~ "Hastructure is a white-label friendly Cashflow & Analytics Engine for MBS/ABS and REITs"
  & info.license ?~ "BSD 3"

myServer :: Server API
myServer = return engineSwagger 
      :<|> showVersion 
      :<|> runAsset
      :<|> runPool
      :<|> runPoolScenarios
      :<|> runDeal
      :<|> runDealScenarios
      :<|> runMultiDeals
      :<|> runDate
--      :<|> error "not implemented"
        where 
          showVersion = return version1 
          runAsset req = return $ wrapRunAsset req
          runPool (SingleRunPoolReq pt passumption mRates) = return $ wrapRunPool pt passumption mRates
          runPoolScenarios (MultiScenarioRunPoolReq pt mAssumps mRates) = return $ Map.map (\assump -> wrapRunPool pt (Just assump) mRates) mAssumps
          runDeal (SingleRunReq dt assump nonPerfAssump) = return $ wrapRun dt assump nonPerfAssump 
          runDealScenarios (MultiScenarioRunReq dt mAssumps nonPerfAssump)
            = return $ Map.map (\singleAssump -> wrapRun dt (Just singleAssump) nonPerfAssump) mAssumps
          runMultiDeals (MultiDealRunReq mDts assump nonPerfAssump) 
            = return $ Map.map (\singleDealType -> wrapRun singleDealType assump nonPerfAssump) mDts
          runDate (RunDateReq sd dp)
            = return $ DU.genSerialDatesTill2 IE sd dp (Lib.toDate "20990101")


writeSwaggerJSON :: IO ()
writeSwaggerJSON = BL8.writeFile "swagger.json" (encodePretty engineSwagger)

data Config = Config { port :: Int} 
            deriving (Show,Generic)

instance FromJSON Config

-- data Ctyp a
-- 
-- {-
--  if you are using GHC 8.6 and above you can make use of deriving Via
--  for creating the Accept Instance
-- 
--  >> data Ctyp a
--  >>   deriving Accept via JSON
-- -}
-- 
-- instance Accept (Ctyp JSON) where
--   contentType _ = contentType (Proxy @JSON)
-- 
-- instance HasErrorBody (Ctyp JSON) '[] where
--   encodeError = undefined -- write your custom implementation
-- 
-- -- | Example Middleware with a different 'HasErrorBody' instance for JSON
-- errorMwJson :: Application -> Application
-- errorMwJson =  errorMw @(Ctyp JSON) @'[]

--instance HasErrorBody (Ctyp JSON) '["error","warning","resp"] where
--  encodeError = error

main :: IO ()
main = 
  do 
    writeSwaggerJSON
    config <- BS.readFile "config.yml"
    let mc = Y.decodeEither' config :: Either ParseException Config
    let (Config _p) = case mc of
                        Left exp -> Config 8081
                        Right c -> c
    run _p 
      -- $ errorMwJson @JSON @'["error","warning","status"]
      $ errorMwDefJson
      $ serve (Proxy :: Proxy API) myServer

$(deriveJSON defaultOptions ''DealType)
