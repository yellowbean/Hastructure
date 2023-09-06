{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
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
import Data.OpenApi hiding (Server)
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
import Network.Wai.Middleware.Servant.Errors (errorMwDefJson, HasErrorBody(..))
import qualified Data.Aeson.Parser
import Language.Haskell.TH

--import Data.OpenApi hiding(Server) 
import Servant.OpenApi
import Servant
import Servant.Types.SourceT (source)

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


import Debug.Trace
debug = flip Debug.Trace.trace

data Version = Version 
  { _version :: String 
  } deriving (Eq, Show, Generic)

$(deriveJSON defaultOptions ''Version)
instance ToSchema Version

version1 :: Version 
version1 = Version "0.19.15"

data PoolType = MPool (P.Pool AB.Mortgage)
              | LPool (P.Pool AB.Loan)
              | IPool (P.Pool AB.Installment)
              | RPool (P.Pool AB.Lease)
              deriving(Show, Generic)

instance ToSchema PoolType
$(deriveJSON defaultOptions ''PoolType)

instance ToSchema (P.Pool AB.Mortgage)
instance ToSchema (P.Pool AB.Loan)
instance ToSchema (P.Pool AB.Installment)
instance ToSchema (P.Pool AB.Lease)

data DealType = MDeal (DB.TestDeal AB.Mortgage)
              | LDeal (DB.TestDeal AB.Loan)
              | IDeal (DB.TestDeal AB.Installment) 
              | RDeal (DB.TestDeal AB.Lease) 
              deriving(Show, Generic)

instance ToSchema Ts
instance ToSchema ResultComponent
instance ToSchema CF.CashFlowFrame
instance ToSchema AP.ApplyAssumptionType
instance ToSchema AP.BondPricingInput
instance ToSchema L.PriceResult

instance ToSchema DealType
$(deriveJSON defaultOptions ''DealType)

instance ToSchema AB.Mortgage
instance ToSchema IR.ARM
instance ToSchema AB.Loan
instance ToSchema AB.Installment
instance ToSchema AB.Lease

instance ToSchema (DB.TestDeal AB.Mortgage)
instance ToSchema (DB.TestDeal AB.Loan)
instance ToSchema (DB.TestDeal AB.Installment)
instance ToSchema (DB.TestDeal AB.Lease)
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
instance ToSchema L.BondType
instance ToSchema L.OriginalInfo
instance ToSchema L.InterestInfo
instance ToSchema Pre
instance ToSchema W.ActionWhen
instance ToSchema W.Action
instance ToSchema W.BookLedgerType
instance ToSchema W.CollectionRule
instance ToSchema Limit
instance ToSchema W.Satisfy
instance ToSchema C.CallOption
instance ToSchema CE.LiqFacility
instance ToSchema HE.RateSwap
instance ToSchema HE.RateSwapType
instance ToSchema HE.RateSwapBase
instance ToSchema HE.CurrencySwap
instance ToSchema CE.LiqSupportType
instance ToSchema CE.LiqRepayType
instance ToSchema CustomDataType
instance ToSchema DealCycle
instance ToSchema TRG.Trigger
instance ToSchema TRG.TriggerEffect
instance ToSchema OverrideType
instance ToSchema ActionOnDate
instance ToSchema DealStats
instance ToSchema Period
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
instance ToSchema FormulaType
instance ToSchema AP.AssumptionBuilder
instance ToSchema CF.TsRow
instance ToSchema (TsPoint Balance)
instance ToSchema (TsPoint IRate)
instance ToSchema (TsPoint Rational)
instance ToSchema (TsPoint Bool)
instance ToSchema AB.Status
instance ToSchema AB.OriginalInfo
instance ToSchema IR.RateType
instance ToSchema AB.AmortPlan
instance ToSchema AB.AssetUnion
instance ToSchema IssuanceFields
instance ToSchema PricingMethod
instance ToSchema RV.RevolvingPool
instance ToSchema (TsPoint [AB.AssetUnion])
instance ToSchema (RoundingBy IRate)
instance ToSchema (RoundingBy Balance)

instance ToSchema (Ratio Integer) where 
  declareNamedSchema _ = NamedSchema Nothing <$> declareSchema (Proxy :: Proxy Double)

instance ToSchema PoolSource
instance ToSchema Threshold

type RunResp = (DealType , Maybe CF.CashFlowFrame, Maybe [ResultComponent],Maybe (Map.Map String L.PriceResult))

wrapRun :: DealType -> Maybe AP.ApplyAssumptionType -> Maybe AP.BondPricingInput -> RunResp
wrapRun (MDeal d) mAssump mPricing = let 
                                       (_d,_pflow,_rs,_p) = D.runDeal d D.DealPoolFlowPricing mAssump mPricing 
                                     in 
                                       (MDeal _d,_pflow,_rs,_p) -- `debug` ("Run Done with deal->"++ show _d)
wrapRun (RDeal d) mAssump mPricing = let 
                                       (_d,_pflow,_rs,_p) = D.runDeal d D.DealPoolFlowPricing mAssump mPricing
                                     in 
                                       (RDeal _d,_pflow,_rs,_p)
wrapRun (IDeal d) mAssump mPricing = let 
                                       (_d,_pflow,_rs,_p) = D.runDeal d D.DealPoolFlowPricing mAssump mPricing
                                     in 
                                       (IDeal _d,_pflow,_rs,_p)
wrapRun (LDeal d) mAssump mPricing = let 
                                       (_d,_pflow,_rs,_p) = D.runDeal d D.DealPoolFlowPricing mAssump mPricing
                                     in 
                                       (LDeal _d,_pflow,_rs,_p)

wrapRunPool :: PoolType -> Maybe AP.ApplyAssumptionType -> CF.CashFlowFrame
wrapRunPool (MPool p) assump = P.aggPool $ D.runPool2 p assump
wrapRunPool (LPool p) assump = P.aggPool $ D.runPool2 p assump
wrapRunPool (IPool p) assump = P.aggPool $ D.runPool2 p assump
wrapRunPool (RPool p) assump = P.aggPool $ D.runPool2 p assump

data RunAssetReq = RunAssetReq Date [AB.AssetUnion] AP.ApplyAssumptionType (Maybe PricingMethod)
                   deriving(Show, Generic)
instance ToSchema RunAssetReq

wrapRunAsset :: RunAssetReq -> (CF.CashFlowFrame, Maybe [PriceResult])
wrapRunAsset (RunAssetReq d assets (AP.PoolLevel assumps) Nothing) 
  = (P.aggPool $ (\a -> D.projAssetUnion a d assumps) <$> assets, Nothing)
wrapRunAsset (RunAssetReq d assets (AP.PoolLevel assumps) (Just pm)) 
  = let 
      assetCf = P.aggPool $ (\a -> D.projAssetUnion a d assumps) <$> assets 
      pricingResult = (\a -> D.priceAssetUnion a d pm assumps) <$> assets
    in 
      (assetCf, Just pricingResult)

type ScenarioName = String
data RunDealReq = SingleRunReq DealType (Maybe AP.ApplyAssumptionType) (Maybe AP.BondPricingInput)
                | MultiScenarioRunReq DealType (Map.Map ScenarioName AP.ApplyAssumptionType) (Maybe AP.BondPricingInput)
                | MultiDealRunReq (Map.Map ScenarioName DealType) (Maybe AP.ApplyAssumptionType) (Maybe AP.BondPricingInput)
                deriving(Show, Generic)

instance ToSchema RunDealReq
data RunPoolReq = SingleRunPoolReq PoolType (Maybe AP.ApplyAssumptionType)
                | MultiScenarioRunPoolReq PoolType (Map.Map ScenarioName AP.ApplyAssumptionType)
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
            :<|> "runAsset" :> ReqBody '[JSON] RunAssetReq :> Post '[JSON] (CF.CashFlowFrame,Maybe [PriceResult])
            :<|> "runPool" :> ReqBody '[JSON] RunPoolReq :> Post '[JSON] CF.CashFlowFrame
            :<|> "runPoolByScenarios" :> ReqBody '[JSON] RunPoolReq :> Post '[JSON] (Map.Map ScenarioName CF.CashFlowFrame)
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
  & info.license ?~ ("BSD 3")

server2 :: Server API
server2 = return engineSwagger 
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
          runPool (SingleRunPoolReq pt passumption) = return $ wrapRunPool pt passumption
          runPoolScenarios (MultiScenarioRunPoolReq pt mAssumps) = return $ Map.map (\assump -> wrapRunPool pt (Just assump)) mAssumps
          runDeal (SingleRunReq dt assump pricing) = let
                                                       resp = wrapRun dt assump pricing 
                                                     in 
                                                       return resp  
          runDealScenarios (MultiScenarioRunReq dt mAssumps pricing)
            = return $ Map.map (\singleAssump -> wrapRun dt (Just singleAssump) pricing) mAssumps
          runMultiDeals (MultiDealRunReq mDts assump pricing) 
            = return $ Map.map (\singleDealType -> wrapRun singleDealType assump pricing) mDts
          runDate (RunDateReq sd dp)
            = return $ U.genSerialDatesTill2 IE sd dp (Lib.toDate "20990101")


writeSwaggerJSON :: IO ()
writeSwaggerJSON = BL8.writeFile "swagger.json" (encodePretty engineSwagger)

data Config = Config { port :: Int} deriving (Show,Generic)
instance FromJSON Config

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
      $ errorMwDefJson
      $ serve (Proxy :: Proxy API) server2
