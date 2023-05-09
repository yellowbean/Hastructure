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
import qualified Asset as P
import qualified Expense as F
import qualified AssetClass.Installment as AC_Installment
import qualified AssetClass.Mortgage as AC_Mortgage
import qualified AssetClass.Loan as AC_Loan
import qualified AssetClass.Lease as AC_Lease
import qualified Assumptions as AP
import qualified Cashflow as CF
import qualified Accounts as A
import qualified Revolving 
import qualified Liability as L
import qualified Call as C
import qualified CreditEnhancement as CE
import qualified Waterfall as W
import qualified InterestRate as IR
import qualified Stmt
import qualified Triggers as TRG


import Debug.Trace
debug = flip Debug.Trace.trace

data Version = Version 
  { _version :: String 
  } deriving (Eq, Show, Generic)

$(deriveJSON defaultOptions ''Version)
instance ToSchema Version

version1 :: Version 
version1 = Version "0.16.0"

data PoolType = MPool (P.Pool AC_Mortgage.Mortgage)
              | LPool (P.Pool AC_Loan.Loan)
              | IPool (P.Pool AC_Installment.Installment)
              | RPool (P.Pool AC_Lease.Lease)
              deriving(Show, Generic)

instance ToSchema PoolType
$(deriveJSON defaultOptions ''PoolType)

instance ToSchema (P.Pool AC_Mortgage.Mortgage)
instance ToSchema (P.Pool AC_Loan.Loan)
instance ToSchema (P.Pool AC_Installment.Installment)
instance ToSchema (P.Pool AC_Lease.Lease)

data DealType = MDeal (D.TestDeal AC_Mortgage.Mortgage)
              | LDeal (D.TestDeal AC_Loan.Loan)
              | IDeal (D.TestDeal AC_Installment.Installment) 
              | RDeal (D.TestDeal AC_Lease.Lease) 
              deriving(Show, Generic)


instance ToSchema Ts
instance ToSchema ResultComponent
instance ToSchema CF.CashFlowFrame
instance ToSchema AP.ApplyAssumptionType
instance ToSchema AP.BondPricingInput
instance ToSchema L.PriceResult

instance ToSchema DealType
$(deriveJSON defaultOptions ''DealType)

instance ToSchema AC_Mortgage.Mortgage
instance ToSchema IR.ARM
instance ToSchema AC_Loan.Loan
instance ToSchema AC_Installment.Installment
instance ToSchema AC_Lease.Lease

instance ToSchema (D.TestDeal AC_Mortgage.Mortgage)
instance ToSchema (D.TestDeal AC_Loan.Loan)
instance ToSchema AC_Lease.LeaseInfo 
instance ToSchema AC_Lease.LeaseStepUp 
instance ToSchema AC_Lease.AccrualPeriod
instance ToSchema (D.TestDeal AC_Installment.Installment)
instance ToSchema DateDesp
instance ToSchema DateType
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
instance ToSchema W.CollectionRule
instance ToSchema W.Limit
instance ToSchema W.Satisfy
instance ToSchema C.CallOption
instance ToSchema CE.LiqFacility
instance ToSchema CE.RateSwap
instance ToSchema CE.RateSwapType
instance ToSchema CE.RateSwapBase
instance ToSchema CE.CurrencySwap
instance ToSchema CE.LiqSupportType
instance ToSchema CE.LiqSupportRate
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
instance ToSchema (D.TestDeal AC_Lease.Lease)
instance ToSchema DealStatus
instance ToSchema DatePattern
instance ToSchema Cmp
instance ToSchema Types.Index
instance ToSchema Stmt.Statement
instance ToSchema Stmt.Txn
instance ToSchema Stmt.TxnComment
instance ToSchema FormulaType
instance ToSchema AP.AssumptionBuilder
instance ToSchema CF.TsRow
instance ToSchema (TsPoint Balance)
instance ToSchema (TsPoint IRate)
instance ToSchema (TsPoint Rational)
instance ToSchema (TsPoint Bool)
instance ToSchema Revolving.LiquidationMethod
instance ToSchema P.Status
instance ToSchema P.OriginalInfo
instance ToSchema IR.RateType
instance ToSchema P.AmortPlan
instance ToSchema P.IssuanceFields

--instance ToSchema (Ratio Integer)
--instance ToSchema (Ratio Integer) where 
--  declareNamedSchema _ = do 
--      integerSchema <- declareSchemaRef (Proxy :: Proxy Integer)
--      return $ NamedSchema (Just "RatioInteger") $ mempty 
--        & type_ ?~ OpenApiObject 
--        & properties .~ 
--            [ ("numerator", integerSchema)
--            , ("denomiator", integerSchema)]
--        & required .~ ["numerator","denominator"]
instance ToSchema (Ratio Integer) where 
  declareNamedSchema _ = NamedSchema Nothing <$> declareSchema (Proxy :: Proxy Double)

instance ToSchema PoolSource
instance ToSchema Threshold

type RunResp = (DealType , Maybe CF.CashFlowFrame, Maybe [ResultComponent],Maybe (Map.Map String L.PriceResult))

wrapRun :: DealType -> Maybe AP.ApplyAssumptionType -> Maybe AP.BondPricingInput -> RunResp
wrapRun (MDeal d) mAssump mPricing = let 
                    (_d,_pflow,_rs,_p) = D.runDeal d D.DealPoolFlowPricing mAssump mPricing
                     in 
                                 (MDeal _d,_pflow,_rs,_p)
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
$(deriveJSON defaultOptions ''RunDealReq)
$(deriveJSON defaultOptions ''RunPoolReq)

type EngineAPI = "version" :> Get '[JSON] Version
            :<|> "runPool" :> ReqBody '[JSON] RunPoolReq :> Post '[JSON] CF.CashFlowFrame
            :<|> "runPoolByScenarios" :> ReqBody '[JSON] RunPoolReq :> Post '[JSON] (Map.Map ScenarioName CF.CashFlowFrame)
            :<|> "runDeal" :> ReqBody '[JSON] RunDealReq :> Post '[JSON] RunResp
            :<|> "runDealByScenarios" :> ReqBody '[JSON] RunDealReq :> Post '[JSON] (Map.Map ScenarioName RunResp)
            :<|> "runMultiDeals" :> ReqBody '[JSON] RunDealReq :> Post '[JSON] (Map.Map ScenarioName RunResp)


engineAPI :: Proxy EngineAPI
engineAPI = Proxy


-- Swagger API
type SwaggerAPI = "swagger.json" :> Get '[JSON] OpenApi
type API = SwaggerAPI :<|> EngineAPI

-- todo swagger 
engineSwagger :: OpenApi
engineSwagger = toOpenApi engineAPI 
  & info.title .~ "Hastructure API"
  & info.version .~  T.pack (_version version1)
  & info.description ?~ "Hastructure is a white-label friendly Cashflow & Analytics Engine for MBS/ABS and REITs"
  & info.license ?~ ("BSD 3")

server2 :: Server API
server2 = return engineSwagger 
      :<|> showVersion 
      :<|> runPool
      :<|> runPoolScenarios
      :<|> runDeal
      :<|> runDealScenarios
      :<|> runMultiDeals
--      :<|> error "not implemented"
        where 
          showVersion = return version1 
          runPool (SingleRunPoolReq pt passumption) = return $ wrapRunPool pt passumption
          runPoolScenarios (MultiScenarioRunPoolReq pt mAssumps) = return $ Map.map (\assump -> wrapRunPool pt (Just assump)) mAssumps
          runDeal (SingleRunReq dt assump pricing) = return $ wrapRun dt assump pricing
          runDealScenarios (MultiScenarioRunReq dt mAssumps pricing)
            = return $ Map.map (\singleAssump -> wrapRun dt (Just singleAssump) pricing) mAssumps
          runMultiDeals (MultiDealRunReq mDts assump pricing) 
            = return $ Map.map (\singleDealType -> wrapRun singleDealType assump pricing) mDts


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
