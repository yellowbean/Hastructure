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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Main 
  where 

import Prelude ()
import Prelude.Compat
import System.Environment
import Control.Monad.Catch       (MonadCatch, MonadThrow (..))
import Control.Monad.IO.Class    (liftIO)
import Control.Monad (mapM)
import Control.Exception (Exception,throwIO,throw)
import Control.Monad.Except
import Control.Monad.Reader
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
import MainBase
import qualified Deal as D
import qualified Deal.DealBase as DB
import qualified Deal.DealMod as DM
import qualified Deal.DealQuery as Q
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
import Data.Scientific (fromRationalRepetend,formatScientific, Scientific,FPFormat(Fixed))
import Control.Lens
import qualified Types as W
import Cashflow (patchCumulative)

import Numeric.RootFinding

import Debug.Trace
debug = flip Debug.Trace.trace


version1 :: Version 
version1 = Version "0.45.7"


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

patchCumulativeToPoolRun :: RunPoolTypeRtn_ -> RunPoolTypeRtn_
patchCumulativeToPoolRun
  = Map.map
          (\(CF.CashFlowFrame _ txns,stats) -> 
            (CF.CashFlowFrame (0,Lib.toDate "19000101",Nothing) (CF.patchCumulative (0,0,0,0,0,0) [] txns),stats))

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

-- Swagger API
type SwaggerAPI = "swagger.json" :> Get '[JSON] OpenApi
type EngineAPI = "version" :> Get '[JSON] Version
            :<|> "runAsset" :> ReqBody '[JSON] RunAssetReq :> Post '[JSON] RunAssetResp
            :<|> "runPool" :> ReqBody '[JSON] RunPoolReq :> Post '[JSON] PoolRunResp
            :<|> "runPoolByScenarios" :> ReqBody '[JSON] RunPoolReq :> Post '[JSON] (Map.Map ScenarioName PoolRunResp)
            :<|> "runDeal" :> ReqBody '[JSON] RunDealReq :> Post '[JSON] RunResp
            :<|> "runDealByScenarios" :> ReqBody '[JSON] RunDealReq :> Post '[JSON] (Map.Map ScenarioName RunResp)
            :<|> "runMultiDeals" :> ReqBody '[JSON] RunDealReq :> Post '[JSON] (Map.Map ScenarioName RunResp)
            :<|> "runDealByRunScenarios" :> ReqBody '[JSON] RunDealReq :> Post '[JSON] (Map.Map ScenarioName RunResp)
            :<|> "runByCombo" :> ReqBody '[JSON] RunDealReq :> Post '[JSON] (Map.Map String RunResp)
            :<|> "runByRootFinder" :> ReqBody '[JSON] RootFindReq :> Post '[JSON] (Either String RootFindResp)
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
runAsset req = return $ wrapRunAsset req

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
runDeal (SingleRunReq dt assump nonPerfAssump) 
  = return $ wrapRun dt assump nonPerfAssump

stressAssetPerf :: Rate -> AP.AssetPerfAssumption -> AP.AssetPerfAssumption
stressAssetPerf r (AP.MortgageAssump (Just da) mp mr ms) 
  = AP.MortgageAssump (Just (AP.stressDefaultAssump r da)) mp mr ms
stressAssetPerf r (AP.LoanAssump (Just da) mp mr ms) 
  = AP.LoanAssump (Just (AP.stressDefaultAssump r da)) mp mr ms 
stressAssetPerf r (AP.InstallmentAssump (Just da) mp mr ms) 
  = AP.InstallmentAssump (Just (AP.stressDefaultAssump r da)) mp mr ms
stressAssetPerf r (AP.ReceivableAssump (Just da) mr ms) 
  = AP.ReceivableAssump (Just (AP.stressDefaultAssump r da)) mr ms
stressAssetPerf _ x = x

stressRevovlingPerf :: Rate -> Maybe AP.RevolvingAssumption -> Maybe AP.RevolvingAssumption
stressRevovlingPerf r Nothing = Nothing
stressRevovlingPerf r (Just (AP.AvailableAssets rp applyAssumpType)) 
  = Just (AP.AvailableAssets rp (over (AP.applyAssumptionTypeAssetPerf . _1) (stressAssetPerf r) applyAssumpType))
stressRevovlingPerf r (Just (AP.AvailableAssetsBy m))
  = Just (AP.AvailableAssetsBy (Map.map (over (_2 . AP.applyAssumptionTypeAssetPerf . _1) (stressAssetPerf r)) m))

modifyDealType :: DM.ModifyType -> Double -> DealType -> DealType
modifyDealType dm f (MDeal d) = MDeal $ DM.modDeal dm f d
modifyDealType dm f (RDeal d) = RDeal $ DM.modDeal dm f d
modifyDealType dm f (IDeal d) = IDeal $ DM.modDeal dm f d
modifyDealType dm f (LDeal d) = LDeal $ DM.modDeal dm f d
modifyDealType dm f (FDeal d) = FDeal $ DM.modDeal dm f d
modifyDealType dm f (UDeal d) = UDeal $ DM.modDeal dm f d
modifyDealType dm f (VDeal d) = VDeal $ DM.modDeal dm f d
modifyDealType dm f (PDeal d) = PDeal $ DM.modDeal dm f d

queryDealType :: DealType -> Date -> DealStats -> Either String Rational
queryDealType (MDeal _d) = Q.queryCompound _d 
queryDealType (RDeal _d) = Q.queryCompound _d 
queryDealType (IDeal _d) = Q.queryCompound _d
queryDealType (LDeal _d) = Q.queryCompound _d
queryDealType (FDeal _d) = Q.queryCompound _d
queryDealType (UDeal _d) = Q.queryCompound _d
queryDealType (VDeal _d) = Q.queryCompound _d
queryDealType (PDeal _d) = Q.queryCompound _d

queryDealTypeBool :: DealType -> Date -> DealStats -> Either String Bool
queryDealTypeBool (MDeal _d) d s = Q.queryDealBool _d s d
queryDealTypeBool (RDeal _d) d s = Q.queryDealBool _d s d
queryDealTypeBool (IDeal _d) d s = Q.queryDealBool _d s d
queryDealTypeBool (LDeal _d) d s = Q.queryDealBool _d s d
queryDealTypeBool (FDeal _d) d s = Q.queryDealBool _d s d
queryDealTypeBool (UDeal _d) d s = Q.queryDealBool _d s d
queryDealTypeBool (VDeal _d) d s = Q.queryDealBool _d s d
queryDealTypeBool (PDeal _d) d s = Q.queryDealBool _d s d

getDealBondMap :: DealType -> Map.Map BondName L.Bond
getDealBondMap (MDeal d) = DB.bonds d
getDealBondMap (RDeal d) = DB.bonds d
getDealBondMap (IDeal d) = DB.bonds d
getDealBondMap (LDeal d) = DB.bonds d
getDealBondMap (FDeal d) = DB.bonds d
getDealBondMap (UDeal d) = DB.bonds d
getDealBondMap (VDeal d) = DB.bonds d
getDealBondMap (PDeal d) = DB.bonds d

getDealFeeMap :: DealType -> Map.Map FeeName F.Fee
getDealFeeMap (MDeal d) = DB.fees d
getDealFeeMap (RDeal d) = DB.fees d
getDealFeeMap (IDeal d) = DB.fees d
getDealFeeMap (LDeal d) = DB.fees d
getDealFeeMap (FDeal d) = DB.fees d
getDealFeeMap (UDeal d) = DB.fees d
getDealFeeMap (VDeal d) = DB.fees d
getDealFeeMap (PDeal d) = DB.fees d

-- stress the pool performance, till a bond suffers first loss
testByDefault :: DealType -> AP.ApplyAssumptionType -> AP.NonPerfAssumption -> BondName -> Double -> Double
testByDefault dt assumps nonPerfAssump@AP.NonPerfAssumption{AP.revolving = mRevolving} bn r 
  = let 
      stressed = over (AP.applyAssumptionTypeAssetPerf . _1 ) (stressAssetPerf (toRational r)) assumps
      stressedNonPerf = nonPerfAssump {AP.revolving = stressRevovlingPerf (toRational r) mRevolving }
      runResult = wrapRun dt (Just stressed) stressedNonPerf -- `debug` ("running stress "++ show stressed)
    in
      case runResult of 
        Right (d,mPoolCfMap,mResult,mPricing) -> 
          let 
            bondBal = L.getOutstandingAmount $ (getDealBondMap d) Map.! bn
          in
            (fromRational (toRational bondBal) - 0.01) -- `debug` (">>> test run result"++ show (fromRational (toRational bondBal) - 0.01))
        Left errorMsg -> error $ "Error in test fun for first loss" ++ show errorMsg


-- add spread to bonds till PV of bond (discounted by pricing assumption) equals to face value
-- with constraint that all liabilities are paid off
testBySpread :: DealRunInput -> (BondName,Bool,Bool) -> Double -> Double
testBySpread (dt,mPAssump,runAssump) (bn,otherBondFlag,otherFeeFlag) f 
  = let
      runResult = wrapRun (modifyDealType (DM.AddSpreadToBonds bn) f dt) mPAssump runAssump
    in 
      case runResult of 
        Right (dt, mPoolCfMap, mResult, pResult) -> 
          let 
            -- bnds
            otherBondsName = [] 
            -- check fees/other bonds
            otherBondOustanding True = sum $ L.getOutstandingAmount <$> Map.elems (getDealBondMap dt)
            otherBondOustanding False = 0.0
            feeOutstanding True = sum $ L.getOutstandingAmount <$> Map.elems (getDealFeeMap dt)
            feeOutstanding False = 0.0 
            v = getPriceValue $ pResult Map.! bn
            bondBal = L.getOriginBalance $ (getDealBondMap dt) Map.! bn
          in
            if (otherBondOustanding otherBondFlag+feeOutstanding otherFeeFlag) > 0  then 
              -1
            else
              (fromRational . toRational) $ bondBal - v -- `debug` ("rate"++ show f ++ "bondBal:"++ show bondBal++"v:"++ show v)
        Left errorMsg -> error $ "Error in test fun for spread testing" ++ show errorMsg

runRootFinderBy :: RootFindReq -> Handler (Either String RootFindResp)
runRootFinderBy (FirstLossReq (dt,Just assumps,nonPerfAssump@AP.NonPerfAssumption{AP.revolving = mRevolving}) bn)
  = return $
      let 
        itertimes = 500
        def = RiddersParam { riddersMaxIter = itertimes, riddersTol = RelTol 0.0001}
      in
        case ridders def (500.0,0.00) (testByDefault dt assumps nonPerfAssump bn) of
          Root r -> Right $
                      FirstLossResult
                        r
                        (over (AP.applyAssumptionTypeAssetPerf . _1 ) (stressAssetPerf (toRational r)) assumps)
                        (stressRevovlingPerf (toRational r) mRevolving)
          NotBracketed -> Left "Not able to bracket the root"
          SearchFailed -> Left "Not able to find the root"

runRootFinderBy (MaxSpreadToFaceReq (dt,pAssump,dAssump) bns chkOtherBnds chkOtherFees) 
  = return $
      let 
        itertimes = 500
        def = RiddersParam { riddersMaxIter = itertimes, riddersTol = RelTol 0.0001}
      in 
        case ridders def (0.00,200.0) (testBySpread (dt,pAssump,dAssump) (bns,chkOtherBnds,chkOtherFees)) of
          Root r -> let 
                      dt' = modifyDealType (DM.AddSpreadToBonds bns) r dt
                    in 
                      Right $ BestSpreadResult r (getDealBondMap dt') dt' 
          NotBracketed -> Left "Not able to bracket the root"
          SearchFailed -> Left "Not able to find the root"

runDealScenarios :: RunDealReq -> Handler (Map.Map ScenarioName RunResp)
runDealScenarios (MultiScenarioRunReq dt mAssumps nonPerfAssump) 
  = return $ Map.map (\singleAssump -> wrapRun dt (Just singleAssump) nonPerfAssump) mAssumps

runMultiDeals :: RunDealReq -> Handler (Map.Map ScenarioName RunResp)
runMultiDeals (MultiDealRunReq mDts assump nonPerfAssump) 
  = return $ Map.map (\singleDealType -> wrapRun singleDealType assump nonPerfAssump) mDts

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
      return rMap

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
      :<|> runRootFinderBy
      :<|> runDate


writeSwaggerJSON :: IO ()
writeSwaggerJSON = BL8.writeFile "swagger.json" (encodePretty engineSwagger)

data Config = Config { port :: Int} 
            deriving (Show,Generic)

instance FromJSON Config

app :: Application
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