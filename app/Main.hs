{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies     #-}
--{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE LiberalTypeSynonyms     #-}
module Main where

import Data.Aeson hiding (json)
import GHC.Generics
import Data.Monoid      ((<>))
import Data.Text        (Text, pack)
import Data.Yaml as Y
import qualified Deal as D
import qualified Asset as P
import qualified Assumptions as AP

import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as BS

import qualified Data.Map as Map 

import Data.Aeson hiding (json)
import Language.Haskell.TH
import Data.Maybe
import Data.Aeson.TH
import Data.Aeson.Types

import Yesod
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types
import Network.Wai.Middleware.Cors

import Debug.Trace
debug = flip trace

data RunDealReq assetType = RunDealReq {
  deal :: D.TestDeal assetType
  ,assump :: Maybe AP.AssumptionInput
  ,bondPricing :: Maybe AP.BondPricingInput
} deriving (Show, Generic)

instance FromJSON assetType => FromJSON (RunDealReq assetType)
instance ToJSON assetType => ToJSON (RunDealReq assetType)

 -- $(deriveJSON defaultOptions ''RunDealReq)

data ReqType = MortgageDeal (RunDealReq P.Mortgage)
             | LoanDeal (RunDealReq P.Loan)
             deriving (Show)

$(deriveJSON defaultOptions ''ReqType)


--getAssump :: ReqType -> Maybe AP.AssumptionInput
--getAssump (MortgageDeal r) = (assump r)
--getAssump (LoanDeal r) = (assump r)
--
--data RunPoolReq = RunPoolReq {
--  pool :: P.Pool P.Mortgage
--  ,poolAggRule :: P.AggregationRule
--  ,poolAssump :: Maybe [AP.AssumptionBuilder]
--}

data App = App

mkYesod "App" [parseRoutes|
 /run_deal RunDealR POST OPTIONS
 /version VersionR GET OPTIONS
|]
 
 -- /run_pool RunPoolR POST OPTIONS

instance Yesod App where
  yesodMiddleware = defaultYesodMiddleware

optionsRunDealR :: Handler String 
optionsRunDealR = do
  addHeader "Access-Control-Allow-Origin" "*"
  addHeader "Access-Control-Allow-Methods" "OPTIONS"
  return "Good"

postRunDealR :: Handler Value
postRunDealR =  do
  -- runReq <- requireCheckJsonBody  -- :: Handler ReqType
  (RunDealReq d a p) <- requireCheckJsonBody :: Handler (RunDealReq P.Mortgage)
  case a of
    Just (AP.Single aps) -> returnJson $
                               D.runDeal 
                                d 
                                 D.DealPoolFlowPricing 
                                 (Just aps) 
                                 p

    Nothing -> returnJson $
                 D.runDeal 
                   d 
                   D.DealPoolFlowPricing 
                   Nothing 
                   p

    Just (AP.Multiple apsm) -> 
        returnJson $
          Map.map 
            (\x -> D.runDeal 
                     d 
                     D.DealPoolFlowPricing 
                     (Just x) 
                     p)
             apsm


--optionsRunPoolR :: Handler String
--optionsRunPoolR = do
--  addHeader "Access-Control-Allow-Origin" "*"
--  addHeader "Access-Control-Allow-Methods" "OPTIONS"
--  return "Good"
--
--postRunPoolR :: Handler Value -- D.TestDeal
--postRunPoolR =  do
--  runReq <- requireCheckJsonBody :: Handler RunPoolReq
--  returnJson $
--     P.projPoolCFs
--        (pool runReq)
--        (fromMaybe [] (poolAssump runReq))
--        (poolAggRule runReq)
--
getVersionR :: Handler String
getVersionR =  do
  addHeader "Access-Control-Allow-Origin" "*"
  addHeader "Access-Control-Allow-Methods" "GET"
  return "{\"version\":\"0.3.1\"}"

optionsVersionR :: Handler String 
optionsVersionR = do
  addHeader "Access-Control-Allow-Origin" "*"
  addHeader "Access-Control-Allow-Methods" "OPTIONS"
  return "Good"


data Config = Config { port :: Int}
            deriving  (Show,Generic)
instance FromJSON Config

main :: IO ()
main =
  do
   config <- BS.readFile "config.yml"
   let mc = Y.decodeEither' config :: Either ParseException Config
   let (Config _p) = case mc of
                     Left exp -> Config 8081
                     Right c -> c
   app <- toWaiApp App
   run _p $ defaultMiddlewaresNoLogging
            $ cors (const $ Just $ simpleCorsResourcePolicy
                                    { corsOrigins = Nothing
                                    , corsMethods = ["OPTIONS", "GET", "PUT", "POST"]
                                    , corsRequestHeaders = simpleHeaders })
            app


-- $(deriveJSON defaultOptions ''(RunDealReq P.Mortgage))
-- $(deriveJSON defaultOptions ''(RunDealReq a))
-- $(deriveJSON defaultOptions ''(RunDealReq2 a))
-- $(deriveJSON defaultOptions ''(RunDealReq P.ConsumerCredit))
-- $(deriveJSON defaultOptions ''(RunDealReq2 P.Mortgage))
-- $(deriveJSON defaultOptions ''RunPoolReq )
