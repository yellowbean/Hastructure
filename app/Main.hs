{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Main where

import Data.Aeson hiding (json)
import Data.Monoid      ((<>))
import Data.Text        (Text, pack)
import Data.Yaml as Y
import GHC.Generics
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

data RunDealReq = RunDealReq {
  deal :: D.TestDeal
  ,assump :: Maybe [AP.AssumptionBuilder]
  ,bondPricing :: Maybe AP.BondPricingInput
}

data RunDealReq2 = RunDealReq2 {
  _deal :: D.TestDeal
  ,_assump :: Maybe AP.AssumptionInput
  ,_bondPricing :: Maybe AP.BondPricingInput
}

data RunPoolReq = RunPoolReq {
  pool :: P.Pool P.Mortgage
  ,poolAggRule :: P.AggregationRule
  ,poolAssump :: Maybe [AP.AssumptionBuilder]
}

data App = App

mkYesod "App" [parseRoutes|
 /run_deal2 RunDealR POST OPTIONS
 /run_deal RunDeal2R POST OPTIONS    -- acception both single and multiple scenario run
 /run_pool RunPoolR POST OPTIONS
 /version VersionR GET OPTIONS
|]

instance Yesod App where
  yesodMiddleware = defaultYesodMiddleware

postRunDealR :: Handler Value -- D.TestDeal
postRunDealR =  do
  runReq <- requireCheckJsonBody :: Handler RunDealReq
  returnJson $
      D.runDeal
         (deal runReq)
         D.DealPoolFlowPricing
         (assump runReq)
         (bondPricing runReq) -- `debug` ("Loading Deal with pool>>"++show(D.pool (deal runReq)))

optionsRunDealR :: Handler String 
optionsRunDealR = do
  addHeader "Access-Control-Allow-Origin" "*"
  addHeader "Access-Control-Allow-Methods" "OPTIONS"
  return "Good"

postRunDeal2R :: Handler Value
postRunDeal2R =  do
  runReq <- requireCheckJsonBody :: Handler RunDealReq2
  case _assump runReq of
    Just (AP.Single aps) -> returnJson $
                               D.runDeal 
                                 (_deal runReq) 
                                 D.DealPoolFlowPricing 
                                 (Just aps) 
                                 (_bondPricing runReq)

    Nothing -> returnJson $
                 D.runDeal 
                   (_deal runReq) 
                   D.DealPoolFlowPricing 
                   Nothing 
                   (_bondPricing runReq)

    Just (AP.Multiple apsm) -> 
        returnJson $
          Map.map 
            (\x -> D.runDeal 
                     (_deal runReq) 
                     D.DealPoolFlowPricing 
                     (Just x) 
                     (_bondPricing runReq))
             apsm


optionsRunDeal2R :: Handler String
optionsRunDeal2R = do
  addHeader "Access-Control-Allow-Origin" "*"
  addHeader "Access-Control-Allow-Methods" "OPTIONS"
  return "Good"

optionsRunPoolR :: Handler String
optionsRunPoolR = do
  addHeader "Access-Control-Allow-Origin" "*"
  addHeader "Access-Control-Allow-Methods" "OPTIONS"
  return "Good"

postRunPoolR :: Handler Value -- D.TestDeal
postRunPoolR =  do
  runReq <- requireCheckJsonBody :: Handler RunPoolReq
  returnJson $
     P.projPoolCFs
        (pool runReq)
        (fromMaybe [] (poolAssump runReq))
        (poolAggRule runReq)

getVersionR :: Handler String
getVersionR =  do
  addHeader "Access-Control-Allow-Origin" "*"
  addHeader "Access-Control-Allow-Methods" "GET"
  return "{\"version\":\"0.0.2\"}"

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


$(deriveJSON defaultOptions ''RunDealReq)
$(deriveJSON defaultOptions ''RunDealReq2)
$(deriveJSON defaultOptions ''RunPoolReq)
