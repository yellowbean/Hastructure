{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies     #-}

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

data ReqType = MortgageDeal (RunDealReq P.Mortgage)
             | LoanDeal (RunDealReq P.Loan)
             deriving (Show)

$(deriveJSON defaultOptions ''ReqType)

data App = App

mkYesod "App" [parseRoutes|
 /run_deal RunDealR POST OPTIONS
 /version VersionR GET OPTIONS
|]
 
-- /run/#Text RunR POST OPTIONS

instance Yesod App where
  yesodMiddleware = defaultYesodMiddleware

optionsRunR :: Handler String 
optionsRunR = do
  addHeader "Access-Control-Allow-Origin" "*"
  addHeader "Access-Control-Allow-Methods" "OPTIONS"
  return "Good"

postRunR :: Text -> Handler Value
postRunR assetType = do
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


optionsRunDealR :: Handler String 
optionsRunDealR = do
  addHeader "Access-Control-Allow-Origin" "*"
  addHeader "Access-Control-Allow-Methods" "OPTIONS"
  return "Good"

postRunDealR :: Handler Value
postRunDealR =  do
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

