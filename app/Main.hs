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
import qualified Data.List as L
import qualified Deal as D
import qualified Asset as P
import qualified AssetClass.Installment as AC_Installment
import qualified AssetClass.Mortgage as AC_Mortgage
import qualified AssetClass.Loan as AC_Loan
import qualified AssetClass.Lease as AC_Lease
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

data DealType = MDeal (D.TestDeal AC_Mortgage.Mortgage)
              | LDeal (D.TestDeal AC_Loan.Loan)
              | IDeal (D.TestDeal AC_Installment.Installment)
              | RDeal (D.TestDeal AC_Lease.Lease)

$(deriveJSON defaultOptions ''DealType)

data RunDealReq = RunDealReq {
   deal :: DealType
  ,assump :: Maybe AP.AssumptionInput
  ,bondPricing :: Maybe AP.BondPricingInput}

$(deriveJSON defaultOptions ''RunDealReq)

data PoolType = MPool (P.Pool AC_Mortgage.Mortgage)
              | LPool (P.Pool AC_Loan.Loan)
              | IPool (P.Pool AC_Installment.Installment)
              | RPool (P.Pool AC_Lease.Lease)
              deriving(Show)

$(deriveJSON defaultOptions ''PoolType)

data RunPoolReq = RunPoolReq {
   pool :: PoolType
  ,pAssump :: Maybe AP.ApplyAssumptionType
} deriving(Show)

$(deriveJSON defaultOptions ''RunPoolReq)

data App = App

mkYesod "App" [parseRoutes|
 /run_deal RunDealR POST OPTIONS
 /run_pool RunPoolR POST OPTIONS
 /version VersionR GET OPTIONS
|]
 
instance Yesod App where
  yesodMiddleware = defaultYesodMiddleware

optionsRunPoolR :: Handler String 
optionsRunPoolR = do
  addHeader "Access-Control-Allow-Origin" "*"
  addHeader "Access-Control-Allow-Methods" "OPTIONS"
  return "Good"

postRunPoolR :: Handler Value
postRunPoolR = do
  req <- requireCheckJsonBody  :: Handler RunPoolReq 
  returnJson $ 
      case req of
        RunPoolReq (MPool p) ma -> P.aggPool $ D.runPool2 p ma
        RunPoolReq (LPool p) ma -> P.aggPool $ D.runPool2 p ma
        RunPoolReq (IPool p) ma -> P.aggPool $ D.runPool2 p ma
        RunPoolReq (RPool p) ma -> P.aggPool $ D.runPool2 p ma

optionsRunDealR :: Handler String 
optionsRunDealR = do
  addHeader "Access-Control-Allow-Origin" "*"
  addHeader "Access-Control-Allow-Methods" "OPTIONS"
  return "Good"

postRunDealR :: Handler Value
postRunDealR = do
  req <- requireCheckJsonBody  :: Handler RunDealReq 
  case req of
    RunDealReq (MDeal d) a p -> run d a p
    RunDealReq (LDeal d) a p -> run d a p
    RunDealReq (IDeal d) a p -> run d a p
    RunDealReq (RDeal d) a p -> run d a p
  where 
    run d a p =
      case a of 
        Just (AP.Single aps) ->
          returnJson $
            D.runDeal d D.DealPoolFlowPricing (Just aps) p
        Nothing ->
          returnJson $
            D.runDeal d D.DealPoolFlowPricing Nothing p
        Just (AP.Multiple apsm) ->
          returnJson $
            Map.map (\x -> D.runDeal d D.DealPoolFlowPricing (Just x) p) apsm

getVersionR :: Handler String
getVersionR =  let 
                 _v = "0.8.6"
               in
                 do
                   addHeader "Access-Control-Allow-Origin" "*"
                   addHeader "Access-Control-Allow-Methods" "GET"
                   return $ "{\"version\":\" "++_v++" \"}"

optionsVersionR :: Handler String 
optionsVersionR = do
  addHeader "Access-Control-Allow-Origin" "*"
  addHeader "Access-Control-Allow-Methods" "OPTIONS"
  return "Good"

data Config = Config { port :: Int} deriving (Show,Generic)
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
