{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Main where

import           Web.Spock
import           Web.Spock.Config
import           Data.Aeson       hiding (json)
import           Data.Monoid      ((<>))
import           Data.Text        (Text, pack)
import           GHC.Generics
import qualified Deal as D -- (TestDeal,run)
import Data.ByteString.Lazy.Char8 (unpack)

type Api = SpockM () () () ()
type ApiAction a = SpockAction () () () a

main :: IO ()
main = do
  spockCfg <- defaultSpockCfg () PCNoDatabase ()
  runSpock 8081 (spock spockCfg app)

app :: Api
app = do
  get "info" $ do
    setHeader "Access-Control-Allow-Headers" "Content-Type"
    text "version:alpha-01"

  post "run_deal" $ do
    theDeal <- jsonBody'  :: ApiAction D.TestDeal
    setHeader "Access-Control-Allow-Origin" "http://localhost:8280"
    setHeader "Access-Control-Allow-Headers" "Content-Type"
    setHeader "Access-Control-Allow-Methods" "*"
    text $  pack $ unpack  $ encode (D.run theDeal 1)

  hookRoute OPTIONS "run_deal" $ do
    setHeader "Access-Control-Allow-Origin" "*"
    setHeader "Access-Control-Allow-Headers" "*"
    setHeader "Access-Control-Allow-Methods" "*"
    text "good"

  post "run_deal2" $ do
    theDeal <- jsonBody'  :: ApiAction D.TestDeal
    setHeader "Access-Control-Allow-Origin" "http://localhost:8280"
    setHeader "Access-Control-Allow-Headers" "Content-Type"
    setHeader "Access-Control-Allow-Methods" "*"
    text $  pack $ unpack  $ encode (D.run2 theDeal Nothing Nothing)

  hookRoute OPTIONS "run_deal2" $ do
    setHeader "Access-Control-Allow-Origin" "*"
    setHeader "Access-Control-Allow-Headers" "*"
    setHeader "Access-Control-Allow-Methods" "*"
    text "good"

