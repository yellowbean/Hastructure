{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Web.Spock
import           Web.Spock.Config

import           Data.Aeson       hiding (json)
import           Data.Monoid      ((<>))
import           Data.Text        (Text, pack)
import           GHC.Generics
import  Deal ()
import  Asset (Mortgage)

--data Person = Person
--  { name :: Text
--  , age  :: Int
--  } deriving (Generic, Show)
--
--instance ToJSON Person
--instance FromJSON Person




type Api = SpockM () () () ()
type ApiAction a = SpockAction () () () a

main :: IO ()
main = do
  spockCfg <- defaultSpockCfg () PCNoDatabase ()
  runSpock 8081 (spock spockCfg app)

app :: Api
app = do
  get "info" $
    text "version:alpha-01"

  --post "run_deal" $ do
  --  theDeal <- jsonBody' :: ApiAction (TestDeal Mortgage)
  --  text $ "Parsed: " <> pack (show theDeal)

