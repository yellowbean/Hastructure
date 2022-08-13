{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}


module Performance.Main ()
where

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import GHC.Generics

import qualified Data.Time as T
import qualified Deal as D

--main = do
--  input <- B.readFile "../DealInputs/JY_2017_05.json"
--  mm <- decode input :: Maybe D.TestDeal
--  case mm of
--    Nothing -> print "error parsing JSON"
--    Just m -> D.runDeal
--               m
--               D.DealPoolFlowPricing
--               Nothing
--               Nothing

-- greet m = (show.name) m ++" was born in the year "++ (show.born) m