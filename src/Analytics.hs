{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Analytics 
  where 
import Types
import Lib
import Util
import Data.Aeson hiding (json)
import Language.Haskell.TH
import Data.Aeson.TH
import Data.Aeson.Types
import GHC.Generics
import Data.Ratio

import Debug.Trace
debug = flip trace


calcWAL :: TimeHorizion -> Balance -> Date -> [(Balance,Date)] -> Balance 
calcWAL th bal d ps = 
  let 
    interval = case th of
                 ByYear -> 365
                 ByMonth -> 30
    weightedAmts = [ mulBR futureAmt ((daysBetween d futureDate) % interval)  | (futureAmt,futureDate) <- ps ]
  in 
    (sum weightedAmts) / bal