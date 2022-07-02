{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Call(CallOption(..))
 where

import qualified Data.Time as T

import Data.Aeson hiding (json)
import Language.Haskell.TH
import Data.Aeson.TH
import Data.Aeson.Types

data CallOption = PoolBalance Float
                | BondBalance Float
                | PoolFactor Float
                | BondFactor Float
                | OnDate T.Day
                | AfterDate T.Day
                | And [CallOption] 
                | Or [CallOption] 
                deriving (Show)

$(deriveJSON defaultOptions ''CallOption)
