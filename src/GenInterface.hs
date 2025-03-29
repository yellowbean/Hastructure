{-# LANGUAGE DeriveGeneric #-}
module GenInterface()

where

import GHC.Generics

import           Proto3.Wire
import qualified Proto3.Wire.Encode as Encode
import qualified Proto3.Wire.Decode as Decode

import Types


-- encodeDateType :: DateType -> Encode.MessageBuilder
-- encodeDateType (DateType a) = Encode.int32 a