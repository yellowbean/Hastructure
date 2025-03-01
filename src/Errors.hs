{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Errors(EngineError(..),ErrorMonad)
 where

import Control.Exception



data EngineError = DivideZero
                 | NoComponentFound
                 | NotValidAction
                 deriving (Show,Eq,Ord,Read)

instance Exception EngineError
type ErrorMonad = Either EngineError
