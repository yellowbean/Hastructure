{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Errors(EngineError(..),ErrorMonad)
 where

import Control.Exception
-- import Control.Monad.Except

instance Exception EngineError


data EngineError = DivideZero
                 | NoComponentFound
                 | NotValidAction
                 deriving (Show,Eq,Ord,Read)

type ErrorMonad = Either EngineError
