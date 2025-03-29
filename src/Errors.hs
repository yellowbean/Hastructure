{-# LANGUAGE ScopedTypeVariables #-}


module Errors(EngineError(..))
 where




data EngineError = DivideZero 
                 | NoComponentFound
                 | NotValidAction
                 deriving (Show,Eq,Ord,Read)