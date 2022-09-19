{-# LANGUAGE OverloadedStrings #-}

module Util
    (mulBR,lastN)
    where
import qualified Data.Time as T
import Data.List
import Data.Fixed
import qualified Data.Map as M

import Lib

import Debug.Trace
debug = flip trace

mulBR :: Balance -> Rate -> Centi
mulBR b r
  = fromRational $ (toRational b) * r --`debug` ("b "++show(b)++"r "++show(r)++" = "++show(b * (fromRational r)))

zipLeftover :: [a] -> [a] -> [a]
zipLeftover []     []     = []
zipLeftover xs     []     = xs
zipLeftover []     ys     = ys
zipLeftover (x:xs) (y:ys) = zipLeftover xs ys

lastN :: Int -> [a] -> [a]
lastN n xs = zipLeftover (drop n xs) xs