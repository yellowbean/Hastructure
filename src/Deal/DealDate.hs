{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Deal.DealDate (DealDates,getClosingDate,getFirstPayDate) 
  where

import qualified Data.Map as Map
import Deal.DealBase
import Types
import Lib

class DealDates a where 
  getClosingDate :: a -> Date
  getFirstPayDate :: a -> Date

instance DealDates DateDesp where 
  getClosingDate (PatternInterval _m)
    = let 
        (sd,dp,ed) = _m Map.! ClosingDate 
      in 
        sd
         
  getClosingDate (CustomDates _ _ cd _) = cd

  getClosingDate (FixInterval _m _p1 _p2) = _m Map.! ClosingDate

  getClosingDate (PreClosingDates _ x _ _ _ _) = x

  getClosingDate (CurrentDates (_,cd) _ _ _ _ ) = cd

  getFirstPayDate (PatternInterval _m) 
    = let 
        (sd,dp,ed) = _m Map.! FirstPayDate
      in 
         sd
  
  getFirstPayDate (CustomDates _ _ _ bActions )
    = getDate $ head bActions
  
  getFirstPayDate (FixInterval _m _p1 _p2)  
    = _m Map.! FirstPayDate
  
  getFirstPayDate (PreClosingDates _ _ _ _ _ (fp,_)) = fp
  
  getFirstPayDate (CurrentDates _ _ _ _ (cpay,_)) = cpay    
