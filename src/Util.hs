{-# LANGUAGE OverloadedStrings #-}

module Util
    (mulBR,lastN)
    where
import qualified Data.Time as T
import Data.List
import Data.Fixed
import Data.Ix
import qualified Data.Map as M

import Lib
import Types

import Debug.Trace
debug = flip trace

mulBR :: Balance -> Rate -> Centi
mulBR b r = fromRational $ (toRational b) * r --`debug` ("b "++show(b)++"r "++show(r)++" = "++show(b * (fromRational r)))

zipLeftover :: [a] -> [a] -> [a]
zipLeftover []     []     = []
zipLeftover xs     []     = xs
zipLeftover []     ys     = ys
zipLeftover (x:xs) (y:ys) = zipLeftover xs ys

lastN :: Int -> [a] -> [a]
lastN n xs = zipLeftover (drop n xs) xs

yearCountFraction :: DayCount -> Date -> Date -> Rational --TODO https://www.iso20022.org/15022/uhb/mt565-16-field-22f.htm
yearCountFraction dc sd ed 
  = case dc of 
      DC_ACT_ACT -> if sameYear then 
                      toRational _diffDays / daysOfYear syear
                    else
                      toRational $ (div sDaysTillYearEnd (daysOfYear syear)) + (div eDaysAfterYearBeg (daysOfYear eyear)) + _diffYears

      DC_ACT_365F -> toRational $ div _diffDays 365

      DC_ACT_360  -> toRational $ div _diffDays 360

      DC_ACT_365A -> case (sameYear,(T.isLeapYear syear)) of 
                       (True,False) -> toRational $ div _diffDays 365
                       (True,True) -> 
                          if inRange (sd,ed) (T.fromGregorian syear 2 29) then 
                             toRational $ div _diffDays 366 
                          else 
                             toRational $ div _diffDays 365
                       (False, _ ) ->
                          case (T.isLeapYear syear,T.isLeapYear eyear,succ syear==eyear) of 
                            (False,False,True) -> 
                                toRational $ div _diffDays 365
                            (False,False,False) -> 
                                if any T.isLeapYear (range (syear,eyear)) then 
                                    toRational $ div _diffDays 366
                                else 
                                    toRational $ div _diffDays 365
                            _ ->  0.0 --TODO need to fix bug here
      DC_ACT_365L -> toRational $
                       if T.isLeapYear eyear then 
                         div _diffDays 366
                       else  
                         div _diffDays 365
      
      DC_NL_365 -> 0.0

      DC_30E_360 -> toRational $ 
                      let
                        sday = f31to30 sday
                        eday = f31to30 eday
                        num = (eday-sday) + 30*(emonth-smonth) + 360*(fromIntegral (eyear-syear))
                      in 
                        div num 360
      
      DC_30Ep_360 -> toRational $ 
                       let
                         sday = f31to30 sday
                         (_eyear,_emonth,_eday) = T.toGregorian $
                                                    if eday==31 then 
                                                      T.addDays 1 ed
                                                    else
                                                      ed
                         num = (_eday-sday) + 30*(_emonth-smonth) + 360*(fromIntegral (_eyear-syear))
                       in 
                         div num 360
      DC_30_360_ISDA -> toRational $ -- 30/360 Bond basis
                          let
                            sday = f31to30 sday
                            eday = if sday>=30 && eday==31 then 
                                     30
                                   else 
                                     eday    
                            num = (eday-sday) + 30*(emonth-smonth) + 360*(fromIntegral (eyear-syear))
                          in 
                            div num 360
      DC_30_360_German -> toRational $ -- 30/360 Bond basis , this was call 30E/360 ISDA by kalotay
                            let
                              sday = if sday==31 || (endOfFeb syear smonth sday) then 
                                       30
                                     else 
                                       sday  
                              eday = if eday==31 || (endOfFeb eyear emonth eday) then 
                                       30
                                     else
                                       eday    
                              num = (eday-sday) + 30*(fromIntegral (emonth-smonth)) + 360*(fromIntegral (eyear-syear))
                            in 
                              div num 360
      DC_30_360_US ->  toRational $ 
                            let
                              _sday = if (endOfFeb syear smonth sday) || sday==31 then 
                                        30
                                      else 
                                        sday  
                              _eday = if (eday==31 && sday >= 30)||(endOfFeb eyear emonth eday) && (endOfFeb syear smonth sday)  then 
                                        30
                                      else
                                        eday 
                              num = (_eday-_sday) + 30*(emonth-smonth) + 360*(fromIntegral (eyear-syear))
                            in 
                              div num 360
      -- https://www.iso20022.org/15022/uhb/mt565-16-field-22f.htm

    where 
      daysOfYear y = if T.isLeapYear y then 366 else 365
      f31to30 d = if d==31 then 
                    30
                  else
                    d
      endOfFeb y d m = if T.isLeapYear y && (m==2) then 
                         d == 29
                       else 
                         d == 28 
      sameYear = syear == eyear
      _diffYears = eyear - syear
      sDaysTillYearEnd = T.diffDays sd (T.fromGregorian syear 12 31)
      eDaysAfterYearBeg = T.diffDays ed (T.fromGregorian eyear 1 1)
      _diffDays = T.diffDays sd ed 
      sLeap = T.isLeapYear syear
      eLeap = T.isLeapYear eyear
      (syear,smonth,sday) = T.toGregorian sd 
      (eyear,emonth,eday) = T.toGregorian ed 