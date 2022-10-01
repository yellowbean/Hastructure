{-# LANGUAGE OverloadedStrings #-}

module Util
    (mulBR,lastN,yearCountFraction,genSerialDates
    ,actionDate)
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

      DC_ACT_365F -> toRational $ div _diffDays 365 -- `debug` ("DIFF Days"++show(_diffDays))

      DC_ACT_360  -> toRational $ div _diffDays 360

      DC_ACT_365A -> toRational $ 
                       if has_leap_day then 
                         div _diffDays 366
                       else 
                         div _diffDays 365

      DC_ACT_365L -> toRational $
                       if T.isLeapYear eyear then 
                         div _diffDays 366
                       else  
                         div _diffDays 365
      
      DC_NL_365 -> toRational $
                       if has_leap_day then 
                         div (pred _diffDays) 365
                       else  
                         div _diffDays 365

      DC_30E_360 -> toRational $ 
                      let
                        _sday = f31to30 sday
                        _eday = f31to30 eday
                        num = (_eday-_sday) + 30*(emonth-smonth) + 360*(fromIntegral (eyear-syear))
                      in 
                        div num 360
      
      DC_30Ep_360 -> toRational $ 
                       let
                         _sday = f31to30 sday
                         (_eyear,_emonth,_eday) = T.toGregorian $
                                                    if eday==31 then 
                                                      T.addDays 1 ed
                                                    else
                                                      ed
                         num = (_eday-_sday) + 30*(_emonth-smonth) + 360*(fromIntegral (_eyear-syear))
                       in 
                         div num 360
      DC_30_360_ISDA -> toRational $ -- 30/360 Bond basis
                          let
                            _sday = f31to30 sday
                            _eday = if _sday>=30 && eday==31 then 
                                     30
                                   else 
                                     eday    
                            num = (_eday-_sday) + 30*(emonth-smonth) + 360*(fromIntegral (eyear-syear))
                          in 
                            div num 360
      DC_30_360_German -> toRational $ -- 30/360 Bond basis , this was call 30E/360 ISDA by kalotay
                            let
                              _sday = if sday==31 || (endOfFeb syear smonth sday) then 
                                       30
                                     else 
                                       sday  
                              _eday = if eday==31 || (endOfFeb eyear emonth eday) then 
                                       30
                                     else
                                       eday    
                              num = (_eday-_sday) + 30*(fromIntegral (emonth-smonth)) + 360*(fromIntegral (eyear-syear))
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
      has_leap_day 
        = case (sameYear,sLeap,eLeap) of                   
            (True,False,False) -> False 
            (True,True,_) -> inRange (sd,ed) (T.fromGregorian syear 2 29)
            _ -> let 
                   _leapDays = [  T.fromGregorian _y 2 29   |  _y <- range (syear,eyear) , (T.isLeapYear _y) ]
                 in   
                   any (inRange (sd,ed)) _leapDays

      _diffYears = eyear - syear
      sDaysTillYearEnd = T.diffDays sd (T.fromGregorian syear 12 31)
      eDaysAfterYearBeg = T.diffDays ed (T.fromGregorian eyear 1 1)
      _diffDays = T.diffDays ed sd
      sLeap = T.isLeapYear syear
      eLeap = T.isLeapYear eyear
      (syear,smonth,sday) = T.toGregorian sd 
      (eyear,emonth,eday) = T.toGregorian ed 

genSerialDates :: DatePattern -> Date -> Int -> Dates
genSerialDates dp sd num
  = take num $ filter (>= sd) $ 
      case dp of 
        MonthEnd -> 
                [T.fromGregorian yearRange (fst __md) (snd __md) | yearRange <- [_y..(_y+yrs)]
                                                                 ,__md <- monthEnds yearRange ]
                where 
                  yrs = fromIntegral $ div num 12 + 1                   
        QuarterEnd -> 
                [T.fromGregorian yearRange __m __d | yearRange <- [_y..(_y+yrs)]
                                                   ,(__m,__d) <- quarterEnds]
                where 
                  yrs = fromIntegral $ div num 4 + 1                   
        YearEnd -> 
                [T.fromGregorian yearRange 12 31 | yearRange <- [_y..(_y+(toInteger num))]]
        YearFirst ->
                [T.fromGregorian yearRange 1 1 | yearRange <- [_y..(_y+(toInteger num))]]
        MonthFirst ->
                [T.fromGregorian yearRange monthRange 1 | yearRange <- [_y..(_y+yrs)]
                                                        , monthRange <- [1..12]]
                where 
                  yrs = fromIntegral $ div num 12 + 1                   
        QuarterFirst ->
                [T.fromGregorian yearRange __m 1 | yearRange <- [_y..(_y+yrs)]
                                                 ,__m <- [3,6,9,12]]
                where 
                  yrs = fromIntegral $ div num 4 + 1                   
        MonthDayOfYear m d -> 
                [T.fromGregorian yearRange m d | yearRange <- [_y..(_y+(toInteger num))]]
        DayOfMonth d ->
                [T.fromGregorian yearRange monthRange d | yearRange <- [_y..(_y+yrs)]
                                                        , monthRange <- [1..12]]
                where 
                  yrs = fromIntegral $ div num 12 + 1                   
       -- DayOfWeek -> [] 
      where 
        quarterEnds = [(3,31),(6,30),(9,30),(12,31)]
        monthEnds y = 
          if T.isLeapYear y then
            [(1,31),(2,28),(3,31),(4,30),(5,31),(6,30),(7,31),(8,31),(9,30),(10,31),(11,30),(12,31)]
          else
            [(1,31),(2,29),(3,31),(4,30),(5,31),(6,30),(7,31),(8,31),(9,30),(10,31),(11,30),(12,31)]
        (_y,_m,_d) = T.toGregorian sd  
        yearBegin = T.fromGregorian _y 1 1


