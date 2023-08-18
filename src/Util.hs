{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Util
    (mulBR,mulBIR,mulBI,mulBInt,mulBInteger,lastN,yearCountFraction,genSerialDates
    ,getValByDate,getValByDates,projDatesByPattern
    ,genSerialDatesTill,genSerialDatesTill2,subDates,getTsDates,sliceDates,SliceType(..)      
    ,calcInt,calcIntRate,calcIntRateCurve
    ,multiplyTs,zipTs,getTsVals,divideBI,mulIR, daysInterval
    ,replace,paddingDefault, capWith, pv2, pv3, splitByDate, rangeBy
    ,shiftTsByAmt,calcWeigthBalanceByDates, monthsAfter
    ,getPriceValue,maximum',minimum',roundingBy,roundingByM
    ,floorWith
    )
    where
import qualified Data.Time as T
import Data.List
import Data.Fixed
import Data.Ratio ((%))
import Data.Ix
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S

import Lib
import Types

import Text.Printf
import Control.Exception

import Debug.Trace
import Data.Time (addDays)
debug = flip trace

mulBR :: Balance -> Rate -> Centi
mulBR b r = fromRational $ toRational b * r 

mulBIR :: Balance -> IRate -> Centi
mulBIR b r = fromRational $ (toRational b) * (toRational r)

mulIR :: Int -> Rational -> Rational
mulIR i r = (toRational i) * r 

mulBInt :: Balance -> Int -> Rational 
mulBInt b i = (toRational b) * (toRational i)

mulBInteger :: Balance -> Integer -> Rational 
mulBInteger b i = mulBInt b (fromInteger i)

mulBI :: Balance -> IRate -> Amount
mulBI bal r = fromRational  $ (toRational bal) * (toRational r)

divideBI :: Balance -> Int -> Balance
divideBI b i = fromRational $ (toRational b) / (toRational i)

zipLeftover :: [a] -> [a] -> [a]
zipLeftover []     []     = []
zipLeftover xs     []     = xs
zipLeftover []     ys     = ys
zipLeftover (x:xs) (y:ys) = zipLeftover xs ys

lastN :: Int -> [a] -> [a]
lastN n xs = zipLeftover (drop n xs) xs

-- http://www.deltaquants.com/day-count-conventions
yearCountFraction :: DayCount -> Date -> Date -> Rational --TODO https://www.iso20022.org/15022/uhb/mt565-16-field-22f.htm
yearCountFraction dc sd ed 
  = case dc of 
      DC_ACT_ACT -> if sameYear then 
                      _diffDays % daysOfYear syear
                    else
                      (sDaysTillYearEnd % (daysOfYear syear)) + (eDaysAfterYearBeg % (daysOfYear eyear)) + (pred _diffYears) 
                      -- `debug` ("<>"++show sDaysTillYearEnd++"<>"++show(daysOfYear syear) ++"<>"++show (daysOfYear eyear)++"<>"++ show eyear)

      DC_ACT_365F -> _diffDays % 365 -- `debug` ("DIFF Days"++show(_diffDays))

      DC_ACT_360  -> _diffDays % 360

      DC_ACT_365A -> if has_leap_day then 
                       _diffDays % 366
                     else 
                       _diffDays % 365

      DC_ACT_365L -> if T.isLeapYear eyear then 
                       _diffDays % 366
                     else  
                       _diffDays % 365
      
      DC_NL_365 -> if has_leap_day then 
                     (pred _diffDays) % 365
                   else  
                     _diffDays % 365

      DC_30E_360 -> let
                      _sday = f31to30 sday
                      _eday = f31to30 eday
                      num = toRational (_eday - _sday) + 30*_gapMonth + 360*_diffYears
                    in 
                      num / 360  -- `debug` ("NUM->"++show num++"E S month"++show emonth++show smonth)
      
      DC_30Ep_360 -> let
                       _sday = f31to30 sday
                       (_eyear,_emonth,_eday) = T.toGregorian $
                                                    if eday==31 then 
                                                      T.addDays 1 ed
                                                    else
                                                      ed
                       __gapMonth = (toInteger $ _emonth - smonth) % 1
                       __diffYears = (toInteger $ _eyear - syear) % 1
                       num = toRational (_eday - _sday) + 30*__gapMonth + 360*__diffYears
                     in 
                       num / 360
      DC_30_360_ISDA -> let
                          _sday = f31to30 sday
                          _eday = if _sday>=30 && eday==31 then 
                                    30
                                  else 
                                    eday    
                          num = toRational (_eday - _sday) + 30*_gapMonth + 360*_diffYears
                        in 
                          num / 360
      -- 30/360 Bond basis , this was call 30E/360 ISDA by kalotay
      DC_30_360_German -> let
                            _sday = if sday==31 || (endOfFeb syear smonth sday) then 
                                      30 -- `debug` ("German eof start if>> "++ show (endOfFeb syear smonth sday)++show syear ++show smonth++show sday)
                                    else 
                                      sday  
                                    -- `debug` ("German eof start else "++ show (endOfFeb syear smonth sday)++show syear ++show smonth++show sday)
                            _eday = if eday==31 || (endOfFeb eyear emonth eday) then 
                                      30
                                    else
                                      eday    
                                    -- `debug` ("German eof end "++ show (endOfFeb eyear emonth eday)++show eyear++show emonth++show eday)
                            num = toRational (_eday - _sday) + 30*_gapMonth + 360*_diffYears -- `debug` ("German"++show(_sday)++"<>"++show _eday)
                          in 
                            num / 360
      DC_30_360_US -> let
                        _sday = if (endOfFeb syear smonth sday) || sday==31 then 
                                  30
                                else 
                                  sday  
                        _eday = if (eday==31 && sday >= 30)||(endOfFeb eyear emonth eday) && (endOfFeb syear smonth sday)  then 
                                  30
                                else
                                  eday 
                        num = toRational (_eday - _sday) + 30*_gapMonth + 360*_diffYears
                      in 
                        num / 360
      -- https://www.iso20022.org/15022/uhb/mt565-16-field-22f.htm

    where 
      daysOfYear y = if T.isLeapYear y then 366 else 365
      f31to30 d = if d==31 then 
                    30
                  else
                    d
      endOfFeb y m d = if T.isLeapYear y then 
                         (m==2) && d == 29
                       else 
                         (m==2) && d == 28
      sameYear = syear == eyear
      has_leap_day 
        = case (sameYear,sLeap,eLeap) of                   
            (True,False,False) -> False 
            (True,True,_) -> inRange (sd,ed) (T.fromGregorian syear 2 29)
            _ -> let 
                   _leapDays = [  T.fromGregorian _y 2 29   |  _y <- range (syear,eyear) , (T.isLeapYear _y) ]
                 in   
                   any (inRange (sd,ed)) _leapDays

      _diffYears = (eyear - syear) % 1 -- Ratio Integer
      _gapDay =   toInteger (eday - sday) % 1
      _gapMonth = toInteger (emonth - smonth) % 1
      sDaysTillYearEnd = succ $ T.diffDays (T.fromGregorian syear 12 31) sd
      eDaysAfterYearBeg = T.diffDays ed (T.fromGregorian eyear 1 1)
      _diffDays = toInteger $ T.diffDays ed sd
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
        CustomDate ds -> ds
        EveryNMonth d n -> 
                d:[ T.addGregorianDurationClip (T.CalendarDiffDays ((toInteger _n)*(toInteger n)) 0) d | _n <- [1..num] ]

      where 
        quarterEnds = [(3,31),(6,30),(9,30),(12,31)]
        monthEnds y = 
          if T.isLeapYear y then
            [(1,31),(2,29),(3,31),(4,30),(5,31),(6,30),(7,31),(8,31),(9,30),(10,31),(11,30),(12,31)]
          else
            [(1,31),(2,28),(3,31),(4,30),(5,31),(6,30),(7,31),(8,31),(9,30),(10,31),(11,30),(12,31)]
        (_y,_m,_d) = T.toGregorian sd  

genSerialDatesTill:: Date -> DatePattern -> Date -> Dates 
genSerialDatesTill sd ptn ed 
  = filter (<= ed) $ genSerialDates ptn sd (fromInteger (succ num))  --`debug` ("Num"++show num)
    where 
      (sy,sm,sday) = T.toGregorian sd 
      (ey,em,eday) = T.toGregorian ed 
      T.CalendarDiffDays cdM cdD = T.diffGregorianDurationRollOver ed sd 
      num = case ptn of 
              MonthEnd -> cdM
              QuarterEnd ->  div cdM 3
              YearEnd ->  div cdM 12
              MonthFirst -> cdM 
              QuarterFirst-> div cdM 3
              YearFirst->  div cdM 12
              MonthDayOfYear _m _d -> div cdM 12 -- T.MonthOfYear T.DayOfMonth
              DayOfMonth _d -> cdM -- T.DayOfMonth 
              CustomDate ds -> 2 + toInteger (length ds)
              EveryNMonth _d _n -> div cdM (toInteger _n)
              _ -> error $ "failed to match" ++ show ptn
              -- DayOfWeek Int -> -- T.DayOfWeek 

genSerialDatesTill2 :: RangeType -> Date -> DatePattern -> Date -> Dates
genSerialDatesTill2 rt sd dp ed 
  = case (rt, head _r==sd, last _r==ed) of 
      (II,True,True) -> _r
      (II,True,False) -> _r ++ [ed]
      (II,False,True)-> sd:_r 
      (II,False,False)-> sd:_r ++ [ed] 
      (EI,True,True) -> tail _r 
      (EI,True,False) -> tail _r ++ [ed]
      (EI,False,True) -> _r 
      (EI,False,False) -> _r ++ [ed]
      (IE,True,True) -> init _r 
      (IE,True,False) -> _r 
      (IE,False,True) -> sd:init _r
      (IE,False,False) -> sd:_r 
      (EE,True,True) -> init $ tail _r 
      (EE,True,False) -> tail _r 
      (EE,False,True) -> init _r
      (EE,False,False) -> _r 
      (NO_IE,_,_) -> _r
    where 
      _r = case dp of 
             AllDatePattern dps -> concat [ genSerialDatesTill sd _dp ed | _dp <- dps ]
             StartsExclusive d _dp ->  filter (> d)  $ genSerialDatesTill2 rt sd _dp ed
             Exclude _d _dps ->
                 let 
                   a = S.fromList $ genSerialDatesTill2 rt sd _d ed
                   b = S.fromList $ genSerialDatesTill2 rt sd (AllDatePattern _dps) ed
                 in 
                   sort $ S.toList $ S.difference a b
             OffsetBy _dp _n -> [ T.addDays (toInteger _n) _d   | _d <- genSerialDatesTill2 rt sd _dp ed ]
             _ -> genSerialDatesTill sd dp ed -- maybe sd/ed in _r


tsPointVal :: TsPoint a -> a 
tsPointVal (TsPoint d v) = v

getValByDate :: Ts -> CutoffType -> Date -> Rational

getValByDate (LeftBalanceCurve dps) ct d
  = case find (\(TsPoint _d _) -> (cmpFun ct) _d d) (reverse dps) of 
      Just (TsPoint _d v) -> toRational v
      Nothing -> 0
    where 
      cmpFun Inc = (<=)
      cmpFun Exc = (<)

getValByDate (BalanceCurve dps) Exc d
  = case find (\(TsPoint _d _) -> d > _d) (reverse dps)  of 
      Just (TsPoint _d v) -> toRational v
      Nothing -> 0

getValByDate (BalanceCurve dps) Inc d
  = case find (\(TsPoint _d _) -> d >= _d) (reverse dps)  of 
      Just (TsPoint _d v) -> toRational v
      Nothing -> 0

getValByDate (FloatCurve dps) Exc d
  = case find (\(TsPoint _d _) -> d > _d) (reverse dps)  of 
      Just (TsPoint _d v) -> toRational v  -- `debug` ("Getting rate "++show(_d)++show(v))
      Nothing -> 0              -- `debug` ("Getting 0 ")

getValByDate (IRateCurve dps) Exc d
  = case find (\(TsPoint _d _) -> d > _d) (reverse dps)  of
      Just (TsPoint _d v) -> toRational v  -- `debug` ("Getting rate "++show(_d)++show(v))
      Nothing -> 0              -- `debug` ("Getting 0 ")

getValByDate (IRateCurve dps) Inc d
  = case find (\(TsPoint _d _) -> d >= _d) (reverse dps)  of
      Just (TsPoint _d v) -> toRational v  -- `debug` ("Getting rate "++show(_d)++show(v))
      Nothing -> 0              -- `debug` ("Getting 0 ")

getValByDate (ThresholdCurve dps) Inc d
  = case find (\(TsPoint _d _) -> d <= _d) dps  of
      Just (TsPoint _d v) -> toRational v  -- `debug` ("Getting rate "++show(_d)++show(v))
      Nothing -> tsPointVal $ last dps --`debug` ("Not found in gvbd")

getValByDate (ThresholdCurve dps) Exc d
  = case find (\(TsPoint _d _) -> d < _d) dps  of
      Just (TsPoint _d v) -> toRational v  -- `debug` ("Getting rate "++show(_d)++show(v))
      Nothing -> tsPointVal $ last dps --`debug` ("Not found in gvbd")

getValByDate (FactorCurveClosed dps ed) Exc d
  = case find (\(TsPoint _d _) -> d > _d) (reverse dps)  of 
      Just found@(TsPoint _found_d _found_v) -> 
        if d >= ed then 
          1.0
        else 
          _found_v
      Nothing -> 1.0

getValByDate (PricingCurve dps) _ d
  = case (d>=lday,d<=fday) of 
      (True,_) -> tsPointVal $ last dps
      (_,True) -> tsPointVal $ head dps
      _  -> let 
              rindex = fromMaybe 0 $findIndex (\(TsPoint _dl _) -> ( _dl > d )) dps
              rdp@(TsPoint _dr _rv) = dps!!rindex 
              ldp@(TsPoint _dl _lv) = dps!!(pred rindex)
              leftDistance = toRational $ daysBetween _dl d  -- `debug` ("LEFT"++show d)
              distance = toRational $ daysBetween _dl _dr  -- `debug` ("TOTAL Horizion"++show _dl++show _dr)
              vdistance =  _rv - _lv -- ("DIST")
            in 
              toRational $ _lv + (vdistance * leftDistance) / distance 
 --              `debug` ("PricingCurve get Val: D "++ show _lv++">>"++ show vdistance++">>"++ show leftDistance++">>"++ show distance)
    where 
      fday = getDate $ head dps
      lday = getDate $ last dps

getIndexRateByDates :: RateAssumption  -> [Date] -> [IRate]
getIndexRateByDates (RateCurve idx rc) ds = fromRational <$> getValByDates rc Inc ds
getIndexRateByDates (RateFlat idx r) ds = replicate (length ds) r 


getValByDates :: Ts -> CutoffType -> [Date] -> [Rational]
getValByDates rc ct = map (getValByDate rc ct)

getTsVals :: Ts -> [Rational]
getTsVals (FloatCurve ts) = [ v | (TsPoint d v) <- ts ]

getTsDates :: Ts -> [Date]
getTsDates (IRateCurve tps) =  map getDate tps
getTsDates (FloatCurve tps) =  map getDate tps
getTsDates (PricingCurve tps) =  map getDate tps
getTsDates (BalanceCurve tps) =  map getDate tps

subDates :: RangeType -> Date -> Date -> [Date] -> [Date]
subDates rt sd ed ds 
  = case rt of 
      II -> filter (\x -> x >= sd && x <= ed ) ds 
      EI -> filter (\x -> x > sd && x <= ed ) ds
      IE -> filter (\x -> x >= sd && x < ed ) ds
      EE -> filter (\x -> x > sd && x < ed ) ds

data SliceType = SliceAfter Date 
               | SliceOnAfter Date 
               | SliceAfterKeepPrevious Date
               | SliceOnAfterKeepPrevious Date

sliceDates :: SliceType -> [Date] -> [Date] 
sliceDates st ds =
    case st of 
      SliceAfter d -> filter (> d) ds
      SliceOnAfter d -> filter (>= d) ds
      SliceAfterKeepPrevious d -> 
          case findIndex (> d) ds of
            Just idx -> snd $ splitAt (pred idx) ds
            Nothing -> [] 
      SliceOnAfterKeepPrevious d -> 
          case findIndex (>= d) ds of
            Just idx -> snd $ splitAt (pred idx) ds
            Nothing -> [] 

calcIntRate :: Date -> Date -> IRate -> DayCount -> IRate
calcIntRate start_date end_date int_rate day_count =
  let 
    yf = yearCountFraction day_count start_date end_date
  in 
    int_rate * (fromRational yf)

calcIntRateCurve :: DayCount -> IRate -> [Date] -> [IRate]
calcIntRateCurve dc r ds 
  = [ calcIntRate sd ed r dc |  (sd,ed) <- zip (init ds) (tail ds) ]

calcInt :: Balance -> Date -> Date -> IRate -> DayCount -> Amount
calcInt bal start_date end_date int_rate day_count =
  let 
    yfactor = yearCountFraction day_count start_date end_date
  in 
    mulBR bal (yfactor * (toRational int_rate)) 

zipTs :: [Date] -> [Rational] -> Ts 
zipTs ds rs = FloatCurve [ TsPoint d r | (d,r) <- (zip ds rs) ]

multiplyTs :: CutoffType -> Ts -> Ts -> Ts
multiplyTs ct (FloatCurve ts1) ts2
  = FloatCurve [(TsPoint d (v * (getValByDate ts2 ct d))) | (TsPoint d v) <- ts1 ] 

projDatesByPattern :: DatePattern -> Date -> Date -> Dates   --TODO to be replace by generateDateSeries
projDatesByPattern dp sd ed
  = let 
      (T.CalendarDiffDays cdm cdd) = T.diffGregorianDurationClip ed sd
      num = case dp of
              MonthEnd -> cdm + 1
              QuarterEnd -> (div cdm 3) + 1 -- `debug` ("cdm"++show cdm)
              YearEnd  -> (div cdm 12) + 1
              MonthFirst -> cdm + 1
              QuarterFirst -> (div cdm 3) + 1
              YearFirst -> (div cdm 12) + 1
              MonthDayOfYear _ _ -> (div cdm 12) + 1
              DayOfMonth _ -> cdm + 1
    in 
      genSerialDates dp sd (fromInteger num)

replace :: [a] -> Int -> a -> [a]
replace xs i e = case splitAt i xs of
                   (before, _:after) -> before ++ e: after
                   _ -> xs

paddingDefault :: a -> [a] -> Int -> [a]
paddingDefault x xs s 
  | (length xs) > s = take s xs
  | otherwise = xs++(replicate (s - (length xs)) x)

capWith :: Ord a => [a] -> a -> [a]
capWith xs cap = [ if x > cap then 
                    cap
                   else 
                    x | x <- xs ]

floorWith :: Ord a => [a] -> a -> [a]
floorWith xs floor = [ max x floor | x <- xs]

pv2 :: IRate -> Date -> Date -> Amount -> Amount
pv2 discount_rate today d amt =
    mulBI amt $ 1/denominator -- `debug` ("days between->"++show d ++show today++">>>"++show distance )
  where
    denominator = (1+discount_rate) ^^ (fromInteger (div distance 365))
    distance = daysBetween today d 

pv3 :: Ts -> Date -> [Date] -> [Amount] -> Balance 
pv3 pvCurve pricingDate ds vs 
  = let 
      rs = fromRational <$> getValByDates pvCurve Inc ds
      pvs = [ pv2 r pricingDate d amt | (r,d,amt) <- zip3 rs ds vs ]
    in 
      sum pvs

daysInterval :: [Date] -> [Integer]
daysInterval ds = zipWith daysBetween (init ds) (tail ds)

splitByDate :: TimeSeries a => [a] -> Date -> SplitType -> ([a],[a])
splitByDate xs d st 
  = case st of 
      EqToLeft ->  span (\x -> getDate x <= d) xs
      EqToRight -> span (\x -> getDate x < d) xs
      EqToLeftKeepOne -> 
          case findIndex (\x -> getDate x >= d ) xs of 
            Just idx -> splitAt (pred idx) xs -- `debug` ("split with "++show (pred idx)++">>"++show (length xs))
            Nothing -> (xs,[])
     -- EqToRightKeepOne -> 
     --     case findIndex (\x -> (getDate x) >= d ) xs of 
     --       Just idx -> splitAt (pred idx) xs -- `debug` ("split with "++show (pred idx)++">>"++show (length xs))
     --       Nothing -> (xs,[])

     -- EqToLeftKeepOnes -> 
     --     case findIndices (\x -> (getDate x) <= d) xs of
     --       [] -> (xs,[])
     --       inds -> 
rangeBy :: TimeSeries a => [a] -> Date -> Date -> RangeType -> [a]
rangeBy xs sd ed rt = 
    case rt of 
      II -> filter (\x -> (getDate x >= sd) && (getDate x <= ed)) xs  -- `debug` ("in rangeBy II")
      IE -> filter (\x -> (getDate x >= sd) && (getDate x < ed)) xs
      EI -> filter (\x -> (getDate x > sd) && (getDate x <= ed)) xs
      EE -> filter (\x -> (getDate x > sd) && (getDate x < ed)) xs
    
debugLine :: Show a => [a] -> String 
debugLine xs = ""

shiftTsByAmt :: Ts -> Rational -> Ts 
shiftTsByAmt (IRateCurve  tps) delta 
  = IRateCurve $ [ TsPoint d ((fromRational delta)+v) | TsPoint d v <- tps ]

shiftTsByAmt _ts delta = _ts

assert1 :: Bool -> a -> String -> a
assert1 False x msg = error msg
assert1 _     x _ = x

calcWeigthBalanceByDates :: [Balance] -> [Date] -> Balance 
calcWeigthBalanceByDates bals ds 
  = assert1
      (succ bs_length == ds_length) 
      (sum $ zipWith mulBR bals weights)
      "calcWeigthBalanceByDates: bs and ds should be same length"
      where 
        bs_length = length bals 
        ds_length = length ds
        weights = getIntervalFactors ds

testSumToOne :: [Rate] -> Bool
testSumToOne rs = sum rs == 1.0

monthsAfter :: Date -> Integer -> Date
monthsAfter d n = T.addGregorianDurationClip (T.CalendarDiffDays n 0) d

getPriceValue :: PriceResult -> Balance
getPriceValue (AssetPrice v _ _ _ _ ) = v

maximum' :: Ord a => [a] -> a
maximum' = foldr1 (\x y ->if x >= y then x else y)

minimum' :: Ord a => [a] -> a
minimum' = foldr1 (\x y ->if x >= y then y else x)

roundingBy :: (Fractional a,RealFrac a) => RoundingBy a -> a -> a
roundingBy (RoundFloor x) n = x * fromIntegral (floor (n/x) :: Integer)
roundingBy (RoundCeil x) n = x * fromIntegral (ceiling (n/x) :: Integer)

roundingByM :: (Fractional a,RealFrac a) => Maybe (RoundingBy a) -> a -> a 
roundingByM Nothing x = x 
roundingByM (Just rb) x = roundingBy rb x