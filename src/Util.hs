{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Util
    (mulBR,mulBIR,mulBI,mulBInt,mulBInteger,lastN
    ,getValByDate,getValByDates 
    ,calcInt,calcIntRate,calcIntRateCurve,divideBB
    ,multiplyTs,zipTs,getTsVals,getTsSize,divideBI,mulIR, daysInterval
    ,replace,paddingDefault, capWith, getTsDates
    ,shiftTsByAmt,calcWeigthBalanceByDates, monthsAfter
    ,getPriceValue,maximum',minimum',roundingBy,roundingByM
    ,floorWith,slice,toPeriodRateByInterval
    -- for debug
    ,zyj
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
import DateUtil

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

mulIntegerR :: Integer -> Rational -> Rational
mulIntegerR i r = (toRational i) * r

mulBInt :: Balance -> Int -> Rational 
mulBInt b i = (toRational b) * (toRational i)

mulBInteger :: Balance -> Integer -> Rational 
mulBInteger b i = mulBInt b (fromInteger i)

mulBI :: Balance -> IRate -> Amount
mulBI bal r = fromRational  $ (toRational bal) * (toRational r)

divideBI :: Balance -> Int -> Balance
divideBI b i = fromRational $ (toRational b) / (toRational i)

divideBB :: Balance -> Balance -> Rational
divideBB b1 b2 = toRational b1 / toRational b2

zipLeftover :: [a] -> [a] -> [a]
zipLeftover []     []     = []
zipLeftover xs     []     = xs
zipLeftover []     ys     = ys
zipLeftover (x:xs) (y:ys) = zipLeftover xs ys

lastN :: Int -> [a] -> [a]
lastN n xs = zipLeftover (drop n xs) xs

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

getValByDate (RatioCurve dps) Exc d
  = case find (\(TsPoint _d _) -> d > _d) (reverse dps)  of
      Just (TsPoint _d v) -> toRational v  -- `debug` ("Getting rate "++show(_d)++show(v))
      Nothing -> 0              -- `debug` ("Getting 0 ")

getValByDate (RatioCurve dps) Inc d
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
getTsVals (RatioCurve ts) = [ v | (TsPoint d v) <- ts ]
getTsVals (BalanceCurve ts) = [ toRational v | (TsPoint d v) <- ts ]
getTsVals (IRateCurve ts) = [ toRational v | (TsPoint d v) <- ts ]

getTsDates :: Ts -> [Date]
getTsDates (IRateCurve tps) =  map getDate tps
getTsDates (RatioCurve tps) =  map getDate tps
getTsDates (FloatCurve tps) =  map getDate tps
getTsDates (PricingCurve tps) =  map getDate tps
getTsDates (BalanceCurve tps) =  map getDate tps

getTsSize :: Ts -> Int 
getTsSize ts = length (getTsVals ts)


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

-- | swap a value in list with index supplied
replace :: [a] -> Int -> a -> [a]
replace xs i e 
  | i > pred (length xs) = error $ "index:"++show i++" is greater than size"++ show (length xs)
  | otherwise = case splitAt i xs of
                   (before, _:after) -> before ++ e: after
                   _ -> xs

-- ^ padding default value to list ,make it length with N
paddingDefault :: a -> [a] -> Int -> [a]
paddingDefault x xs s 
  | length xs > s = take s xs
  | otherwise = xs ++ replicate (s - length xs) x

capWith :: Ord a => a -> [a] -> [a]
capWith cap xs = [ if x > cap then 
                    cap
                   else 
                    x | x <- xs ]

floorWith :: Ord a => a -> [a] -> [a]
floorWith floor xs = [ max x floor | x <- xs]

daysInterval :: [Date] -> [Integer]
daysInterval ds = zipWith daysBetween (init ds) (tail ds)

   
debugLine :: Show a => [a] -> String 
debugLine xs = ""

shiftTsByAmt :: Ts -> Rational -> Ts 
shiftTsByAmt (IRateCurve  tps) delta 
  = IRateCurve $ [ TsPoint d (fromRational delta+v) | TsPoint d v <- tps ]

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

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from ) (drop from xs)


toPeriodRateByInterval :: Rate -> Int -> Rate
toPeriodRateByInterval annualRate days
  = toRational $ 1 - fromRational (1-annualRate) ** (fromIntegral days / 365) -- `debug` ("days>>"++show days++"DIV"++ show ((fromIntegral days) / 365))

----- DEBUG/PRINT
-- z y j : stands for chinese Zhao Yao Jing ,which is a mirror reveals the devil 
zyj :: Show a => Maybe String -> [a] -> String
zyj ms vs = 
  let 
    ss = show <$> vs
  in 
    "|" ++ (fromMaybe "" ms) ++ "|" ++ concat (intersperse " >> " ss) ++ "|"
