{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Analytics (calcConvexity,calcDuration,pv,calcWAL,pv2,pv3,fv2,pv21,calcRequiredAmtForIrrAtDate,calcIRR)

  where 
import Types
import Lib
import Util
import DateUtil
import Data.Aeson hiding (json)
import Language.Haskell.TH
import Data.Aeson.TH
import Data.Aeson.Types
import GHC.Generics
import Data.Ratio
import Numeric.RootFinding

import Debug.Trace
debug = flip trace

-- ^ calculate the Weighted Average Life of cashflow, with unit option to Monthly or Yearly
calcWAL :: TimeHorizion -> Balance -> Date -> [(Balance,Date)] -> Balance 
calcWAL th bal d ps = 
  let 
    interval = case th of
                  ByYear -> 365
                  ByMonth -> 30
    weightedAmts = [ mulBR futureAmt ((daysBetween d futureDate) % interval)  | (futureAmt,futureDate) <- ps ]
  in 
    sum weightedAmts / bal

calcDuration :: DayCount -> Date -> [(Date,Balance)] -> Ts -> Rate
calcDuration dc d ps pricingCurve 
  = (foldr (\(_d,_b) acc ->
                    (*) 
                      (divideBB (pv pricingCurve d _d _b) presentValue) 
                      (yearCountFraction dc d _d)
                    + acc)
                    0.0000
                    ps)
    where 
      presentValue = sum [ pv pricingCurve d _d _b | (_d,_b) <- ps ] 

calcConvexity :: DayCount -> Date -> [(Date,Balance)] -> Ts -> Rate
calcConvexity dc d ps pricingCurve 
  = toRational $
      (*)
        presentValue' $
        (foldr (\(_t,_c,_f) acc ->
                      (_t * (_t + 1) * fromRational _c) / ((1.000 + _f) ** (_t+2))
                      )
                      0.0000
                      (zip3 ts payments pvFactors)) -- `debug` ("'v"++show presentValue'++"others"++ show (zip3 ts payments pvFactors))
    where 
      pvFactors::[Double] = fromRational <$> getValByDate pricingCurve Inc <$> fst <$> ps
      presentValue'::Double = 1 / (fromRational . toRational) (sum [ pv pricingCurve d _d _b | (_d,_b) <- ps ])
      payments = toRational . snd <$> ps
      ts::[Double] = fromRational <$> yearCountFraction dc d <$> fst <$> ps

-- ^ calculate present value of input amount in future with given a curve and PV date
pv :: Ts -> Date -> Date -> Amount -> Amount
pv pc today d amt = 
  realToFrac $ (realToFrac amt) * (1 / factor) --  `debug` ("DF:"++show factor++" PV AMT"++show amt)
  where
    distance::Double = fromIntegral $ daysBetween today d
    discount_rate = fromRational $ getValByDate pc Exc d -- `debug` ("Get val by ts"++show pc ++">>d"++ show d)
    factor::Double = (1 + realToFrac discount_rate) ** (distance / 365) --  `debug` ("discount_rate"++show(discount_rate) ++" dist days=>"++show(distance))

-- ^ calculate present value in the future using constant rate
pv2 :: IRate -> Date -> Date -> Amount -> Amount
pv2 discount_rate today d amt 
  | today == d = amt
  | otherwise 
    = realToFrac $ (realToFrac amt) * (1/denominator)  -- `debug` ("pv: cash"++ show amt++" deno"++ show denominator++">> rate"++show discount_rate)
      where
        denominator::Double = (1 + realToFrac discount_rate) ** (distance / 365)
        distance::Double = fromIntegral $ daysBetween today d -- `debug` ("days betwwen"++ show (daysBetween today d)++">>"++ show d ++ ">>today>>"++ show today)

-- ^ calculate present value to specific date given a series of amount with dates
pv21 :: IRate -> Date -> [Date] -> [Amount] -> Balance
pv21 r d ds vs = sum [ pv2 r d _d amt | (_d,amt) <- zip ds vs ]

-- ^ using double for ridder's method

pv2' :: Double -> Date -> Date -> Double -> Double
pv2' r today d amt 
  | today == d = amt
  | otherwise 
    = realToFrac $ (realToFrac amt) * (1/denominator)  -- `debug` ("pv: cash"++ show amt++" deno"++ show denominator++">> rate"++show discount_rate)
      where
        denominator::Double = (1 + r) ** (distance / 365)
        distance::Double = fromIntegral $ daysBetween today d -- `debug` ("days betwwen"++ show (daysBetween today d)++">>"++ show d ++ ">>today>>"++ show today)

pv22 :: Double -> Date -> [Date] -> [Amount] -> Double
pv22 r d ds vs = 
  let 
    vs' = (fromRational . toRational) <$> vs
  in 
    sum [ pv2' r d _d amt | (_d,amt) <- zip ds vs' ]

-- ^ calcualte present value given a series of amount with dates
pv3 :: Ts -> Date -> [Date] -> [Amount] -> Balance 
pv3 pvCurve pricingDate ds vs 
  = let 
      rs = fromRational <$> getValByDates pvCurve Inc ds
      pvs = [ pv2 r pricingDate d amt | (r,d,amt) <- zip3 rs ds vs ]
    in 
      sum pvs

fv2 :: IRate -> Date -> Date -> Amount -> Amount
fv2 discount_rate today futureDay amt 
  = realToFrac $ realToFrac amt * factor 
  where
    factor::Double = (1 + realToFrac discount_rate) ** (distance / 365)
    distance::Double = fromIntegral $ daysBetween today futureDay


calcPvFromIRR :: Double -> [Date] -> [Amount] -> Date -> Double -> Double
calcPvFromIRR irr [] _ d amt = 0
calcPvFromIRR irr ds vs d amt = 
  let 
    begDate = head ds
    pv = pv22 irr begDate (ds++[d]) (vs++[ (fromRational . toRational) amt ])
  in 
    (fromRational . toRational) pv

-- ^ calculate IRR of a series of cashflow
calcRequiredAmtForIrrAtDate :: Double -> [Date] -> [Amount] -> Date -> Maybe Amount
calcRequiredAmtForIrrAtDate irr [] _ d = Nothing 
calcRequiredAmtForIrrAtDate irr ds vs d = 
  let 
    itertimes = 500
    def = RiddersParam { riddersMaxIter = itertimes, riddersTol = RelTol 0.00000001}
  in 
    case ridders def (0.0001,100000000000000) (calcPvFromIRR irr ds vs d) of
          Root finalAmt -> Just (fromRational (toRational finalAmt))
          _ -> Nothing

-- ^ calc IRR from a cashflow 
calcIRR :: [Date] -> [Amount] -> Either String Rate
calcIRR  _ [] = Left "No cashflow amount"
calcIRR [] _ = Left "No cashflow date"
calcIRR ds vs
  | all (> 0) vs = Left "All cashflow can't be all positive"
  | all (< 0) vs = Left "All cashflow can't be all negative"
  | otherwise = 
    let 
      itertimes = 1000
      def = RiddersParam { riddersMaxIter = itertimes, riddersTol = RelTol 0.0000000001}
      beginDate = head ds
      sumOfPv irr = fromRational . toRational $ pv22 irr beginDate ds vs
    in 
      case ridders def (-1,1000) sumOfPv of
            Root irrRate -> Right $ toRational irrRate
            _ -> Left $ "IRR can't be calculated with input "++ show vs++" and dates"++ show ds