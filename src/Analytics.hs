{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Analytics (calcDuration,pv,calcWAL,pv2,pv3,fv2)
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

calcDuration :: Date -> [(Date,Balance)] -> Ts -> Balance
calcDuration d ps pricingCurve 
  = (foldr (\(_d,_b) acc ->
                    (mulBR  
                      ((pv pricingCurve d _d _b) / presentValue) 
                      (yearCountFraction DC_ACT_365F d _d))
                    + acc)
                    0
                    ps)
    where 
      presentValue = sum [ pv pricingCurve d _d _b | (_d,_b) <- ps ] 

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
pv2 discount_rate today d amt =
  realToFrac $ (realToFrac amt) * (1/denominator)  -- `debug` ("pv: cash"++ show amt++" deno"++ show denominator++">> rate"++show discount_rate)
  where
    denominator::Double = (1 + realToFrac discount_rate) ** (distance / 365)
    distance::Double = fromIntegral $ daysBetween today d -- `debug` ("days betwwen"++ show (daysBetween today d)++">>"++ show d ++ ">>today>>"++ show today)

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