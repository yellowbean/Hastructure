{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Analytics (calcDuration,pv,calcWAL)
  where 
import Types
import Lib
import Util
import Data.Aeson hiding (json)
import Language.Haskell.TH
import Data.Aeson.TH
import Data.Aeson.Types
import GHC.Generics
import Data.Ratio

import Debug.Trace
debug = flip trace


calcWAL :: TimeHorizion -> Balance -> Date -> [(Balance,Date)] -> Balance 
calcWAL th bal d ps = 
  let 
    interval = case th of
                 ByYear -> 365
                 ByMonth -> 30
    weightedAmts = [ mulBR futureAmt ((daysBetween d futureDate) % interval)  | (futureAmt,futureDate) <- ps ]
  in 
    (sum weightedAmts) / bal

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

pv :: Ts -> Date -> Date -> Amount -> Amount
pv pc today d amt = 
   realToFrac $ (realToFrac amt) * (1 / factor) --  `debug` ("DF:"++show factor++" PV AMT"++show amt)
  where
   distance::Double = fromIntegral $ daysBetween today d
   discount_rate = fromRational $ getValByDate pc Exc d -- `debug` ("Get val by ts"++show pc ++">>d"++ show d)
   factor::Double = (1 + realToFrac discount_rate) ** (distance / 365) --  `debug` ("discount_rate"++show(discount_rate) ++" dist days=>"++show(distance))
