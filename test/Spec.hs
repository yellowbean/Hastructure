-- main :: IO ()
-- main = putStrLn "Test suite not yet implemented"

import Test.QuickCheck
import Lib (calcInt,DayCount)
import Data.Time (fromGregorian)


func_t_int = calcInt 1000 (fromGregorian 2020 1 1) (fromGregorian 2020 1 15) 0.12 ACT_365
            == 120.1

main = quickCheck func_t_int
--    - Cabal-3.6.3.0@sha256:ff97c442b0c679c1c9876acd15f73ac4f602b973c45bde42b43ec28265ee48f4,12459