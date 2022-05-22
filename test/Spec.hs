-- main :: IO ()
-- main = putStrLn "Test suite not yet implemented"

import Test.QuickCheck
import Lib (calcInt,DayCount)
import Data.Time (fromGregorian)


func_t_int = calcInt 1000 (fromGregorian 2020 1 1) (fromGregorian 2020 1 15) 0.12 ACT_365
            == 120.1

main = quickCheck func_t_int
