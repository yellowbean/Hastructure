module UT.LibTest(curveTests)
where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Time as T
import Lib

curveTests =testGroup "Curve Tests"
  [
    let
     _ts = (FloatCurve [TsPoint (toDate "20210101") 0.01
                         ,TsPoint (toDate "20230101") 0.02])
     _r1 = getValByDate _ts (toDate "20201231")
     _r2 = getValByDate _ts (toDate "20210201")
     _r3 = getValByDate _ts (toDate "20230102")
     _r4 = getValByDate _ts (toDate "20231231")
    in
      testCase "Query interst rate curve by date" $
      assertEqual
        "test 4 dates"
        [_r1,_r2,_r3,_r4] [0, 0.01,0.02,0.02]
  ]