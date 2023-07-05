module UT.InterestRateTest(armResetTests,interestRoundingTest)
where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Time as T
import InterestRate
import Types
import Lib
import Util

import Debug.Trace
debug = flip trace

resetDates = toDates ["20230301","20230401","20230501"] 
rt = Floater2 LPR5Y 0.01 0.03 QuarterEnd Nothing Nothing Nothing
rc = IRateCurve [TsPoint (toDate "20230301") 0.01
                ,TsPoint (toDate "20230401") 0.02
                ,TsPoint (toDate "20230501") 0.03]

rc2 = IRateCurve [TsPoint (toDate "20230301") 0.07
                ,TsPoint (toDate "20230401") 0.02
                ,TsPoint (toDate "20230501") 0.03]

rc3 = IRateCurve [TsPoint (toDate "20230301") 0.01
                ,TsPoint (toDate "20230401") 0.08
                ,TsPoint (toDate "20230501") 0.03]
 
arm = ARM 3 (Just 0.015) (Just 0.02) (Just 0.09) (Just 0.02)
armCap = ARM 3 (Just 0.015) (Just 0.02) (Just 0.035) (Just 0.02)
armFloor = ARM 3 (Just 0.015) (Just 0.02) (Just 0.035) (Just 0.025)

armResetTests =  testGroup "ARM reset curve test"
  [
    testCase "no adj rate" $
     assertEqual "no adj rate"  
       [0.05,0.02,0.03,0.04] 
       (runInterestRate arm 0.05 rt resetDates rc)
    ,testCase "cap at first period" $
     assertEqual "cap at first period" 
       [0.05,0.065,0.03,0.04] 
       (runInterestRate arm 0.05 rt resetDates rc2)
    ,testCase "cap at second period" $ 
     assertEqual "Cap at second period" 
       [0.05,0.02,0.04,0.04] 
       (runInterestRate arm 0.05 rt resetDates rc3)
    ,testCase "cap at life cap" $ 
     assertEqual "Cap at life" 
       [0.05,0.02,0.035,0.035] 
       (runInterestRate armCap 0.05 rt resetDates rc3)
    ,testCase "life floor" $ 
     assertEqual "life floor" 
       [0.05,0.025,0.035,0.035] 
       (runInterestRate armFloor 0.05 rt resetDates rc3)
  ]

-- resetDates = toDates ["20230301","20230401","20230501"] 
rt2 = Floater2 LPR5Y 0.01 0.03 QuarterEnd Nothing Nothing (Just (RoundFloor 0.02))

interestRoundingTest =  testGroup "Interest Rounding"
  [
    testCase "rounding by 0.005" $
     assertEqual "no adj rate"  
       [0.05,0.02,0.02,0.04] 
       (runInterestRate arm 0.05 rt2 resetDates rc)
    
  ]