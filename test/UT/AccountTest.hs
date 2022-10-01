module UT.AccountTest(intTests)
where

import Test.Tasty
import Test.Tasty.HUnit
import Accounts 
import Lib
import Util
import Types

import qualified Data.Time as T

intTests =
  let 
    acc1 = Account 200 "A1" (Just (BankAccount 0.03 (toDate "20221001") QuarterEnd)) Nothing Nothing
  in 
    testGroup "Interest on Bank Account Test"
     [
      testCase "Build EarnIntAction" $
        assertEqual "QuarterEnd" 
          [("A1",(genSerialDates QuarterEnd (toDate "20221001") 5))] $ 
          buildEarnIntAction [acc1] (toDate "20231231") []
      ,testCase "Build EarnIntAction Same Year" $
        assertEqual "QuarterEnd Same Year" 
          [("A1",(genSerialDates QuarterEnd (toDate "20221001") 1))] $ 
          buildEarnIntAction [acc1] (toDate "20221231") []
     ]