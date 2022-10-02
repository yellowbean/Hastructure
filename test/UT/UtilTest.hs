module UT.UtilTest(daycountTests1,daycountTests2,daycountTests3,daycountTests4)--,daycountTests3,daycountTests4)
where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Time as T
import Util
import Types
import Data.Fixed
import Data.Ratio ((%))

import Debug.Trace
debug = flip trace


daycountTests1 =  
  let
    d1 = T.fromGregorian 2007 12 28
    d2 = T.fromGregorian 2008 2 28
  in
    testGroup "Day Count Tests 1"
  [
    testCase "Act/Act" $
      assertEqual "should be close to 0.1694288494678 " 
        ((4 % 365) + (58 % 366)) $  --TODO to be confirm from Delta
        yearCountFraction DC_ACT_ACT d1 d2
    ,testCase "Act/365F" $
      assertEqual "" 
        (62 % 365) $ 
        yearCountFraction DC_ACT_365F d1 d2
    ,testCase "Act/360" $
      assertEqual "" 
        (62 % 360) $ 
        yearCountFraction DC_ACT_360 d1 d2
    ,testCase "Act/365A" $
      assertEqual "" 
        (62 % 365) $ 
        yearCountFraction DC_ACT_365A d1 d2
    ,testCase "Act/365L" $
      assertEqual "" 
        (62 % 366) $ 
        yearCountFraction DC_ACT_365L d1 d2
    ,testCase "NL/365" $
      assertEqual "" 
        (62 % 365) $ 
        yearCountFraction DC_NL_365 d1 d2
    ,testCase "30 360 ISDA" $
      assertEqual "" 
        (60 % 360) $ 
        yearCountFraction DC_30_360_ISDA d1 d2
    ,testCase "30E 360" $
      assertEqual "" 
        (60 % 360) $ 
        yearCountFraction DC_30E_360 d1 d2
    ,testCase "30Ep 360" $
      assertEqual "" 
        (60 % 360) $ 
        yearCountFraction DC_30Ep_360 d1 d2
    ,testCase "30 360 German" $
      assertEqual "" 
        (60 % 360) $ 
        yearCountFraction DC_30_360_German d1 d2
    ,testCase "30 360 US" $
      assertEqual "" 
        (60 % 360) $ 
        yearCountFraction DC_30_360_US d1 d2
  ]

daycountTests2 =  
 let
   d1 = T.fromGregorian 2007 12 28
   d2 = T.fromGregorian 2008 2 29
 in
   testGroup "Day Count Tests 2"
 [
   testCase "Act/Act" $
     assertEqual "" 
       ((4 % 365) + (59 % 366)) $ 
       yearCountFraction DC_ACT_ACT d1 d2
   ,testCase "Act/365F" $
     assertEqual "" 
       (63 % 365) $ 
       yearCountFraction DC_ACT_365F d1 d2
   ,testCase "Act/360" $
     assertEqual "" 
       (63 % 360) $ 
       yearCountFraction DC_ACT_360 d1 d2
   ,testCase "Act/365A" $
     assertEqual "" 
       (63 % 366) $ 
       yearCountFraction DC_ACT_365A d1 d2
   ,testCase "Act/365L" $
     assertEqual "" 
       (63 % 366) $ 
       yearCountFraction DC_ACT_365L d1 d2
   ,testCase "NL/365" $
     assertEqual "" 
       (62 % 365) $ 
       yearCountFraction DC_NL_365 d1 d2
   ,testCase "30 360 ISDA" $
     assertEqual "" 
       (61 % 360) $ 
       yearCountFraction DC_30_360_ISDA d1 d2
   ,testCase "30E 360" $
     assertEqual "" 
       (61 % 360) $ 
       yearCountFraction DC_30E_360 d1 d2
   ,testCase "30Ep 360" $
     assertEqual "" 
       (61 % 360) $ 
       yearCountFraction DC_30Ep_360 d1 d2
   ,testCase "30 360 German" $
     assertEqual "" 
       (62 % 360) $ 
       yearCountFraction DC_30_360_German d1 d2
   ,testCase "30 360 US" $
     assertEqual "" 
       (61 % 360) $ 
       yearCountFraction DC_30_360_US d1 d2
 ]
 
daycountTests3 =  
 let
   d1 = T.fromGregorian 2007 10 31
   d2 = T.fromGregorian 2008 11 30
 in
   testGroup "Day Count Tests 3"
 [
   testCase "Act/Act" $
     assertEqual "should be close to 0.1694288494678 " 
       ((62 % 365) + (334 % 366)) $ 
       yearCountFraction DC_ACT_ACT d1 d2
   ,testCase "Act/365F" $
     assertEqual "" 
       (396 % 365) $ 
       yearCountFraction DC_ACT_365F d1 d2
   ,testCase "Act/360" $
     assertEqual "" 
       (396 % 360) $ 
       yearCountFraction DC_ACT_360 d1 d2
   ,testCase "Act/365A" $
     assertEqual "" 
       (396 % 366) $ 
       yearCountFraction DC_ACT_365A d1 d2
   ,testCase "Act/365L" $
     assertEqual "" 
       (396 % 366) $ 
       yearCountFraction DC_ACT_365L d1 d2
   ,testCase "NL/365" $
     assertEqual "" 
       (395 % 365) $ 
       yearCountFraction DC_NL_365 d1 d2
   ,testCase "30 360 ISDA" $
     assertEqual "" 
       (390 % 360) $ 
       yearCountFraction DC_30_360_ISDA d1 d2
   ,testCase "30E 360" $
     assertEqual "" 
       (390 % 360) $ 
       yearCountFraction DC_30E_360 d1 d2
   ,testCase "30Ep 360" $
     assertEqual "" 
       (390 % 360) $ 
       yearCountFraction DC_30Ep_360 d1 d2
   ,testCase "30 360 German" $
     assertEqual "" 
       (390 % 360) $ 
       yearCountFraction DC_30_360_German d1 d2
   ,testCase "30 360 US" $
     assertEqual "" 
       (390 % 360) $ 
       yearCountFraction DC_30_360_US d1 d2
 ]
 
daycountTests4 =  
 let
   d1 = T.fromGregorian 2008 2 1
   d2 = T.fromGregorian 2009 5 31
 in
   testGroup "Day Count Tests 4"
 [
   testCase "Act/Act" $
     assertEqual "" 
       ((335 % 366) + (150 % 365)) $ 
       yearCountFraction DC_ACT_ACT d1 d2
   ,testCase "Act/365F" $
     assertEqual "" 
       (485 % 365) $ 
       yearCountFraction DC_ACT_365F d1 d2
   ,testCase "Act/360" $
     assertEqual "" 
       (485 % 360) $ 
       yearCountFraction DC_ACT_360 d1 d2
   ,testCase "Act/365A" $
     assertEqual "" 
       (485 % 366) $ 
       yearCountFraction DC_ACT_365A d1 d2
   ,testCase "Act/365L" $
     assertEqual "" 
       (485 % 365) $ 
       yearCountFraction DC_ACT_365L d1 d2
   ,testCase "NL/365" $
     assertEqual "" 
       (484 % 365) $ 
       yearCountFraction DC_NL_365 d1 d2
   ,testCase "30 360 ISDA" $
     assertEqual "" 
       (480 % 360) $ 
       yearCountFraction DC_30_360_ISDA d1 d2
   ,testCase "30E 360" $
     assertEqual "" 
       (479 % 360) $ 
       yearCountFraction DC_30E_360 d1 d2
   ,testCase "30Ep 360" $
     assertEqual "" 
       (480 % 360) $ 
       yearCountFraction DC_30Ep_360 d1 d2
   ,testCase "30 360 German" $
     assertEqual "" 
       (479 % 360) $ 
       yearCountFraction DC_30_360_German d1 d2
   ,testCase "30 360 US" $
     assertEqual "" 
       (480 % 360) $ 
       yearCountFraction DC_30_360_US d1 d2
 ]
     