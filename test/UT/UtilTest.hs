module UT.UtilTest(daycountTests1,daycountTests2,daycountTests3,daycountTests4)--,daycountTests3,daycountTests4)
where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Time as T
import Util
import Types
import Data.Fixed

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
        (toRational ((div 4 365) + (div 58 366))) $ 
        yearCountFraction DC_ACT_ACT d1 d2
    ,testCase "Act/365F" $
      assertEqual "" 
        (toRational (div 62 365)) $ 
        yearCountFraction DC_ACT_365F d1 d2
    ,testCase "Act/360" $
      assertEqual "" 
        (toRational (div 62 360)) $ 
        yearCountFraction DC_ACT_360 d1 d2
    ,testCase "Act/365A" $
      assertEqual "" 
        (toRational (div 62 365)) $ 
        yearCountFraction DC_ACT_365A d1 d2
    ,testCase "Act/365L" $
      assertEqual "" 
        (toRational (div 62 366)) $ 
        yearCountFraction DC_ACT_365L d1 d2
    ,testCase "NL/365" $
      assertEqual "" 
        (toRational (div 62 365)) $ 
        yearCountFraction DC_NL_365 d1 d2
    ,testCase "30 360 ISDA" $
      assertEqual "" 
        (toRational (div 60 360)) $ 
        yearCountFraction DC_30_360_ISDA d1 d2
    ,testCase "30E 360" $
      assertEqual "" 
        (toRational (div 60 360)) $ 
        yearCountFraction DC_30E_360 d1 d2
    ,testCase "30Ep 360" $
      assertEqual "" 
        (toRational (div 60 360)) $ 
        yearCountFraction DC_30Ep_360 d1 d2
    ,testCase "30 360 German" $
      assertEqual "" 
        (toRational (div 60 360)) $ 
        yearCountFraction DC_30_360_German d1 d2
    ,testCase "30 360 US" $
      assertEqual "" 
        (toRational (div 60 360)) $ 
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
     assertEqual "should be close to 0.1694288494678 " 
       (toRational ((div 4 365) + (div 59 366))) $ 
       yearCountFraction DC_ACT_ACT d1 d2
   ,testCase "Act/365F" $
     assertEqual "" 
       (toRational (div 63 365)) $ 
       yearCountFraction DC_ACT_365F d1 d2
   ,testCase "Act/360" $
     assertEqual "" 
       (toRational (div 63 360)) $ 
       yearCountFraction DC_ACT_360 d1 d2
   ,testCase "Act/365A" $
     assertEqual "" 
       (toRational (div 63 366)) $ 
       yearCountFraction DC_ACT_365A d1 d2
   ,testCase "Act/365L" $
     assertEqual "" 
       (toRational (div 63 366)) $ 
       yearCountFraction DC_ACT_365L d1 d2
   ,testCase "NL/365" $
     assertEqual "" 
       (toRational (div 62 365)) $ 
       yearCountFraction DC_NL_365 d1 d2
   ,testCase "30 360 ISDA" $
     assertEqual "" 
       (toRational (div 61 360)) $ 
       yearCountFraction DC_30_360_ISDA d1 d2
   ,testCase "30E 360" $
     assertEqual "" 
       (toRational (div 61 360)) $ 
       yearCountFraction DC_30E_360 d1 d2
   ,testCase "30Ep 360" $
     assertEqual "" 
       (toRational (div 61 360)) $ 
       yearCountFraction DC_30Ep_360 d1 d2
   ,testCase "30 360 German" $
     assertEqual "" 
       (toRational (div 62 360)) $ 
       yearCountFraction DC_30_360_German d1 d2
   ,testCase "30 360 US" $
     assertEqual "" 
       (toRational (div 61 360)) $ 
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
       (toRational ((div 62 365) + (div 334 366))) $ 
       yearCountFraction DC_ACT_ACT d1 d2
   ,testCase "Act/365F" $
     assertEqual "" 
       (toRational (div 396 365)) $ 
       yearCountFraction DC_ACT_365F d1 d2
   ,testCase "Act/360" $
     assertEqual "" 
       (toRational (div 396 360)) $ 
       yearCountFraction DC_ACT_360 d1 d2
   ,testCase "Act/365A" $
     assertEqual "" 
       (toRational (div 396 366)) $ 
       yearCountFraction DC_ACT_365A d1 d2
   ,testCase "Act/365L" $
     assertEqual "" 
       (toRational (div 396 366)) $ 
       yearCountFraction DC_ACT_365L d1 d2
   ,testCase "NL/365" $
     assertEqual "" 
       (toRational (div 395 365)) $ 
       yearCountFraction DC_NL_365 d1 d2
   ,testCase "30 360 ISDA" $
     assertEqual "" 
       (toRational (div 390 360)) $ 
       yearCountFraction DC_30_360_ISDA d1 d2
   ,testCase "30E 360" $
     assertEqual "" 
       (toRational (div 390 360)) $ 
       yearCountFraction DC_30E_360 d1 d2
   ,testCase "30Ep 360" $
     assertEqual "" 
       (toRational (div 390 360)) $ 
       yearCountFraction DC_30Ep_360 d1 d2
   ,testCase "30 360 German" $
     assertEqual "" 
       (toRational (div 390 360)) $ 
       yearCountFraction DC_30_360_German d1 d2
   ,testCase "30 360 US" $
     assertEqual "" 
       (toRational (div 390 360)) $ 
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
     assertEqual "should be close to 0.1694288494678 " 
       (toRational ((div 335 366) + (div 150 366))) $ 
       yearCountFraction DC_ACT_ACT d1 d2
   ,testCase "Act/365F" $
     assertEqual "" 
       (toRational (div 485 365)) $ 
       yearCountFraction DC_ACT_365F d1 d2
   ,testCase "Act/360" $
     assertEqual "" 
       (toRational (div 485 360)) $ 
       yearCountFraction DC_ACT_360 d1 d2
   ,testCase "Act/365A" $
     assertEqual "" 
       (toRational (div 485 366)) $ 
       yearCountFraction DC_ACT_365A d1 d2
   ,testCase "Act/365L" $
     assertEqual "" 
       (toRational (div 485 365)) $ 
       yearCountFraction DC_ACT_365L d1 d2
   ,testCase "NL/365" $
     assertEqual "" 
       (toRational (div 484 365)) $ 
       yearCountFraction DC_NL_365 d1 d2
   ,testCase "30 360 ISDA" $
     assertEqual "" 
       (toRational (div 480 360)) $ 
       yearCountFraction DC_30_360_ISDA d1 d2
   ,testCase "30E 360" $
     assertEqual "" 
       (toRational (div 479 360)) $ 
       yearCountFraction DC_30E_360 d1 d2
   ,testCase "30Ep 360" $
     assertEqual "" 
       (toRational (div 480 360)) $ 
       yearCountFraction DC_30Ep_360 d1 d2
   ,testCase "30 360 German" $
     assertEqual "" 
       (toRational (div 479 360)) $ 
       yearCountFraction DC_30_360_German d1 d2
   ,testCase "30 360 US" $
     assertEqual "" 
       (toRational (div 480 360)) $ 
       yearCountFraction DC_30_360_US d1 d2
 ]
     