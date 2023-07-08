module UT.QueryTest(queryTest)
where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Time as T
import Lib
import Util
import Stmt
import Cashflow
import Data.Ratio 
import qualified UT.DealTest as DT
import Deal
import Deal.DealBase
import Asset
import Types

queryTest = 
  let 
    a = CashFlowFrame $ [ MortgageFlow (toDate "20220101") 100 20 15 0 0 0 0 0.01 Nothing 
                        , MortgageFlow (toDate "20220201") 100 20 15 0 0 0 0 0.01 Nothing 
                        , MortgageFlow (toDate "20220301") 100 20 15 0 0 0 0 0.01 Nothing 
                        , MortgageFlow (toDate "20220401") 100 20 15 0 0 0 0 0.01 Nothing 
                        ]
    opool = (pool DT.td2)
    t = DT.td2 { pool = opool { futureCf = Just a } }                    
  in 
    testGroup "" $ 
      [
        testCase "Query Interest Collected" $
          assertEqual "Mid slide"
            30.0
            (queryDeal t (PoolCollectionHistory CollectedInterest (toDate "20220115") (toDate "20220315")))            

      ]
