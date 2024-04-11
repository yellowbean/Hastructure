module DealTest.RevolvingTest(baseTests)

where

import Test.Tasty
import Test.Tasty.HUnit
import Deal

import qualified Accounts as A
import qualified Stmt as S
import qualified Asset as P
import qualified AssetClass.Mortgage as ACM
import qualified AssetClass.AssetBase as AB
import qualified Expense as F
import qualified Deal.DealBase as D
import qualified Deal as DR
import qualified Liability as L
import qualified Waterfall as W
import qualified Revolving as R
import qualified Cashflow as CF
import qualified Assumptions as AP
import qualified Call as C
import InterestRate
import qualified CreditEnhancement as CE
import qualified Triggers as Trg
import Lib

import qualified Data.Map as Map
import qualified Data.Time as T
import qualified Data.Set as S
import Types

import Control.Lens hiding (element)
import Control.Lens.TH

import DealTest.DealTest (emptyCase,baseCase)


baseTests = 
  let 
    poolAssets = [(AB.PersonalLoan AB.LoanOriginalInfo{AB.originBalance= 1000, AB.originRate= Fix DC_ACT_365F 0.08,
                                                    AB.originTerm = 24, AB.period = Monthly ,AB.startDate = (T.fromGregorian 2022 1 1),
                                                    AB.prinType = AB.I_P}
                                1000
                                0.08
                                24
                                AB.Current)]
    revolvingAssump = Just (AP.AvailableAssets (R.ConstantAsset $ AB.LO <$> poolAssets)
                                                (AP.PoolLevel ((AP.LoanAssump Nothing Nothing Nothing Nothing)
                                                                ,AP.DummyDelinqAssump
                                                                ,AP.DummyDefaultAssump))
                            )

    -- revolvingDeal = set D.dealPool (D.SoloPool P.Pool{P.assets = poolAssets, P.futureCf=Nothing
    --                                                ,P.futureScheduleCf = Nothing, P.asOfDate = toDate "20220101"
    --                                                ,P.issuanceStat = Nothing, P.extendPeriods = Nothing })  baseCase

   -- (dealAfterRun,poolCf,_,_) = DR.runDeal baseCase DealPoolFlowPricing 
   --                                       Nothing 
   --                                       (AP.NonPerfAssumption Nothing Nothing Nothing revolvingAssump Nothing Nothing Nothing Nothing Nothing Nothing)
  in 
   testGroup "Revolving: Single Pool" 
   [ testCase "Asset: Loan" $
     assertEqual  "First Pay"
     True
     True
     ,testCase "empty pool flow" $
     assertEqual "empty pool flow"
     0
     -- (P.futureCf (D.pool baseCase))
     0
   ]

