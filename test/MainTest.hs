import Test.Tasty
-- import Test.Tasty.SmallCheck as SC
--import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.List
import Data.Ord

import qualified UT.AssetTest as AT
import qualified UT.AccountTest as AccT
import qualified UT.CashflowTest as CFT
import qualified UT.BondTest as BT
import qualified UT.LibTest as LT
import qualified UT.ExpTest as ET
import qualified UT.DealTest as DT
import qualified UT.QueryTest as QT
import qualified UT.StmtTest as ST
import qualified UT.UtilTest as UtilT
import qualified UT.AnalyticsTest as AnalyticsT
import qualified UT.InterestRateTest as IRT

import qualified DealTest.DealTest as DealTest

import qualified Accounts as A
import qualified Lib as L
import qualified Stmt as S
import qualified Data.Time as T
import qualified Data.Vector as UtilT

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [AT.mortgageTests
                           ,AT.mortgageCalcTests
                           -- ,AT.loanTests
                           ,AT.leaseTests
                           ,AT.leaseFunTests
                           ,AT.installmentTest
                           ,AT.armTest
                           ,AT.ppyTest
                           -- ,CFT.cfTests
                           -- ,CFT.tsSplitTests
                           -- ,CFT.testMergePoolCf
                           -- ,CFT.combineTest
                           -- ,BT.pricingTests
                           -- ,BT.bndUtilTest
                           -- ,LT.curveTests
                           -- ,LT.pvTests
                           -- --,LT.queryStmtTests
                           -- ,LT.datesTests
                           -- ,LT.prorataTests
                           -- ,LT.tsOperationTests
                           -- ,ET.expTests
                           -- ,DT.queryTests
                           -- ,DT.triggerTests
                           -- ,DT.dateTests
                           -- ,DT.liqProviderTest
                           -- ,UtilT.daycountTests1
                           -- ,UtilT.daycountTests2
                           -- ,UtilT.daycountTests3
                           -- ,UtilT.daycountTests4
                           -- ,UtilT.tsTest
                           -- ,UtilT.ts2Test
                           -- ,UtilT.ts3Test
                           -- ,UtilT.dateVectorPatternTest
                           -- ,UtilT.paddingTest
                           -- ,UtilT.dateSliceTest
                           -- ,UtilT.capTest
                           -- ,UtilT.roundingTest
                           -- ,UtilT.sliceTest
                           -- ,UtilT.splitTsTest
                           -- ,AccT.intTests
                           -- ,AccT.investTests
                           -- ,AccT.reserveAccTest
                           -- ,QT.queryTest
                           -- ,ST.txnTest
                           -- ,IRT.armResetTests
                           -- ,IRT.interestRoundingTest
                           -- ,AnalyticsT.walTest
                           -- ,AnalyticsT.durationTest
                           -- ,DealTest.baseTests
                           ]
