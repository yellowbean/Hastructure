import Test.Tasty
-- import Test.Tasty.SmallCheck as SC
--import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.List
import Data.Ord

import qualified UT.AssetTest as AT
import qualified UT.CashflowTest as CFT
import qualified UT.BondTest as BT
import qualified UT.LibTest as LT
import qualified UT.ExpTest as ET
import qualified UT.DealTest as DT
import qualified UT.UtilTest as UtilT

import qualified Accounts as A
import qualified Lib as L
import qualified Stmt as S
import qualified Data.Time as T

main = defaultMain tests

td = T.fromGregorian  2020 1 1
stmt1 = S.Statement [(S.AccTxn td 100 20 "Pay"),(S.AccTxn td 100 10 "")]
acc1 = A.Account 100 "A1" Nothing Nothing (Just stmt1)
acc2 = A.Account 150 "A2" Nothing Nothing Nothing

tests :: TestTree
tests = testGroup "Tests" [accTests,stmtTests
                           ,AT.mortgageTests
                           ,CFT.cfTests
                           ,BT.pricingTests
                           ,LT.curveTests
                           ,LT.queryStmtTests
                           ,LT.datesTests
                           ,ET.expTests
                           ,DT.waterfallTests
                           ,DT.queryTests
                           ,UtilT.daycountTests1
                           ,UtilT.daycountTests2
                           ,UtilT.daycountTests3
                           ,UtilT.daycountTests4
                           
                           ]

accTests = testGroup "Account Tests"
  [testCase "Draw" $
    assertEqual "draw:amount"
      (A.getAvailBal (A.draw 60 td "" acc1 )) 40
   ,testCase "Transfer" $
    assertEqual "transfer:amount"
      (A.getAvailBal (fst (A.transfer acc1 20 td acc2))) 80
  ]

stmtTests = testGroup "Statement Test"
  [testCase "Aggregate Txn" $
    assertEqual "Sum by regrex"
      (S.queryStmtAmt (A.accStmt acc1) "Pay") 20
  ]



-- properties :: TestTree
-- properties = testGroup "Properties" [scProps, qcProps]


--scProps = testGroup "(checked by SmallCheck)"
--  [ SC.testProperty "sort == sort . reverse" $
--      \list -> sort (list :: [Int]) == sort (reverse list)
--  , SC.testProperty "Fermat's little theorem" $
--      \x -> ((x :: Integer)^7 - x) `mod` 7 == 0
--  -- the following property does not hold
--  , SC.testProperty "Fermat's last theorem" $
--      \x y z n ->
--        (n :: Integer) >= 3 SC.==> x^n + y^n /= (z^n :: Integer)
--  ]
--
--qcProps = testGroup "(checked by QuickCheck)"
--  [ QC.testProperty "sort == sort . reverse" $
--      \list -> sort (list :: [Int]) == sort (reverse list)
--  , QC.testProperty "Fermat's little theorem" $
--      \x -> ((x :: Integer)^7 - x) `mod` 7 == 0
--  -- the following property does not hold
--  , QC.testProperty "Fermat's last theorem" $
--      \x y z n ->
--        (n :: Integer) >= 3 QC.==> x^n + y^n /= (z^n :: Integer)
--  ]
