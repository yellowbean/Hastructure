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
import qualified UT.UtilTest as UtilT

import qualified Accounts as A
import qualified Lib as L
import qualified Stmt as S
import qualified Data.Time as T

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [AT.mortgageTests
                           ,CFT.cfTests
                           ,BT.pricingTests
                           ,LT.curveTests
                           --,LT.queryStmtTests
                           ,LT.datesTests
                           ,LT.prorataTests
                           ,ET.expTests
                           ,DT.waterfallTests
                           ,DT.queryTests
                           ,DT.triggerTests
                           ,UtilT.daycountTests1
                           ,UtilT.daycountTests2
                           ,UtilT.daycountTests3
                           ,UtilT.daycountTests4
                           ,UtilT.tsTest
                           ,UtilT.dateVectorPatternTest
                           ,AccT.intTests
                           ,QT.queryTest
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
