import Test.Tasty
-- import Test.Tasty.SmallCheck as SC
--import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.List
import Data.Ord

-- import qualified Accounts as A
-- import qualified Data.Date as T

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]



unitTests = testGroup "Unit tests"
 -- let
 --   acc1 = Account 100 "A1" Nothing Nothing Nothing
 --   td = T.fromGregorian 1970 1 1
 -- in
  [ --testCase "draw account" $ A.getAvailBal (draw 60 td "" acc1 ) == 40
      -- [1, 2, 3] `compare` [1,2] @?= GT

  -- the following test does not hold
   testCase "List comparison (same length)" $
      [1, 2, 3] `compare` [1,2,2] @?= GT
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
