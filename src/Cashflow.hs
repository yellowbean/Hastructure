{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DeriveGeneric       #-}

module Cashflow (CashFlowFrame(..),Principals,Interests,Amount
                ,combine,mergePoolCf,sumTsCF
                ,sizeCashFlowFrame,aggTsByDates, getTsCashFlowFrame
                ,mflowInterest,mflowPrincipal,mflowRecovery,mflowPrepayment
                ,mflowRental,mflowRate,sumPoolFlow
                ,mflowDefault,mflowLoss,mflowDate
                ,getSingleTsCashFlowFrame,getDatesCashFlowFrame,getDateRangeCashFlowFrame
                ,lookupSource
                ,mflowBalance,mflowBegBalance,tsDefaultBal
                ,mflowBorrowerNum,mflowPrepaymentPenalty
                ,splitCashFlowFrameByDate
                ,tsTotalCash, setPrepaymentPenalty, setPrepaymentPenaltyFlow
                ,tsDateLT,getDate,getTxnLatestAsOf
                ,mflowWeightAverageBalance,appendCashFlow,combineCashFlow
                ,addFlowBalance,totalLoss,totalDefault,totalRecovery,firstDate
                ,shiftCfToStartDate,cfInsertHead,buildBegTsRow
                ,TsRow(..),cfAt) where

import Data.Time (Day)
import Data.Fixed
import Lib (weightedBy,toDate,getIntervalFactors,daysBetween)
import Util (mulBR,splitByDate)
import Types
import qualified Data.Map as Map
import qualified Data.Time as T
import qualified Data.List as L
import Data.Maybe 

import Data.Aeson hiding (json)
import Language.Haskell.TH
import GHC.Generics
import Data.Aeson.TH
import Data.Aeson.Types

import Text.Printf

import Debug.Trace
debug = flip trace

type Delinquent = Centi
type Amounts = [Float]
type Principals = [Principal]
type Interests = [Interest]
type Prepayments = [Prepayment]
type Recoveries = [Recovery]
type Rates = [Rate]

data TsRow = CashFlow Date Amount
           | BondFlow Date Balance Principal Interest
           | MortgageFlow Date Balance Principal Interest Prepayment Default Recovery Loss IRate (Maybe BorrowerNum) (Maybe PrepaymentPenalty)
           | LoanFlow Date Balance Principal Interest Prepayment Default Recovery Loss IRate
           | LeaseFlow Date Balance Rental
           deriving(Show,Eq,Ord,Generic)

instance TimeSeries TsRow where 
    getDate (CashFlow x _) = x
    getDate (BondFlow x  _ _ _) = x
    getDate (MortgageFlow x _ _ _ _ _ _ _ _ _ _) = x
    getDate (LoanFlow x _ _ _ _ _ _ _ _) = x
    getDate (LeaseFlow x _ _ ) = x

data CashFlowFrame = CashFlowFrame [TsRow]
                     deriving (Show,Eq,Generic)
                   
sizeCashFlowFrame :: CashFlowFrame -> Int
sizeCashFlowFrame (CashFlowFrame ts) = length ts

getTsCashFlowFrame :: CashFlowFrame -> [TsRow]
getTsCashFlowFrame (CashFlowFrame ts) = ts

getDatesCashFlowFrame :: CashFlowFrame -> [Date]
getDatesCashFlowFrame (CashFlowFrame ts) = getDates ts

getDateRangeCashFlowFrame :: CashFlowFrame -> (Date,Date)
getDateRangeCashFlowFrame (CashFlowFrame trs)
  = (getDate (head trs), getDate (last trs))

cfAt :: CashFlowFrame -> Int -> Maybe TsRow
cfAt (CashFlowFrame trs) idx = 
    if (idx < 0) || (idx >= length trs) then
        Nothing
    else
        Just (trs!!idx)

cfInsertHead :: TsRow -> CashFlowFrame -> CashFlowFrame
cfInsertHead tr (CashFlowFrame trs)
  = CashFlowFrame $ tr:trs

getSingleTsCashFlowFrame :: CashFlowFrame -> Date -> TsRow
getSingleTsCashFlowFrame (CashFlowFrame trs) d
  = head $ filter (\x -> getDate x == d) trs

splitCashFlowFrameByDate :: CashFlowFrame -> Date -> SplitType  -> (CashFlowFrame,CashFlowFrame)
splitCashFlowFrameByDate (CashFlowFrame txns) d st
  = let 
      (ls,rs) = splitByDate txns d st
    in 
      (CashFlowFrame ls,CashFlowFrame rs)


getTxnLatestAsOf :: CashFlowFrame -> Date -> Maybe TsRow
getTxnLatestAsOf (CashFlowFrame txn) d = L.find (\x -> getDate x <= d) $ reverse txn

addTs :: TsRow -> TsRow -> TsRow     -- left is ealier ,right is later,combine TS from same cashflow
addTs (CashFlow d1 a1 ) (CashFlow _ a2 ) = CashFlow d1 (a1 + a2)
addTs (BondFlow d1 b1 p1 i1 ) tr@(BondFlow _ b2 p2 i2 ) = BondFlow d1 (b1 - mflowAmortAmount tr) (p1 + p2) (i1 + i2)
addTs (MortgageFlow d1 b1 p1 i1 prep1 def1 rec1 los1 rat1 mbn1 pn1) tr@(MortgageFlow _ b2 p2 i2 prep2 def2 rec2 los2 rat2 mbn2 pn2)
  = let 
      bn = do bn1 <- mbn1
              bn2 <- mbn2
              return (bn1 + bn2)
      p =  do p1 <- pn1
              p2 <- pn2
              return (p1 + p2)              
    in 
      (MortgageFlow d1 (b1 - mflowAmortAmount tr) (p1 + p2) (i1 + i2) (prep1 + prep2) (def1 + def2) (rec1 + rec2) (los1+los2) (fromRational (weightedBy [b1,b2] (map toRational [rat1,rat2]))) bn p)
addTs (LoanFlow d1 b1 p1 i1 prep1 def1 rec1 los1 rat1) tr@(LoanFlow _ b2 p2 i2 prep2 def2 rec2 los2 rat2)
  = (LoanFlow d1 (b1 - mflowAmortAmount tr) (p1 + p2) (i1 + i2) (prep1 + prep2) (def1 + def2) (rec1 + rec2) (los1+los2) (fromRational (weightedBy [b1,b2] (map toRational [rat1,rat2]))))
addTs (LeaseFlow d1 b1 r1) tr@(LeaseFlow d2 b2 r2) 
  = (LeaseFlow d1 (b1 - mflowAmortAmount tr) (r1 + r2) )

combineTs :: TsRow -> TsRow -> TsRow     -- left is ealier ,right is later,combine TS from two cashflow
combineTs (CashFlow d1 a1 ) (CashFlow _ a2 ) = CashFlow d1 (a1 + a2)
combineTs (BondFlow d1 b1 p1 i1 ) tr@(BondFlow _ b2 p2 i2 ) = BondFlow d1 (b1 + b2) (p1 + p2) (i1 + i2)
combineTs (MortgageFlow d1 b1 p1 i1 prep1 def1 rec1 los1 rat1 mbn1 pn1) tr@(MortgageFlow _ b2 p2 i2 prep2 def2 rec2 los2 rat2 mbn2 pn2)
  = let 
      bn = do bn1 <- mbn1
              bn2 <- mbn2
              return (bn1 + bn2)
      p =  do p1 <- pn1
              p2 <- pn2
              return (p1 + p2)                  
    in 
      (MortgageFlow d1 (b1 + b2) (p1 + p2) (i1 + i2) (prep1 + prep2) (def1 + def2) (rec1 + rec2) (los1+los2) (fromRational (weightedBy [b1,b2] (map toRational [rat1,rat2]))) bn p)
combineTs (LoanFlow d1 b1 p1 i1 prep1 def1 rec1 los1 rat1) tr@(LoanFlow _ b2 p2 i2 prep2 def2 rec2 los2 rat2)
  = (LoanFlow d1 (b1 + b2) (p1 + p2) (i1 + i2) (prep1 + prep2) (def1 + def2) (rec1 + rec2) (los1+los2) (fromRational (weightedBy [b1,b2] (map toRational [rat1,rat2]))))
combineTs (LeaseFlow d1 b1 r1) tr@(LeaseFlow d2 b2 r2) 
  = (LeaseFlow d1 (b1 + b2) (r1 + r2) )

appendTs :: TsRow -> TsRow -> TsRow --early row on left, later row on right, update right TS balance
appendTs bn1@(BondFlow d1 b1 _ _ ) bn2@(BondFlow d2 b2 p2 i2 ) 
  = updateFlowBalance (b1 - (mflowAmortAmount bn2)) bn2 -- `debug` ("b1 >> "++show b1++">>"++show (mflowAmortAmount bn2))
appendTs (MortgageFlow d1 b1 p1 i1 prep1 def1 rec1 los1 rat1 mbn1 _) bn2@(MortgageFlow _ b2 p2 i2 prep2 def2 rec2 los2 rat2 mbn2 _)
  = updateFlowBalance (b1 - (mflowAmortAmount bn2)) bn2
appendTs (LoanFlow d1 b1 p1 i1 prep1 def1 rec1 los1 rat1) bn2@(LoanFlow _ b2 p2 i2 prep2 def2 rec2 los2 rat2)
  = updateFlowBalance (b1 - (mflowAmortAmount bn2)) bn2
appendTs (LeaseFlow d1 b1 r1) bn2@(LeaseFlow d2 b2 r2) 
  = updateFlowBalance (b1 - (mflowAmortAmount bn2)) bn2

addTsCF :: TsRow -> TsRow -> TsRow
addTsCF (CashFlow d1 a1 ) (CashFlow _ a2 ) = (CashFlow d1 (a1 + a2))
addTsCF (BondFlow d1 b1 p1 i1 ) (BondFlow _ b2 p2 i2 ) = (BondFlow d1 (min b1 b2) (p1 + p2) (i1 + i2) )
addTsCF (MortgageFlow d1 b1 p1 i1 prep1 def1 rec1 los1 rat1 mbn1 pn1) (MortgageFlow d2 b2 p2 i2 prep2 def2 rec2 los2 rat2 mbn2 pn2)
  = let 
      bn =  do bn1 <- mbn1 
               bn2 <- mbn2 
               return (min bn1 bn2)
      p =  do p1 <- pn1
              p2 <- pn2
              return (p1 + p2)                   
    in 
      (MortgageFlow d2 (min b1 b2) (p1 + p2) (i1 + i2) (prep1 + prep2) (def1 + def2) (rec1 + rec2) (los1+los2) (fromRational (weightedBy [b1,b2] (map toRational [rat1,rat2]))) bn p)
addTsCF (LoanFlow d1 b1 p1 i1 prep1 def1 rec1 los1 rat1) (LoanFlow _ b2 p2 i2 prep2 def2 rec2 los2 rat2)
  = (LoanFlow d1 (min b1 b2) (p1 + p2) (i1 + i2) (prep1 + prep2) (def1 + def2) (rec1 + rec2) (los1+los2) (fromRational (weightedBy [b1,b2] (map toRational [rat1,rat2]))) )
addTsCF (LeaseFlow d1 b1 r1) (LeaseFlow d2 b2 r2) = (LeaseFlow d1 (min b1 b2) (r1 + r2) )

sumTs :: [TsRow] -> Date -> TsRow
sumTs trs d = tsSetDate (foldr1 addTs trs) d

sumTsCF :: [TsRow] -> Date -> TsRow
sumTsCF trs d = tsSetDate (foldl1 addTsCF trs) d -- `debug` ("Summing"++show trs++">>"++ show (tsSetDate (foldr1 addTsCF trs) d))

tsTotalCash :: TsRow -> Balance
tsTotalCash (CashFlow _ x) = x
tsTotalCash (BondFlow _ _ a b) = a + b
tsTotalCash (MortgageFlow x _ _ a b c _ e _ _ mPn) = a + b + c + e + fromMaybe 0 mPn
tsTotalCash (LoanFlow _ _ a b c _ e _ _) =  a + b + c + e
tsTotalCash (LeaseFlow _ _ a) =  a

tsDefaultBal :: TsRow -> Balance
tsDefaultBal CashFlow {} = error "not supported"
tsDefaultBal BondFlow {} = error "not supported"
tsDefaultBal (MortgageFlow _ _ _ _ _ x _ _ _ _ _) = x
tsDefaultBal (LoanFlow _ _ _ _ _ x _ _ _) = x
tsDefaultBal LeaseFlow {} = error "not supported"

tsSetDate :: TsRow -> Date -> TsRow
tsSetDate (CashFlow _ a) x  = CashFlow x a
tsSetDate (BondFlow _ a b c) x = BondFlow x a b c
tsSetDate (MortgageFlow _ a b c d e f g h i j) x = MortgageFlow x a b c d e f g h i j
tsSetDate (LoanFlow _ a b c d e f g h) x = LoanFlow x a b c d e f g h
tsSetDate (LeaseFlow _ a b) x = LeaseFlow x a b

tsSetBalance :: Balance -> TsRow -> TsRow
tsSetBalance x (CashFlow _d a) = CashFlow _d x
tsSetBalance x (BondFlow _d a b c) = BondFlow _d x b c
tsSetBalance x (MortgageFlow _d a b c d e f g h i j) = MortgageFlow _d x b c d e f g h i j
tsSetBalance x (LoanFlow _d a b c d e f g h) = LoanFlow _d x b c d e f g h
tsSetBalance x (LeaseFlow _d a b) = LeaseFlow _d x b

tsOffsetDate :: Integer -> TsRow -> TsRow
tsOffsetDate x (CashFlow _d a) = CashFlow (T.addDays x _d) a
tsOffsetDate x (BondFlow _d a b c) = BondFlow (T.addDays x _d) a b c
tsOffsetDate x (MortgageFlow _d a b c d e f g h i j) = MortgageFlow (T.addDays x _d) a b c d e f g h i j
tsOffsetDate x (LoanFlow _d a b c d e f g h) = LoanFlow (T.addDays x _d) a b c d e f g h
tsOffsetDate x (LeaseFlow _d a b) = LeaseFlow (T.addDays x _d) a b

reduceTs :: [TsRow] -> TsRow -> [TsRow]
reduceTs [] _tr = [_tr]
reduceTs (tr:trs) _tr 
  | sameDate tr _tr = addTs tr _tr : trs -- `debug` ("Same date for "++show tr ++ show _tr)
  | otherwise = appendTs tr _tr : tr : trs  -- `debug` ("head of trs"++ show tr)

firstDate :: CashFlowFrame -> Date 
firstDate (CashFlowFrame rs) = getDate $ head rs

combine :: CashFlowFrame -> CashFlowFrame -> CashFlowFrame -- Left CF is earlier than Right CF
combine cf1@(CashFlowFrame rs1) cf2@(CashFlowFrame rs2)
  | ds1 == ds2 = CashFlowFrame $ (zipWith combineTs rs1 rs2)
  | fdRs1 == fdRs2 
    = combineCashFlow 
        (CashFlowFrame [combineTs (head rs1) (head rs2)])
        $ combine (CashFlowFrame (tail rs1)) (CashFlowFrame (tail rs2))
  | fdRs1 > fdRs2 = combine cf2 cf1
  | otherwise = 
      let 
        (ts_patch,ts_keep) = splitByDate rs1 fdRs2 EqToRight
        patch_bal = mflowBegBalance $ head rs2  --  `debug` ("rs2 -> \n"++ show rs2)
        ts_patched = [ addFlowBalance patch_bal y | y <- ts_patch ] -- `debug` ("patch bal \n "++ show patch_bal)
        sorted_cff = L.sortOn getDate (ts_keep++rs2) --  `debug` ("TS patched->\n"++ show ts_patched)
      in 
        CashFlowFrame $ ts_patched ++ (tail (reverse (foldl reduceTs [last ts_patched] sorted_cff)))   -- `debug` ("In sorted_cff"++ show sorted_cff)
  where 
     firstDateOfCfs r =  getDate $ head r -- the first date of cashflow
     (fdRs1,fdRs2) = (firstDateOfCfs rs1,firstDateOfCfs rs2)
     (ds1,ds2) = (getDate <$> rs1,getDate <$> rs2)

tsDateLT :: Date -> TsRow  -> Bool
tsDateLT td (CashFlow d _) = d < td
tsDateLT td (BondFlow d _ _ _) =  d < td
tsDateLT td (MortgageFlow d _ _ _ _ _ _ _ _ _ _) = d < td
tsDateLT td (LoanFlow d _ _ _ _ _ _ _ _) = d < td
tsDateLT td (LeaseFlow d _ _ ) = d < td

tsDateLET :: Date -> TsRow  -> Bool
tsDateLET td (CashFlow d _) = d <= td
tsDateLET td (BondFlow d _ _ _) =  d <= td
tsDateLET td (MortgageFlow d _ _ _ _ _ _ _ _ _ _) = d <= td
tsDateLET td (LoanFlow d _ _ _ _ _ _ _ _) = d <= td
tsDateLET td (LeaseFlow d _ _ ) = d <= td

aggTsByDates :: [TsRow] -> [Date] -> [TsRow]
aggTsByDates trs ds =
  map 
    (\(x,_d) -> sumTsCF x _d) 
    (filter 
      (\(y,__d) -> not (null y))
      (zip (reduceFn [] ds trs) ds)) -- `debug` ("Final agg >> "++ show (reduceFn [] ds trs) )
  where
    reduceFn accum _ [] =  accum  -- `debug` ("Returning->"++show(accum))
    reduceFn accum (cutoffDay:[]) _trs =
      accum ++ [(filter (\x -> getDate(x) <= cutoffDay) _trs)]
    reduceFn accum (cutoffDay:cutoffDays) _trs =
      case newAcc of
        [] -> reduceFn (accum++[[]]) cutoffDays _trs     --  `debug` ("Adding empty")
        newFlow -> reduceFn (accum++[newAcc]) cutoffDays rest --  `debug` ("Adding "++show(newAcc)++" cutoffDay "++show(cutoffDay))
      where
        (newAcc,rest) = L.partition (tsDateLET cutoffDay) _trs -- `debug` ("Spliting"++show cutoffDay++"From>>"++show _trs )


mflowPrincipal :: TsRow -> Balance
mflowPrincipal (MortgageFlow _ _ x _ _ _ _ _ _ _ _) = x
mflowPrincipal (LoanFlow _ _ x _ _ _ _ _ _) = x
mflowPrincipal _  = error "not supported"

mflowInterest :: TsRow -> Balance
mflowInterest (MortgageFlow _ _ _ x _ _ _ _ _ _ _) = x
mflowInterest (LoanFlow _ _ _ x _ _ _ _ _) = x
mflowInterest _  = error "not supported"

mflowPrepayment :: TsRow -> Balance
mflowPrepayment (MortgageFlow _ _ _ _ x _ _ _ _ _ _) = x
mflowPrepayment (LoanFlow _ _ _ _ x _ _ _ _) = x
mflowPrepayment _  = error "not supported"

mflowDefault :: TsRow -> Balance
mflowDefault (MortgageFlow _ _ _ _ _ x _ _ _ _ _) = x
mflowDefault (LoanFlow _ _ _ _ _ x _ _ _) = x
mflowDefault _  = error "not supported"

mflowRecovery :: TsRow -> Balance
mflowRecovery (MortgageFlow _ _ _ _ _ _ x _ _ _ _) = x
mflowRecovery (LoanFlow _ _ _ _ _ _ x _ _) = x
mflowRecovery _  = error "not supported"

mflowBalance :: TsRow -> Balance
mflowBalance (MortgageFlow _ x _ _ _ _ _ _ _ _ _) = x
mflowBalance (LoanFlow _ x _ _ _ _ _ _ _) = x
mflowBalance (LeaseFlow _ x _ ) = x

addFlowBalance :: Balance -> TsRow -> TsRow 
addFlowBalance 0 x = x
addFlowBalance b (MortgageFlow a x c d e f g h i j k ) = (MortgageFlow a (x+b) c d e f g h i j k)
addFlowBalance b (LoanFlow a x c d e f g i j) = (LoanFlow a (x+b) c d e f g i j)
addFlowBalance b (LeaseFlow a x c ) = (LeaseFlow a (x+b) c )

updateFlowBalance :: Balance -> TsRow -> TsRow 
updateFlowBalance b (MortgageFlow a x c d e f g h i j k ) = (MortgageFlow a b c d e f g h i j k)
updateFlowBalance b (LoanFlow a x c d e f g i j) = (LoanFlow a b c d e f g i j)
updateFlowBalance b (LeaseFlow a x c ) = (LeaseFlow a b c )

mflowBegBalance :: TsRow -> Balance
mflowBegBalance (MortgageFlow _ x p _ ppy def _ _ _ _ _) = x + p + ppy + def
mflowBegBalance (LoanFlow _ x p _ ppy def _ _ _) = x + p + ppy + def
mflowBegBalance (LeaseFlow _ b r) = b + r

mflowLoss :: TsRow -> Balance
mflowLoss (MortgageFlow _ _ _ _ _ _ _ x _ _ _) = x
mflowLoss (LoanFlow _ _ _ _ _ _ _ x _) = x

mflowRate :: TsRow -> IRate
mflowRate (MortgageFlow _ _ _ _ _ _ _ _ x _ _) = x
mflowRate (LoanFlow _ _ _ _ _ _ _ _ x) = x

mflowRental :: TsRow -> Amount
mflowRental (LeaseFlow _ _ x ) = x

mflowDate :: TsRow -> Date
mflowDate (MortgageFlow x _ _ _ _ _ _ _ _ _ _) = x
mflowDate (LoanFlow x _ _ _ _ _ _ _ _) = x
mflowDate (LeaseFlow x _ _ ) = x

mflowAmortAmount :: TsRow -> Balance
mflowAmortAmount (MortgageFlow _ _ x _ y z _ _ _ _ _) = x + y + z
mflowAmortAmount (LoanFlow _ _ x _ y z _ _ _) = x + y + z
mflowAmortAmount (LeaseFlow _ _ x ) = x

mflowBorrowerNum :: TsRow -> Maybe BorrowerNum
mflowBorrowerNum (MortgageFlow _ _ _ _ _ _ _ _ _ x _) = x
mflowBorrowerNum _ = undefined

mflowPrepaymentPenalty :: TsRow -> Balance
mflowPrepaymentPenalty (MortgageFlow _ _ _ _ _ _ _ _ _ _ (Just x)) = x
mflowPrepaymentPenalty (MortgageFlow _ _ _ _ _ _ _ _ _ _ Nothing) = 0
mflowPrepaymentPenalty _ = undefined

mflowWeightAverageBalance :: Date -> Date -> [TsRow] -> Balance
mflowWeightAverageBalance sd ed trs
  = sum $ zipWith mulBR _bals _dfs  -- `debug` ("CalcingAvgBal=>"++show sd++show ed++show txns  )
    where
     txns = filter (\x -> (mflowDate x>=sd)&&(mflowDate x)<=ed) trs
     _ds = map mflowDate txns -- `debug` ("fee base txns"++show txns)
     _bals = map mflowBegBalance txns
     _dfs =  getIntervalFactors $ [sd]++_ds

appendCashFlow :: CashFlowFrame -> [TsRow] -> CashFlowFrame
appendCashFlow (CashFlowFrame _tsr) tsr 
  = CashFlowFrame $ _tsr ++ tsr

emptyTsRow :: Date -> TsRow -> TsRow 
emptyTsRow _d (MortgageFlow a x c d e f g h i j k) = (MortgageFlow _d 0 0 0 0 0 0 0 0 Nothing Nothing)
emptyTsRow _d (LoanFlow a x c d e f g i j) = (LoanFlow _d 0 0 0 0 0 0 0 0)
emptyTsRow _d (LeaseFlow a x c ) = (LeaseFlow _d 0 0 )

-- | given a row ,build a new cf row with begin balance
buildBegTsRow :: Date -> TsRow -> TsRow
buildBegTsRow d tr 
  = (tsSetBalance (mflowBalance tr + mflowAmortAmount tr)) (emptyTsRow d tr)


combineCashFlow :: CashFlowFrame -> CashFlowFrame -> CashFlowFrame
combineCashFlow cf1 (CashFlowFrame txn) 
  = appendCashFlow cf1 txn

totalLoss :: CashFlowFrame -> Balance
totalLoss (CashFlowFrame rs) = sum $ mflowLoss <$> rs

totalDefault :: CashFlowFrame -> Balance
totalDefault (CashFlowFrame rs) = sum $ mflowDefault <$> rs

totalRecovery :: CashFlowFrame -> Balance
totalRecovery (CashFlowFrame rs) = sum $ mflowRecovery <$> rs

mergePoolCf :: CashFlowFrame -> CashFlowFrame -> CashFlowFrame
mergePoolCf cf (CashFlowFrame []) = cf
mergePoolCf (CashFlowFrame []) cf = cf
mergePoolCf cf1@(CashFlowFrame txns1) cf2@(CashFlowFrame txns2) -- first day of left is earlier than right one
  | startDate1 > startDate2 = mergePoolCf cf2 cf1 
  | otherwise 
      = let 
          splitDate = firstDate cf2
          (CashFlowFrame txn0,cfToBeMerged) = splitCashFlowFrameByDate cf1 splitDate EqToRight
          (CashFlowFrame txn1) = combine cfToBeMerged cf2 -- `debug` ("left"++show cfToBeMerged++">> right"++ show cf2)
        in 
          CashFlowFrame (txn0++txn1) -- `debug` ("Txn1"++show txn1)
  where 
    [startDate1,startDate2] =  firstDate <$> [cf1,cf2]
    rightToLeft = startDate1 >= startDate2

shiftCfToStartDate :: Date -> CashFlowFrame -> CashFlowFrame
shiftCfToStartDate d cf@(CashFlowFrame (txn:txns))
  = let 
      fstDate = firstDate cf 
      diffDays = daysBetween fstDate d
    in 
      CashFlowFrame $ (tsOffsetDate diffDays) <$> (txn:txns)

sumPoolFlow :: CashFlowFrame -> PoolSource -> Balance
sumPoolFlow (CashFlowFrame trs) ps 
  = sum $ lookup ps <$> trs
    where
      lookup CollectedPrepayment  = mflowPrepayment
      lookup CollectedPrincipal = mflowPrincipal
      lookup CollectedRecoveries = mflowRecovery
      lookup CollectedRental = mflowRental
      lookup CollectedInterest = mflowInterest

lookupSource :: TsRow -> PoolSource -> Balance 
lookupSource tr CollectedPrepayment  = mflowPrepayment tr
lookupSource tr CollectedPrincipal = mflowPrincipal tr
lookupSource tr CollectedRecoveries = mflowRecovery tr
lookupSource tr CollectedRental = mflowRental tr
lookupSource tr CollectedInterest = mflowInterest tr
lookupSource tr CollectedPrepaymentPenalty = mflowPrepaymentPenalty tr
lookupSource tr NewDefaults = mflowDefault tr
lookupSource tr NewLosses = mflowLoss tr

setPrepaymentPenalty :: Balance -> TsRow -> TsRow
setPrepaymentPenalty bal (MortgageFlow a b c d e f g h i j k) = MortgageFlow a b c d e f g h i j (Just bal)
setPrepaymentPenalty _ _ = error "prepay pental only applies to MortgageFlow"

setPrepaymentPenaltyFlow :: [Balance] -> [TsRow] -> [TsRow]
setPrepaymentPenaltyFlow bals trs = [ setPrepaymentPenalty bal tr | (bal,tr) <- zip bals trs]

$(deriveJSON defaultOptions ''TsRow)
$(deriveJSON defaultOptions ''CashFlowFrame)
