{-# LANGUAGE TemplateHaskell       #-}

module Cashflow (CashFlowFrame(..),Principals,Interests,Amount
                ,combine
                ,sizeCashFlowFrame, aggTsByDates, getTsCashFlowFrame
                ,mflowInterest,mflowPrincipal,mflowRecovery,mflowPrepayment
                ,mflowRental
                ,mflowDefault,mflowLoss,mflowDate
                ,getSingleTsCashFlowFrame,removeTsCashFlowFrameByDate,getDatesCashFlowFrame
                ,getEarlierTsCashFlowFrame
                ,mflowBalance,mflowBegBalance,tsDefaultBal,getAllAfterCashFlowFrame
                ,mflowBorrowerNum
                ,getAllBeforeCashFlowFrame,splitCashFlowFrameByDate
                ,tsTotalCash,Date -- ,PersonalLoanFlow
                ,getTxnAsOf,tsDateLT,getDate,getTxnLatestAsOf,getTxnAfter
                ,getTxnBetween,getTxnBetween2
                ,mflowWeightAverageBalance,appendCashFlow,combineCashFlow
                ,TsRow(..),cfAt) where

import Data.Time (Day)
import Data.Fixed
import Lib (weightedBy,toDate,getIntervalFactors)
import Util (mulBR,splitByDate)
import Types
import qualified Data.Map as Map
import qualified Data.Time as T
import qualified Data.List as L

import Data.Aeson hiding (json)
import Language.Haskell.TH
import Data.Aeson.TH
import Data.Aeson.Types

import Text.Printf

import Debug.Trace
debug = flip trace

type Delinquent = Centi
type Delinquent30 = Centi
type Delinquent60 = Centi
type Delinquent90 = Centi
type Delinquent120 = Centi

-- type Date = T.Day

type Amounts = [Float]
-- type Balances = [Balance]
type Principals = [Principal]
type Interests = [Interest]
type Prepayments = [Prepayment]
type Recoveries = [Recovery]
type Rates = [Rate]

data ColType = ColNum Centi 
             | ColDate Date 
             | ColBal Centi 
             | ColRate IRate
             deriving (Show)

data TsRow = CashFlow Date Amount
           | BondFlow Date Balance Principal Interest
           | MortgageFlow Date Balance Principal Interest Prepayment Default Recovery Loss IRate (Maybe BorrowerNum)
           | MortgageFlow2 Date Balance Principal Interest Prepayment Delinquent Default Recovery Loss IRate
           | MortgageFlow3 Date Balance Principal Interest Prepayment Delinquent30 Delinquent60 Delinquent90 Default Recovery Loss IRate
           | LoanFlow Date Balance Principal Interest Prepayment Default Recovery Loss IRate
           | LeaseFlow Date Balance Rental
           deriving(Show,Eq,Ord)

instance TimeSeries TsRow where 
    cmp tr1 tr2 = compare (getDate tr1) (getDate tr2)
    sameDate tr1 tr2 = (getDate tr1) == (getDate tr2)
    getDate (CashFlow x _) = x
    getDate (BondFlow x  _ _ _) = x
    getDate (MortgageFlow x _ _ _ _ _ _ _ _ _ ) = x
    getDate (MortgageFlow2 x _ _ _ _ _ _ _ _ _) = x
    getDate (MortgageFlow3 x _ _ _ _ _ _ _ _ _ _ _) = x
    getDate (LoanFlow x _ _ _ _ _ _ _ _) = x
    getDate (LeaseFlow x _ _ ) = x
    getDates txns = map getDate txns

data CashFlowFrame = CashFlowFrame [TsRow]
                   deriving (Show,Eq)

sizeCashFlowFrame :: CashFlowFrame -> Int
sizeCashFlowFrame (CashFlowFrame ts) = length ts

getTsCashFlowFrame :: CashFlowFrame -> [TsRow]
getTsCashFlowFrame (CashFlowFrame ts) = ts

getDatesCashFlowFrame :: CashFlowFrame -> [Date]
getDatesCashFlowFrame cff =  map getDate $ getTsCashFlowFrame cff

removeTsCashFlowFrameByDate :: CashFlowFrame -> Date -> Maybe CashFlowFrame
removeTsCashFlowFrameByDate (CashFlowFrame trs) d =
  let
    r = filter (\x -> getDate x /= d) trs
  in
    if null r then
      Nothing
    else
      Just (CashFlowFrame r)

cfAt :: CashFlowFrame -> Int -> Maybe TsRow
cfAt (CashFlowFrame trs) idx = 
    if (idx < 0) || (idx >= length trs) then
        Nothing
    else
        Just (trs!!idx)

getSingleTsCashFlowFrame :: CashFlowFrame -> Date -> TsRow
getSingleTsCashFlowFrame (CashFlowFrame trs) d
  = head $ filter (\x -> getDate x == d) trs

getEarlierTsCashFlowFrame :: CashFlowFrame -> Date -> Maybe TsRow
getEarlierTsCashFlowFrame (CashFlowFrame trs) d
  = L.find (tsDateLT d) (reverse trs)

getAllBeforeCashFlowFrame :: CashFlowFrame -> Date -> Maybe CashFlowFrame
getAllBeforeCashFlowFrame cf@(CashFlowFrame trx) d
  =
   let
     txn = getTxnAsOf cf d
   in
     case txn of
       [] -> Nothing
       _ -> Just (CashFlowFrame txn)

getAllAfterCashFlowFrame :: CashFlowFrame -> Date -> Maybe CashFlowFrame
getAllAfterCashFlowFrame cf@(CashFlowFrame trx) d
  =
   let
     txn = getTxnAfter cf d
   in
     case txn of
       [] -> Nothing
       _ -> Just (CashFlowFrame txn)

splitCashFlowFrameByDate :: CashFlowFrame -> Date -> SplitType  -> (CashFlowFrame,CashFlowFrame)
splitCashFlowFrameByDate (CashFlowFrame txns) d st
  = let 
      (ls,rs) = splitByDate txns d st
    in 
      (CashFlowFrame ls,CashFlowFrame rs)

getTxnAsOf :: CashFlowFrame -> Date -> [TsRow]
getTxnAsOf (CashFlowFrame txn) d = filter (\x -> getDate x < d) txn

getTxnAfter :: CashFlowFrame -> Date -> [TsRow]
getTxnAfter (CashFlowFrame txn) d = filter (\x -> getDate x >= d) txn

getTxnBetween :: CashFlowFrame -> Date -> Date -> [TsRow]
getTxnBetween (CashFlowFrame txn) sd ed
  =  filter (\x -> ((getDate x) >= sd) && ((getDate x) < ed)) txn

getTxnBetween2 :: CashFlowFrame -> RangeType -> Date -> Date -> [TsRow]
getTxnBetween2 (CashFlowFrame txn) rt sd ed
  =  case rt of 
       II -> filter (\x -> (getDate x >= sd) && (getDate x <= ed)) txn
       IE -> filter (\x -> (getDate x >= sd) && (getDate x < ed)) txn
       EI -> filter (\x -> (getDate x > sd) && (getDate x <= ed)) txn

getTxnLatestAsOf :: CashFlowFrame -> Date -> Maybe TsRow
getTxnLatestAsOf (CashFlowFrame txn) d = L.find (\x -> getDate x <= d) $ reverse txn

addTs :: TsRow -> TsRow -> TsRow
addTs (CashFlow d1 a1 ) (CashFlow _ a2 ) = (CashFlow d1 (a1 + a2))
addTs (BondFlow d1 b1 p1 i1 ) (BondFlow _ b2 p2 i2 ) = (BondFlow d1 (b1 + b2) (p1 + p2) (i1 + i2) )
addTs (MortgageFlow d1 b1 p1 i1 prep1 def1 rec1 los1 rat1 mbn1) (MortgageFlow _ b2 p2 i2 prep2 def2 rec2 los2 rat2 mbn2)
  = let 
      bn = do bn1 <- mbn1
              bn2 <- mbn2
              return (bn1 + bn2)
    in 
      (MortgageFlow d1 (b1 + b2) (p1 + p2) (i1 + i2) (prep1 + prep2) (def1 + def2) (rec1 + rec2) (los1+los2) (fromRational (weightedBy [b1,b2] (map toRational [rat1,rat2]))) bn)
addTs (MortgageFlow2 d1 b1 p1 i1 prep1 del1 def1 rec1 los1 rat1) (MortgageFlow2 _ b2 p2 i2 prep2 del2 def2 rec2 los2 rat2)
  = (MortgageFlow2 d1 (b1 + b2) (p1 + p2) (i1 + i2) (prep1 + prep2) (del1+del2) (def1 + def2) (rec1 + rec2) (los1+los2) (fromRational (weightedBy [b1,b2] (map toRational [rat1,rat2]))))
addTs (MortgageFlow3 d1 b1 p1 i1 prep1 del13 del16 del19 def1 rec1 los1 rat1) (MortgageFlow3 _ b2 p2 i2 prep2 del23 del26 del29 def2 rec2 los2 rat2)
  = (MortgageFlow3 d1 (b1 + b2) (p1 + p2) (i1 + i2) (prep1 + prep2) (del13+del23) (del16+del26) (del19+del29) (def1 + def2) (rec1 + rec2) (los1+los2) (fromRational (weightedBy [b1,b2] (map toRational [rat1,rat2]))))
addTs (LoanFlow d1 b1 p1 i1 prep1 def1 rec1 los1 rat1) (LoanFlow _ b2 p2 i2 prep2 def2 rec2 los2 rat2)
  = (LoanFlow d1 (b1 + b2) (p1 + p2) (i1 + i2) (prep1 + prep2) (def1 + def2) (rec1 + rec2) (los1+los2) (fromRational (weightedBy [b1,b2] (map toRational [rat1,rat2]))))
addTs (LeaseFlow d1 b1 r1) (LeaseFlow d2 b2 r2) = (LeaseFlow d1 (b1 + b2) (r1 + r2) )

addTsCF :: TsRow -> TsRow -> TsRow
addTsCF (CashFlow d1 a1 ) (CashFlow _ a2 ) = (CashFlow d1 (a1 + a2))
addTsCF (BondFlow d1 b1 p1 i1 ) (BondFlow _ b2 p2 i2 ) = (BondFlow d1 (min b1 b2) (p1 + p2) (i1 + i2) )
addTsCF (MortgageFlow d1 b1 p1 i1 prep1 def1 rec1 los1 rat1 mbn1) (MortgageFlow d2 b2 p2 i2 prep2 def2 rec2 los2 rat2 mbn2)
  = let 
      bn =  do bn1 <- mbn1 
               bn2 <- mbn2 
               return (min bn1 bn2)
    in 
      (MortgageFlow d2 (min b1 b2) (p1 + p2) (i1 + i2) (prep1 + prep2) (def1 + def2) (rec1 + rec2) (los1+los2) (fromRational (weightedBy [b1,b2] (map toRational [rat1,rat2]))) bn)
addTsCF (MortgageFlow2 d1 b1 p1 i1 prep1 del1 def1 rec1 los1 rat1) (MortgageFlow2 _ b2 p2 i2 prep2 del2 def2 rec2 los2 rat2)
  = (MortgageFlow2 d1 (min b1 b2) (p1 + p2) (i1 + i2) (prep1 + prep2) (del1 + del2) (def1 + def2) (rec1 + rec2) (los1+los2) (fromRational (weightedBy [b1,b2] (map toRational [rat1,rat2]) )))
addTsCF (MortgageFlow3 d1 b1 p1 i1 prep1 del13 del16 del19 def1 rec1 los1 rat1) (MortgageFlow3 _ b2 p2 i2 prep2 del23 del26 del29 def2 rec2 los2 rat2)
  = (MortgageFlow3 d1 (min b1 b2) (p1 + p2) (i1 + i2) (prep1 + prep2) (del13+del23) (del16+del26) (del19+del29) (def1 + def2) (rec1 + rec2) (los1+los2) (fromRational (weightedBy [b1,b2] (map toRational [rat1,rat2]))) )
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
tsTotalCash (MortgageFlow x _ _ a b c _ e _ _) = a + b + c + e
tsTotalCash (MortgageFlow2 x _ _ a b c _ _ e _) = a + b + c + e
tsTotalCash (MortgageFlow3 x _ _ a b c _ _ _ _ e _) = a + b + c + e
tsTotalCash (LoanFlow _ _ a b c _ e _ _) =  a + b + c + e
tsTotalCash (LeaseFlow _ _ a) =  a

tsDefaultBal :: TsRow -> Balance
tsDefaultBal (CashFlow _ _) = 0
tsDefaultBal (BondFlow _ _ _ _) = 0
tsDefaultBal (MortgageFlow _ _ _ _ _ x _ _ _ _) = x
tsDefaultBal (MortgageFlow2 _ _ _ _ _ _ x _ _ _) = x
tsDefaultBal (MortgageFlow3 _ _ _ _ _ _ _ _ x _ _ _) = x
tsDefaultBal (LoanFlow _ _ _ _ _ x _ _ _) = x

tsSetDate :: TsRow -> Date ->TsRow
tsSetDate (CashFlow _ a) x  = CashFlow x a
tsSetDate (BondFlow _ a b c) x = BondFlow x a b c
tsSetDate (MortgageFlow _ a b c d e f g h i) x = MortgageFlow x a b c d e f g h i
tsSetDate (MortgageFlow2 _ a b c d e f g h i) x = MortgageFlow2 x a b c d e f g h i
tsSetDate (MortgageFlow3 _ a b c d e f g h i j k) x = MortgageFlow3 x a b c d e f g h i j k
tsSetDate (LoanFlow _ a b c d e f g h) x = LoanFlow x a b c d e f g h
tsSetDate (LeaseFlow _ a b) x = LeaseFlow x a b

reduceTs :: [TsRow] -> TsRow -> [TsRow]
reduceTs [] _tr = [_tr]
reduceTs (tr:trs) _tr 
  | sameDate tr _tr = (addTs tr _tr):trs
  | otherwise =  _tr:tr:trs

combine :: CashFlowFrame -> CashFlowFrame -> CashFlowFrame
combine (CashFlowFrame rs1) (CashFlowFrame rs2) =
    CashFlowFrame $  foldl reduceTs [] sorted_cff --  `debug` ("In Combine")
    where cff = rs1++rs2
          sorted_cff = L.sortOn getDate cff  -- `debug` ("BEFORE sort"++ show cff)

tsDateLT :: Date -> TsRow  -> Bool
tsDateLT td (CashFlow d _) = d < td
tsDateLT td (BondFlow d _ _ _) =  d < td
tsDateLT td (MortgageFlow d _ _ _ _ _ _ _ _ _) = d < td
tsDateLT td (MortgageFlow2 d _ _ _ _ _ _ _ _ _) = d < td
tsDateLT td (MortgageFlow3 d _ _ _ _ _ _ _ _ _ _ _) = d < td
tsDateLT td (LoanFlow d _ _ _ _ _ _ _ _) = d < td
tsDateLT td (LeaseFlow d _ _ ) = d < td

tsDateLET :: Date -> TsRow  -> Bool
tsDateLET td (CashFlow d _) = d <= td
tsDateLET td (BondFlow d _ _ _) =  d <= td
tsDateLET td (MortgageFlow d _ _ _ _ _ _ _ _ _) = d <= td
tsDateLET td (MortgageFlow2 d _ _ _ _ _ _ _ _ _) = d <= td
tsDateLET td (MortgageFlow3 d _ _ _ _ _ _ _ _ _ _ _) = d <= td
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
mflowPrincipal (MortgageFlow _ _ x _ _ _ _ _ _ _) = x
mflowPrincipal (MortgageFlow2 _ _ x _ _ _ _ _ _ _) = x
mflowPrincipal (MortgageFlow3 _ _ x _ _ _ _ _ _ _ _ _) = x
mflowPrincipal (LoanFlow _ _ x _ _ _ _ _ _) = x
mflowPrincipal _  = -1.0

mflowInterest :: TsRow -> Balance
mflowInterest (MortgageFlow _ _ _ x _ _ _ _ _ _) = x
mflowInterest (MortgageFlow2 _ _ _ x _ _ _ _ _ _) = x
mflowInterest (MortgageFlow3 _ _ _ x _ _ _ _ _ _ _ _) = x
mflowInterest (LoanFlow _ _ _ x _ _ _ _ _) = x
mflowInterest _  = -1.0

mflowPrepayment :: TsRow -> Balance
mflowPrepayment (MortgageFlow _ _ _ _ x _ _ _ _ _) = x
mflowPrepayment (MortgageFlow2 _ _ _ _ x _ _ _ _ _) = x
mflowPrepayment (MortgageFlow3 _ _ _ _ x _ _ _ _ _ _ _) = x
mflowPrepayment (LoanFlow _ _ _ _ x _ _ _ _) = x
mflowPrepayment _  = -1.0

mflowDefault :: TsRow -> Balance
mflowDefault (MortgageFlow _ _ _ _ _ x _ _ _ _) = x
mflowDefault (MortgageFlow2 _ _ _ _ _ _ x _ _ _) = x
mflowDefault (MortgageFlow3 _ _ _ _ _ _ _ _ x _ _ _) = x
mflowDefault (LoanFlow _ _ _ _ _ x _ _ _) = x
mflowDefault _  = -1.0

mflowRecovery :: TsRow -> Balance
mflowRecovery (MortgageFlow _ _ _ _ _ _ x _ _ _) = x
mflowRecovery (MortgageFlow2 _ _ _ _ _ _ _ x _ _) = x
mflowRecovery (MortgageFlow3 _ _ _ _ _ _ _ _ _ x _ _) = x
mflowRecovery (LoanFlow _ _ _ _ _ _ x _ _) = x
mflowRecovery _  = -1.0

mflowBalance :: TsRow -> Balance
mflowBalance (MortgageFlow _ x _ _ _ _ _ _ _ _) = x
mflowBalance (MortgageFlow2 _ x _ _ _ _ _ _ _ _) = x
mflowBalance (MortgageFlow3 _ x _ _ _ _ _ _ _ _ _ _) = x
mflowBalance (LoanFlow _ x _ _ _ _ _ _ _) = x
mflowBalance (LeaseFlow _ x _ ) = x

mflowBegBalance :: TsRow -> Balance
mflowBegBalance (MortgageFlow _ x p _ ppy def _ _ _ _) = x + p + ppy + def
mflowBegBalance (MortgageFlow2 _ x p _ ppy _ def _ _ _) = x + p + ppy + def
mflowBegBalance (MortgageFlow3 _ x p _ ppy _ _ _ def _ _ _) = x + p + ppy + def
mflowBegBalance (LoanFlow _ x p _ ppy def _ _ _) = x + p + ppy + def
mflowBegBalance (LeaseFlow _ b r) = b + r

mflowLoss :: TsRow -> Balance
mflowLoss (MortgageFlow _ _ _ _ _ _ _ x _ _) = x
mflowLoss (MortgageFlow2 _ _ _ _ _ _ _ _ x _) = x
mflowLoss (MortgageFlow3 _ _ _ _ _ _ _ _ _ _ x _) = x
mflowLoss (LoanFlow _ _ _ _ _ _ _ x _) = x

mflowRate :: TsRow -> IRate
mflowRate (MortgageFlow _ _ _ _ _ _ _ _ x _) = x
mflowRate (MortgageFlow2 _ _ _ _ _ _ _ _ _ x) = x
mflowRate (MortgageFlow3 _ _ _ _ _ _ _ _ _ _ _ x) = x
mflowRate (LoanFlow _ _ _ _ _ _ _ _ x) = x

mflowRental :: TsRow -> Amount
mflowRental (LeaseFlow _ _ x ) = x

mflowDate :: TsRow -> Date
mflowDate (MortgageFlow x _ _ _ _ _ _ _ _ _) = x
mflowDate (MortgageFlow2 x _ _ _ _ _ _ _ _ _) = x
mflowDate (MortgageFlow3 x _ _ _ _ _ _ _ _ _ _ _) = x
mflowDate (LoanFlow x _ _ _ _ _ _ _ _) = x
mflowDate (LeaseFlow x _ _ ) = x

mflowBorrowerNum :: TsRow -> Maybe BorrowerNum
mflowBorrowerNum (MortgageFlow _ _ _ _ _ _ _ _ _ x) = x
mflowBorrowerNum _ = undefined

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

combineCashFlow :: CashFlowFrame -> CashFlowFrame -> CashFlowFrame
combineCashFlow cf1 (CashFlowFrame txn) 
  = appendCashFlow cf1 txn


$(deriveJSON defaultOptions ''TsRow)
$(deriveJSON defaultOptions ''CashFlowFrame)
