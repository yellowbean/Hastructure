{-# LANGUAGE TemplateHaskell       #-}


module Cashflow (CashFlowFrame(..),Principals,Interests,Amount
                ,mkCashFlowFrame,mkColDay,mkColNum,mkColBal,combine
                ,sizeCashFlowFrame, aggTsByDates, getTsCashFlowFrame
                ,mflowInterest,mflowPrincipal,mflowRecovery,mflowPrepayment
                ,mflowDefault,mflowLoss,mflowDate
                ,getSingleTsCashFlowFrame,removeTsCashFlowFrameByDate
                ,getEarlierTsCashFlowFrame
                ,mflowBalance,mflowBegBalance,tsDefaultBal,getAllAfterCashFlowFrame
                ,getAllBeforeCashFlowFrame,splitCashFlowFrameByDate
                ,tsTotalCash,Date -- ,PersonalLoanFlow
                ,getTxnAsOf,tsDateLT,tsDate,getTxnLatestAsOf,getTxnAfter
                ,getTxnBetween,getTxnBetween2
                ,mflowWeightAverageBalance
                ,TsRow(..)) where

import Data.Time (Day)
import Data.Fixed
import Lib (weightedBy,toDate,IRate,getIntervalFactors)
import Util (mulBR)
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

type Interest = Centi
type Principal = Centi
-- type Balance = Centi
type Amount = Centi
type Prepayment = Centi
type Recovery = Centi
type Delinquent = Centi
type Delinquent30 = Centi
type Delinquent60 = Centi
type Delinquent90 = Centi
type Delinquent120 = Centi
type Default = Centi
type Loss = Centi
type Rate = Rational
-- type Date = T.Day

type Amounts = [Float]
-- type Balances = [Balance]
type Principals = [Principal]
type Interests = [Interest]
type Prepayments = [Prepayment]
type Recoveries = [Recovery]
type Rates = [Rate]

data ColType = ColNum Centi | ColDate Date | ColBal Centi | ColRate IRate
    deriving (Show)

data TsRow = CashFlow Date Amount
           |BondFlow Date Balance Principal Interest
           -- |FeeFlow Date Balance Amount
           -- |AccountFlow Date Balance Amount
           |MortgageFlow Date Balance Principal Interest Prepayment Default Recovery Loss IRate
           |MortgageFlow2 Date Balance Principal Interest Prepayment Delinquent Default Recovery Loss IRate
           |MortgageFlow3 Date Balance Principal Interest Prepayment Delinquent30 Delinquent60 Delinquent90 Default Recovery Loss IRate
           |PersonalLoanFlow Date Balance Principal Interest Prepayment Default Recovery Loss IRate
           deriving(Show)

instance Ord TsRow where
  compare (CashFlow d1 _) (CashFlow d2 _) = compare d1 d2
  compare (BondFlow d1 _ _ _) (BondFlow d2 _ _ _) = compare d1 d2
  compare (MortgageFlow d1 _ _ _ _ _ _ _ _) (MortgageFlow d2 _ _ _ _ _ _ _ _) = compare d1 d2
  compare (MortgageFlow2 d1 _ _ _ _ _ _ _ _ _) (MortgageFlow2 d2 _ _ _ _ _ _ _ _ _) = compare d1 d2
  compare (MortgageFlow3 d1 _ _ _ _ _ _ _ _ _ _ _) (MortgageFlow3 d2 _ _ _ _ _ _ _ _ _ _ _) = compare d1 d2

instance Eq TsRow where
  (CashFlow d1 _) == (CashFlow d2 _) = d1 == d2
  (BondFlow d1 _ _ _) == (BondFlow d2 _ _ _) = d1 == d2
  (MortgageFlow d1 _ _ _ _ _ _ _ _) == (MortgageFlow d2 _ _ _ _ _ _ _ _) = d1 == d2
  (MortgageFlow2 d1 _ _ _ _ _ _ _ _ _) == (MortgageFlow2 d2 _ _ _ _ _ _ _ _ _) = d1 == d2
  (MortgageFlow3 d1 _ _ _ _ _ _ _ _ _ _ _) == (MortgageFlow3 d2 _ _ _ _ _ _ _ _ _ _ _) = d1 == d2

--instance Show TsRow where
--  show (CashFlow d f1) = "Cashflow "++ show(d) ++ printf " %.2f" f1
--  show (BondFlow d f1 f2 f3) = "BondFlow " ++show(d) ++ printf " %.2f %.2f %.2f" f1 f2 f3
--  show (MortgageFlow d f1 f2 f3 f4 f5 f6 f7 f8 ) = "MortgageFlow " ++show(d) ++ printf " %.2f %.2f %.2f %.2f %.2f %.2f %.2f %.2f" f1 f2 f3 f4 f5 f6 f7 f8
--  show (MortgageFlow2 d f1 f2 f3 f4 f5 f6 f7 f8 f9) = "MortgageFlow2 " ++show(d) ++ printf " %.2f %.2f %.2f %.2f %.2f %.2f %.2f %.2f %.2f" f1 f2 f3 f4 f5 f6 f7 f8 f9
--  show (MortgageFlow3 d f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11) = "MortgageFlow3 " ++show(d) ++ printf " %.2f %.2f %.2f %.2f %.2f %.2f %.2f %.2f %.2f %.2f %.2f" f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11



data CashFlowFrame = CashFlowFrame [TsRow]
              deriving (Show)
                -- |BondFrame [BondFlow]
                -- |MortgageFrame [MortgageFlow]

mkRow :: [ColType] -> TsRow
mkRow (ColDate d:ColBal b:ColNum prin:ColNum i:ColNum pre:ColBal def_b:ColNum rec:ColNum los:ColRate rat:[])
  = MortgageFlow d b prin i pre def_b rec los rat

mkCashFlowFrame :: [[ColType]] -> CashFlowFrame
mkCashFlowFrame xss = CashFlowFrame $ map mkRow xss

sizeCashFlowFrame :: CashFlowFrame -> Int
sizeCashFlowFrame (CashFlowFrame ts) = length ts

getTsCashFlowFrame :: CashFlowFrame -> [TsRow]
getTsCashFlowFrame (CashFlowFrame ts) = ts

removeTsCashFlowFrameByDate :: CashFlowFrame -> Date -> Maybe CashFlowFrame
removeTsCashFlowFrameByDate (CashFlowFrame trs) d =
  let
    r = filter (\x -> tsDate x /= d) trs
  in
    if null r then
      Nothing
    else
      Just (CashFlowFrame r)

getSingleTsCashFlowFrame :: CashFlowFrame -> Date -> TsRow
getSingleTsCashFlowFrame (CashFlowFrame trs) d
  = head $ filter (\x -> tsDate x == d) trs

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

splitCashFlowFrameByDate :: CashFlowFrame -> Date -> (Maybe CashFlowFrame, Maybe CashFlowFrame)
splitCashFlowFrameByDate (CashFlowFrame txns) d
  = let
      --l = filter (\x -> tsDate x < d) txns  `debug` (show(l))
      --r = filter (\x -> tsDate x >= d) txns `debug` (show(r))
      p = L.partition (tsDateLET d) txns
    in
      case p of
        ([], []) -> (Nothing,Nothing)
        ([], _r) -> (Nothing, Just (CashFlowFrame _r))
        (_l, []) -> (Just (CashFlowFrame _l), Nothing)
        (_l, _r) -> (Just (CashFlowFrame _l), Just (CashFlowFrame _r)) --  `debug` (show(p))


getTxnAsOf :: CashFlowFrame -> Date -> [TsRow]
getTxnAsOf (CashFlowFrame txn) d = filter (\x -> tsDate x < d) txn

getTxnAfter :: CashFlowFrame -> Date -> [TsRow]
getTxnAfter (CashFlowFrame txn) d = filter (\x -> tsDate x >= d) txn

getTxnBetween :: CashFlowFrame -> Date -> Date -> [TsRow]
getTxnBetween (CashFlowFrame txn) sd ed
  =  filter (\x -> ((tsDate x) >= sd) && ((tsDate x) < ed)) txn

getTxnBetween2 :: CashFlowFrame -> RangeType -> Date -> Date -> [TsRow]
getTxnBetween2 (CashFlowFrame txn) rt sd ed
  =  case rt of 
       II -> filter (\x -> (tsDate x >= sd) && (tsDate x <= ed)) txn
       IE -> filter (\x -> (tsDate x >= sd) && (tsDate x < ed)) txn
       EI -> filter (\x -> (tsDate x > sd) && (tsDate x <= ed)) txn

getTxnLatestAsOf :: CashFlowFrame -> Date -> Maybe TsRow
getTxnLatestAsOf (CashFlowFrame txn) d = L.find (\x -> tsDate x <= d) $ reverse txn

mkColDay :: [Date] -> [ColType]
mkColDay ds = [ ColDate _d | _d <- ds ]

mkColNum :: [Centi] -> [ColType]
mkColNum ds = [ ColNum _d | _d <- ds ]

mkColBal :: [Centi] -> [ColType]
mkColBal ds = [ ColBal _d | _d <- ds ]

addTs :: TsRow -> TsRow -> TsRow
addTs (CashFlow d1 a1 ) (CashFlow _ a2 ) = (CashFlow d1 (a1 + a2))
addTs (BondFlow d1 b1 p1 i1 ) (BondFlow _ b2 p2 i2 ) = (BondFlow d1 (b1 + b2) (p1 + p2) (i1 + i2) )
addTs (MortgageFlow d1 b1 p1 i1 prep1 def1 rec1 los1 rat1) (MortgageFlow _ b2 p2 i2 prep2 def2 rec2 los2 rat2)
  = (MortgageFlow d1 (b1 + b2) (p1 + p2) (i1 + i2) (prep1 + prep2) (def1 + def2) (rec1 + rec2) (los1+los2) (fromRational (weightedBy [b1,b2] (map toRational [rat1,rat2]))))
addTs (MortgageFlow2 d1 b1 p1 i1 prep1 del1 def1 rec1 los1 rat1) (MortgageFlow2 _ b2 p2 i2 prep2 del2 def2 rec2 los2 rat2)
  = (MortgageFlow2 d1 (b1 + b2) (p1 + p2) (i1 + i2) (prep1 + prep2) (del1+del2) (def1 + def2) (rec1 + rec2) (los1+los2) (fromRational (weightedBy [b1,b2] (map toRational [rat1,rat2]))))
addTs (MortgageFlow3 d1 b1 p1 i1 prep1 del13 del16 del19 def1 rec1 los1 rat1) (MortgageFlow3 _ b2 p2 i2 prep2 del23 del26 del29 def2 rec2 los2 rat2)
  = (MortgageFlow3 d1 (b1 + b2) (p1 + p2) (i1 + i2) (prep1 + prep2) (del13+del23) (del16+del26) (del19+del29) (def1 + def2) (rec1 + rec2) (los1+los2) (fromRational (weightedBy [b1,b2] (map toRational [rat1,rat2]))))

addTsCF :: TsRow -> TsRow -> TsRow
addTsCF (CashFlow d1 a1 ) (CashFlow _ a2 ) = (CashFlow d1 (a1 + a2))
addTsCF (BondFlow d1 b1 p1 i1 ) (BondFlow _ b2 p2 i2 ) = (BondFlow d1 (min b1 b2) (p1 + p2) (i1 + i2) )
addTsCF (MortgageFlow d1 b1 p1 i1 prep1 def1 rec1 los1 rat1) (MortgageFlow _ b2 p2 i2 prep2 def2 rec2 los2 rat2)
  = (MortgageFlow d1 (min b1 b2) (p1 + p2) (i1 + i2) (prep1 + prep2) (def1 + def2) (rec1 + rec2) (los1+los2) (fromRational (weightedBy [b1,b2] (map toRational [rat1,rat2]))) )
addTsCF (MortgageFlow2 d1 b1 p1 i1 prep1 del1 def1 rec1 los1 rat1) (MortgageFlow2 _ b2 p2 i2 prep2 del2 def2 rec2 los2 rat2)
  = (MortgageFlow2 d1 (min b1 b2) (p1 + p2) (i1 + i2) (prep1 + prep2) (del1 + del2) (def1 + def2) (rec1 + rec2) (los1+los2) (fromRational (weightedBy [b1,b2] (map toRational [rat1,rat2]) )))
addTsCF (MortgageFlow3 d1 b1 p1 i1 prep1 del13 del16 del19 def1 rec1 los1 rat1) (MortgageFlow3 _ b2 p2 i2 prep2 del23 del26 del29 def2 rec2 los2 rat2)
  = (MortgageFlow3 d1 (min b1 b2) (p1 + p2) (i1 + i2) (prep1 + prep2) (del13+del23) (del16+del26) (del19+del29) (def1 + def2) (rec1 + rec2) (los1+los2) (fromRational (weightedBy [b1,b2] (map toRational [rat1,rat2]))) )

sumTs :: [TsRow] -> Date -> TsRow
sumTs trs d = tsSetDate (foldr1 addTs trs) d

sumTsCF :: [TsRow] -> Date -> TsRow
sumTsCF trs d = tsSetDate (foldr1 addTsCF trs) d

tsDate :: TsRow -> Date
tsDate (CashFlow x _) = x
tsDate (BondFlow x  _ _ _) = x
tsDate (MortgageFlow x _ _ _ _ _ _ _ _) = x
tsDate (MortgageFlow2 x _ _ _ _ _ _ _ _ _) = x
tsDate (MortgageFlow3 x _ _ _ _ _ _ _ _ _ _ _) = x

tsTotalCash :: TsRow -> Centi
tsTotalCash (CashFlow _ x) = x
tsTotalCash (BondFlow _ _ a b) = a + b
tsTotalCash (MortgageFlow x _ _ a b c _ e _) = a + b + c + e
tsTotalCash (MortgageFlow2 x _ _ a b c _ _ e _) = a + b + c + e
tsTotalCash (MortgageFlow3 x _ _ a b c _ _ _ _ e _) = a + b + c + e

tsDefaultBal :: TsRow -> Centi
tsDefaultBal (CashFlow _ _) = 0
tsDefaultBal (BondFlow _ _ _ _) = 0
tsDefaultBal (MortgageFlow _ _ _ _ _ x _ _ _) = x
tsDefaultBal (MortgageFlow2 _ _ _ _ _ _ x _ _ _) = x
tsDefaultBal (MortgageFlow3 _ _ _ _ _ _ _ _ x _ _ _) = x


tsSetDate :: TsRow -> Date ->TsRow
tsSetDate (CashFlow _ a) x  = (CashFlow x a)
tsSetDate (BondFlow _ a b c) x = (BondFlow x a b c)
tsSetDate (MortgageFlow _ a b c d e f g h) x = (MortgageFlow x a b c d e f g h)
tsSetDate (MortgageFlow2 _ a b c d e f g h i) x = (MortgageFlow2 x a b c d e f g h i)
tsSetDate (MortgageFlow3 _ a b c d e f g h i j k) x = (MortgageFlow3 x a b c d e f g h i j k)

reduceTs :: [TsRow] -> TsRow -> [TsRow]
reduceTs [] _tr = [_tr]
reduceTs (tr:trs) _tr =
  if tr == _tr
  then (addTs tr _tr):trs
  else _tr:tr:trs

combine :: CashFlowFrame -> CashFlowFrame -> CashFlowFrame
combine (CashFlowFrame rs1) (CashFlowFrame rs2) =
    CashFlowFrame $  foldl reduceTs [] sorted_cff
    where cff = rs1++rs2
          sorted_cff = L.sort cff

tsDateLT :: Date -> TsRow  -> Bool
tsDateLT td (CashFlow d _) = d < td
tsDateLT td (BondFlow d _ _ _) =  d < td
tsDateLT td (MortgageFlow d _ _ _ _ _ _ _ _) = d < td
tsDateLT td (MortgageFlow2 d _ _ _ _ _ _ _ _ _) = d < td
tsDateLT td (MortgageFlow3 d _ _ _ _ _ _ _ _ _ _ _) = d < td

tsDateLET :: Date -> TsRow  -> Bool
tsDateLET td (CashFlow d _) = d <= td
tsDateLET td (BondFlow d _ _ _) =  d <= td
tsDateLET td (MortgageFlow d _ _ _ _ _ _ _ _) = d <= td
tsDateLET td (MortgageFlow2 d _ _ _ _ _ _ _ _ _) = d <= td
tsDateLET td (MortgageFlow3 d _ _ _ _ _ _ _ _ _ _ _) = d <= td

aggTsByDates :: [TsRow] -> [Date] -> [TsRow]
aggTsByDates trs ds =
  map (\(x,y) -> sumTsCF x y) (zip (reduceFn [] ds trs) ds)
  where
    reduceFn accum _ [] =  accum  -- `debug` ("Returning->"++show(accum))
    reduceFn accum (cutoffDay:[]) _trs =
      accum ++ [(filter (\x -> tsDate(x) <= cutoffDay) _trs)]
    reduceFn accum (cutoffDay:cutoffDays) _trs =
      case newAcc of
        [] -> reduceFn accum cutoffDays _trs
        newFlow -> reduceFn (accum++[newAcc]) cutoffDays rest --  `debug` ("Adding "++show(newAcc)++" cutoffDay "++show(cutoffDay))
      where
        (newAcc,rest) = L.partition (tsDateLET cutoffDay) _trs


mflowPrincipal :: TsRow -> Centi
mflowPrincipal (MortgageFlow _ _ x _ _ _ _ _ _) = x
mflowPrincipal (MortgageFlow2 _ _ x _ _ _ _ _ _ _) = x
mflowPrincipal (MortgageFlow3 _ _ x _ _ _ _ _ _ _ _ _) = x
mflowPrincipal _  = -1.0

mflowInterest :: TsRow -> Centi
mflowInterest (MortgageFlow _ _ _ x _ _ _ _ _) = x
mflowInterest (MortgageFlow2 _ _ _ x _ _ _ _ _ _) = x
mflowInterest (MortgageFlow3 _ _ _ x _ _ _ _ _ _ _ _) = x
mflowInterest _  = -1.0

mflowPrepayment :: TsRow -> Centi
mflowPrepayment (MortgageFlow _ _ _ _ x _ _ _ _) = x
mflowPrepayment (MortgageFlow2 _ _ _ _ x _ _ _ _ _) = x
mflowPrepayment (MortgageFlow3 _ _ _ _ x _ _ _ _ _ _ _) = x
mflowPrepayment _  = -1.0

mflowDefault :: TsRow -> Centi
mflowDefault (MortgageFlow _ _ _ _ _ x _ _ _) = x
mflowDefault (MortgageFlow2 _ _ _ _ _ _ x _ _ _) = x
mflowDefault (MortgageFlow3 _ _ _ _ _ _ _ _ x _ _ _) = x
mflowDefault _  = -1.0
mflowRecovery :: TsRow -> Centi
mflowRecovery (MortgageFlow _ _ _ _ _ _ x _ _) = x
mflowRecovery (MortgageFlow2 _ _ _ _ _ _ _ x _ _) = x
mflowRecovery (MortgageFlow3 _ _ _ _ _ _ _ _ _ x _ _) = x
mflowRecovery _  = -1.0

mflowBalance :: TsRow -> Centi -- getting end balance of period
mflowBalance (MortgageFlow _ x _ _ _ _ _ _ _) = x
mflowBalance (MortgageFlow2 _ x _ _ _ _ _ _ _ _) = x
mflowBalance (MortgageFlow3 _ x _ _ _ _ _ _ _ _ _ _) = x

mflowBegBalance :: TsRow -> Centi -- backout beg balance of period
mflowBegBalance (MortgageFlow _ x p _ ppy def _ _ _) = x + p + ppy + def
mflowBegBalance (MortgageFlow2 _ x p _ ppy _ def _ _ _) = x + p + ppy + def
mflowBegBalance (MortgageFlow3 _ x p _ ppy _ _ _ def _ _ _) = x + p + ppy + def

mflowLoss :: TsRow -> Centi
mflowLoss (MortgageFlow _ _ _ _ _ _ _ x _) = x
mflowLoss (MortgageFlow2 _ _ _ _ _ _ _ _ x _) = x
mflowLoss (MortgageFlow3 _ _ _ _ _ _ _ _ _ _ x _) = x

mflowRate :: TsRow -> IRate
mflowRate (MortgageFlow _ _ _ _ _ _ _ _ x) = x
mflowRate (MortgageFlow2 _ _ _ _ _ _ _ _ _ x) = x
mflowRate (MortgageFlow3 _ _ _ _ _ _ _ _ _ _ _ x) = x

mflowDate :: TsRow -> Date
mflowDate (MortgageFlow x _ _ _ _ _ _ _ _) = x
mflowDate (MortgageFlow2 x _ _ _ _ _ _ _ _ _) = x
mflowDate (MortgageFlow3 x _ _ _ _ _ _ _ _ _ _ _) = x


mflowWeightAverageBalance :: Date -> Date -> [TsRow] -> Balance
mflowWeightAverageBalance sd ed trs
  = sum $ zipWith mulBR _bals _dfs  -- `debug` ("CalcingAvgBal=>"++show sd++show ed++show txns  )
    where
     _dfs =  getIntervalFactors $ [sd]++_ds
     _bals = map mflowBegBalance txns
     _ds = map mflowDate txns
     txns = filter (\x -> (mflowDate x>=sd)&&(mflowDate x)<=ed) trs


$(deriveJSON defaultOptions ''TsRow)
$(deriveJSON defaultOptions ''CashFlowFrame)
