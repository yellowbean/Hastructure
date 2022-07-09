{-# LANGUAGE TemplateHaskell       #-}


module Cashflow (CashFlowFrame(..),Principals,Interests,Amount
                ,mkCashFlowFrame,mkColDay,mkColNum,mkColBal,combine
                ,sizeCashFlowFrame, aggTsByDates, getTsCashFlowFrame
                ,mflowInterest,mflowPrincipal,mflowRecovery,mflowPrepayment
                ,getSingleTsCashFlowFrame,removeTsCashFlowFrameByDate
                ,getEarlierTsCashFlowFrame
                ,mflowBalance,tsDefaultBal,getAllAfterCashFlowFrame
                ,getTxnAsOf,tsDateLT,tsDate,getTxnLatestAsOf
                ,TsRow(..),Balances) where

import Data.Time (Day)
import Lib (Dates)
import qualified Data.Map as Map
import qualified Data.Time as T
import qualified Data.List as L

import Data.Aeson hiding (json)
import Language.Haskell.TH
import Data.Aeson.TH
import Data.Aeson.Types

import Debug.Trace
debug = flip trace

type Interest = Float
type Principal = Float
type Balance = Float
type Amount = Float
type Prepayment = Float
type Recovery = Float 
type Default = Float 
type Rate = Float
type Date = T.Day

type Amounts = [Float]
type Balances = [Balance]
type Principals = [Principal]
type Interests = [Interest]
type Prepayments = [Prepayment]
type Recoveries = [Recovery]
type Rates = [Rate]


data ColType = ColNum Float | ColDate Date | ColBal Float
    deriving (Show)

data TsRow = CashFlow Date Amount
               |BondFlow Date Balance Principal Interest
              -- |FeeFlow Date Balance Amount
              -- |AccountFlow Date Balance Amount
              |MortgageFlow Date Balance Principal Interest Prepayment Default Recovery
              deriving (Show)

instance Ord TsRow where
  compare (CashFlow d1 _) (CashFlow d2 _) = compare d1 d2
  compare (BondFlow d1 _ _ _) (BondFlow d2 _ _ _) = compare d1 d2
  compare (MortgageFlow d1 _ _ _ _ _ _) (MortgageFlow d2 _ _ _ _ _ _) = compare d1 d2

instance Eq TsRow where
  (CashFlow d1 _) == (CashFlow d2 _) = d1 == d2
  (BondFlow d1 _ _ _) == (BondFlow d2 _ _ _) = d1 == d2
  (MortgageFlow d1 _ _ _ _ _ _) == (MortgageFlow d2 _ _ _ _ _ _) = d1 == d2

data CashFlowFrame = CashFlowFrame [TsRow]
              deriving (Show)
                -- |BondFrame [BondFlow]
                -- |MortgageFrame [MortgageFlow]

mkRow :: [ColType] -> TsRow
mkRow ((ColDate d):(ColBal b):(ColNum prin):(ColNum i):(ColNum pre):(ColBal def_b):(ColNum rec):[])
  = MortgageFlow d b prin i pre def_b rec

mkCashFlowFrame :: [[ColType]] -> CashFlowFrame
mkCashFlowFrame xss = CashFlowFrame $ map mkRow xss

sizeCashFlowFrame :: CashFlowFrame -> Int
sizeCashFlowFrame (CashFlowFrame ts) = length ts

getTsCashFlowFrame :: CashFlowFrame -> [TsRow]
getTsCashFlowFrame (CashFlowFrame ts) = ts

removeTsCashFlowFrameByDate :: CashFlowFrame -> T.Day -> (Maybe CashFlowFrame)
removeTsCashFlowFrameByDate (CashFlowFrame trs) d =
  let
    r = filter (\x -> (tsDate x) /= d) trs
  in
    if (length r)==0 then
      Nothing
    else
      Just (CashFlowFrame r)

getSingleTsCashFlowFrame :: CashFlowFrame -> T.Day -> TsRow
getSingleTsCashFlowFrame (CashFlowFrame trs) d
  = head $ filter (\x -> (tsDate x) == d) trs

getEarlierTsCashFlowFrame :: CashFlowFrame -> T.Day -> Maybe TsRow
getEarlierTsCashFlowFrame (CashFlowFrame trs) d
  = L.find (tsDateLT d) (reverse trs)

getAllAfterCashFlowFrame ::CashFlowFrame -> T.Day -> CashFlowFrame
getAllAfterCashFlowFrame cf@(CashFlowFrame trx) d
  = CashFlowFrame (getTxnAfter cf d)

getTxnAsOf :: CashFlowFrame -> T.Day -> [TsRow]
getTxnAsOf (CashFlowFrame txn) d = filter (\x -> (tsDate x) <= d) txn

getTxnAfter :: CashFlowFrame -> T.Day -> [TsRow]
getTxnAfter (CashFlowFrame txn) d = filter (\x -> (tsDate x) > d) txn

getTxnLatestAsOf :: CashFlowFrame -> T.Day -> Maybe TsRow
getTxnLatestAsOf (CashFlowFrame txn) d = L.find (\x -> (tsDate x) <= d) $ reverse txn

mkColDay :: [T.Day] -> [ColType]
mkColDay ds = [ (ColDate _d) | _d <- ds ]

mkColNum :: [Float] -> [ColType]
mkColNum ds = [ (ColNum _d) | _d <- ds ]

mkColBal :: [Float] -> [ColType]
mkColBal ds = [ (ColBal _d) | _d <- ds ]

addTs :: TsRow -> TsRow -> TsRow
addTs (CashFlow d1 a1 ) (CashFlow _ a2 ) = (CashFlow d1 (a1 + a2))
addTs (BondFlow d1 b1 p1 i1 ) (BondFlow _ b2 p2 i2 ) = (BondFlow d1 (b1 + b2) (p1 + p2) (i1 + i2) )
addTs (MortgageFlow d1 b1 p1 i1 prep1 def1 rec1 ) (MortgageFlow _ b2 p2 i2 prep2 def2 rec2 )
  = (MortgageFlow d1 (b1 + b2) (p1 + p2) (i1 + i2) (prep1 + prep2) (def1 + def2) (rec1 + rec2))

addTsCF :: TsRow -> TsRow -> TsRow
addTsCF (CashFlow d1 a1 ) (CashFlow _ a2 ) = (CashFlow d1 (a1 + a2))
addTsCF (BondFlow d1 b1 p1 i1 ) (BondFlow _ b2 p2 i2 ) = (BondFlow d1 (min b1 b2) (p1 + p2) (i1 + i2) )
addTsCF (MortgageFlow d1 b1 p1 i1 prep1 def1 rec1 ) (MortgageFlow _ b2 p2 i2 prep2 def2 rec2 )
  = (MortgageFlow d1 (min b1 b2) (p1 + p2) (i1 + i2) (prep1 + prep2) (def1 + def2) (rec1 + rec2))

sumTs :: [TsRow] -> T.Day -> TsRow
sumTs trs d = tsSetDate (foldr1 addTs trs) d

sumTsCF :: [TsRow] -> T.Day -> TsRow
sumTsCF trs d = tsSetDate (foldr1 addTsCF trs) d

tsDate :: TsRow -> T.Day
tsDate (CashFlow x _) = x
tsDate (BondFlow x  _ _ _) = x
tsDate (MortgageFlow x _ _ _ _ _ _) = x

tsDefaultBal :: TsRow -> Float
tsDefaultBal (CashFlow _ _) = 0
tsDefaultBal (BondFlow _ _ _ _) = 0
tsDefaultBal (MortgageFlow _ _ _ _ _ x _) = x


tsSetDate :: TsRow -> T.Day ->TsRow
tsSetDate (CashFlow _ a) x  = (CashFlow x a)
tsSetDate (BondFlow _ a b c) x = (BondFlow x a b c)
tsSetDate (MortgageFlow _ a b c d e f ) x = (MortgageFlow x a b c d e f)

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

tsDateLT :: T.Day -> TsRow  -> Bool
tsDateLT td (CashFlow d _) = d < td
tsDateLT td (BondFlow d _ _ _) =  d < td
tsDateLT td (MortgageFlow d _ _ _ _ _ _) = d < td

tsDateLET :: T.Day -> TsRow  -> Bool
tsDateLET td (CashFlow d _) = d <= td
tsDateLET td (BondFlow d _ _ _) =  d <= td
tsDateLET td (MortgageFlow d _ _ _ _ _ _) = d <= td


--splitTsRowByDates :: [TsRow] -> [T.Day] -> [TsRow] -> [TsRow]
--splitTsRowByDates accum _ [] = accum
--splitTsRowByDates accum ([]:day) ys = accum ++ (filter  (\x-> tsDate(x) <= day) ys)
--splitTsRowByDates accum (day:days) ys
--  = splitTsRowByDates (accum++ _xs ) days _ys
--     where
--       (_xs,_ys) = L.partition (tsDateLET day) ys

aggTsByDates :: [TsRow] -> [T.Day] -> [TsRow]
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


mflowPrincipal :: TsRow -> Float
mflowPrincipal (MortgageFlow _ _ x _ _ _ _) = x
mflowPrincipal _  = -1.0
mflowInterest :: TsRow -> Float
mflowInterest (MortgageFlow _ _ _ x _ _ _) = x
mflowInterest _  = -1.0
mflowPrepayment :: TsRow -> Float
mflowPrepayment (MortgageFlow _ _ _ _ x _ _) = x
mflowPrepayment _  = -1.0
mflowDefault :: TsRow -> Float
mflowDefault (MortgageFlow _ _ _ _ _ x _) = x
mflowDefault _  = -1.0
mflowRecovery :: TsRow -> Float
mflowRecovery (MortgageFlow _ _ _ _ _ _ x) = x
mflowRecovery _  = -1.0
mflowBalance :: TsRow -> Float
mflowBalance (MortgageFlow _ x _ _ _ _ _) = x


$(deriveJSON defaultOptions ''TsRow)
$(deriveJSON defaultOptions ''CashFlowFrame)
