module Cashflow (CashFlowFrame(..),Principals,Interests,Amount
                ,mkCashFlowFrame,mkColDay,mkColNum,mkColBal,combine
                ,sizeCashFlowFrame, aggTsByDates, getTsCashFlowFrame
                ,mflowInterest,mflowPrincipal,mflowRecovery,mflowPrepayment
                ,getSingleTsCashFlowFrame,removeTsCashFlowFrameByDate
                ,TsRow(..) ) where

import Data.Time (Day)
import Lib (Dates)
-- import Data.Dates (Date)
import Debug.Trace
-- import Data.Currency (Alpha)
import qualified Data.Map as Map
import qualified Data.Time as T
import qualified Data.List as L

type Interest = Float
type Principal = Float
type Balance = Float
type Amount = Float
type Prepayment = Float
type Recovery = Float
type Date = T.Day

type Amounts = [Float]
type Principals = [Principal]
type Interests = [Interest]
type Prepayments = [Prepayment]
type Recoveries = [Recovery]

data ColType = ColNum Float | ColDate Date | ColBal Float
    deriving (Show)

data TsRow = CashFlow Date Amount
              |BondFlow Date Balance Principal Interest
              |MortgageFlow Date Balance Principal Interest Prepayment Recovery
              deriving (Show)


instance Ord TsRow where
  compare (CashFlow d1 _) (CashFlow d2 _) = compare d1 d2
  compare (BondFlow d1 _ _ _) (BondFlow d2 _ _ _) = compare d1 d2
  compare (MortgageFlow d1 _ _ _ _ _) (MortgageFlow d2 _ _ _ _ _) = compare d1 d2

instance Eq TsRow where
  (CashFlow d1 _) == (CashFlow d2 _) = d1 == d2
  (BondFlow d1 _ _ _) == (BondFlow d2 _ _ _) = d1 == d2
  (MortgageFlow d1 _ _ _ _ _) == (MortgageFlow d2 _ _ _ _ _) = d1 == d2

data CashFlowFrame = CashFlowFrame [TsRow]
              deriving (Show)
                -- |BondFrame [BondFlow]
                -- |MortgageFrame [MortgageFlow]

mkRow :: [ColType] -> TsRow
mkRow ((ColDate d):(ColBal b):(ColNum prin):(ColNum i):(ColNum pre):(ColNum rec):[])
  = MortgageFlow d b prin i pre rec

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

mkColDay :: [T.Day] -> [ColType]
mkColDay ds = [ (ColDate _d) | _d <- ds ]

mkColNum :: [Float] -> [ColType]
mkColNum ds = [ (ColNum _d) | _d <- ds ]

mkColBal :: [Float] -> [ColType]
mkColBal ds = [ (ColBal _d) | _d <- ds ]

addTs :: TsRow -> TsRow -> TsRow
addTs (CashFlow d1 a1 ) (CashFlow _ a2 ) = (CashFlow d1 (a1 + a2))
addTs (BondFlow d1 b1 p1 i1 ) (BondFlow _ b2 p2 i2 ) = (BondFlow d1 (b1 + b2) (p1 + p2) (i1 + i2) )
addTs (MortgageFlow d1 b1 p1 i1 prep1 rec1 ) (MortgageFlow _ b2 p2 i2 prep2 rec2 )
  = (MortgageFlow d1 (b1 + b2) (p1 + p2) (i1 + i2) (prep1 + prep2) (rec1 + rec2))

sumTs :: [TsRow] -> T.Day -> TsRow
sumTs trs d = tsSetDate (foldr1 addTs trs) d

tsDate :: TsRow -> T.Day
tsDate (CashFlow x _) = x
tsDate (BondFlow x  _ _ _) = x
tsDate (MortgageFlow x _ _ _ _ _) = x

tsSetDate :: TsRow -> T.Day ->TsRow
tsSetDate (CashFlow _ a) x  = (CashFlow x a)
tsSetDate (BondFlow _ a b c) x = (BondFlow x a b c)
tsSetDate (MortgageFlow _ a b c d e) x = (MortgageFlow x a b c d e)

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
tsDateLT td (MortgageFlow d _ _ _ _ _) = d < td


aggTsByDates :: [TsRow] -> [T.Day] -> [TsRow]
aggTsByDates trs ds =
  map (\(x,y) -> sumTs x y) 
      (zip (reduceFn [] ds trs) (trace ("ds size"++show(length(ds))) ds))
  where
    reduceFn accum _ [] =  reverse accum
    reduceFn ([]:accum) (cfd:cfds) _trs =  reduceFn accum  cfds _trs
    reduceFn accum (cutoffDay:cutoffDays) _trs = 
      reduceFn (newAcc:accum) cutoffDays rest
        where
          (newAcc,rest) = L.partition (tsDateLT cutoffDay) _trs
    reduceFn accum _ _ = reverse accum


mflowPrincipal :: TsRow -> Float
mflowPrincipal (MortgageFlow _ _ x _ _ _) = x
mflowPrincipal _  = -1.0
mflowInterest :: TsRow -> Float
mflowInterest (MortgageFlow _ _ _ x _ _) = x
mflowInterest _  = -1.0
mflowPrepayment :: TsRow -> Float
mflowPrepayment (MortgageFlow _ _ _ _ x _) = x
mflowPrepayment _  = -1.0
mflowRecovery :: TsRow -> Float
mflowRecovery (MortgageFlow _ _ _ _ _ x) = x
mflowRecovery _  = -1.0


