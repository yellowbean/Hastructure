{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DeriveGeneric       #-}

module Cashflow (CashFlowFrame(..),Principals,Interests,Amount
                ,combine,mergePoolCf,sumTsCF,tsSetDate,tsSetLoss,tsSetRecovery
                ,sizeCashFlowFrame,aggTsByDates, getTsCashFlowFrame
                ,mflowInterest,mflowPrincipal,mflowRecovery,mflowPrepayment
                ,mflowRental,mflowRate,sumPoolFlow,splitTrs,aggregateTsByDate
                ,mflowDefault,mflowLoss,mflowDate
                ,getSingleTsCashFlowFrame,getDatesCashFlowFrame,getDateRangeCashFlowFrame
                ,lookupSource,reduceTs,combineTss
                ,mflowBalance,mflowBegBalance,tsDefaultBal
                ,mflowBorrowerNum,mflowPrepaymentPenalty
                ,splitCashFlowFrameByDate,emptyTsRow,mflowAmortAmount
                ,tsTotalCash, setPrepaymentPenalty, setPrepaymentPenaltyFlow
                ,tsDateLT,getDate,getTxnLatestAsOf
                ,mflowWeightAverageBalance,appendCashFlow,combineCashFlow
                ,addFlowBalance,totalLoss,totalDefault,totalRecovery,firstDate
                ,shiftCfToStartDate,cfInsertHead,buildBegTsRow,insertBegTsRow
                ,tsCumDefaultBal,tsCumDelinqBal,tsCumLossBal,tsCumRecoveriesBal
                ,TsRow(..),cfAt,cutoffTrs,patchBeginBalance,patchCumulative,extendTxns,dropTailEmptyTxns) where

import Data.Time (Day)
import Data.Fixed
import Lib (weightedBy,toDate,getIntervalFactors,daysBetween)
import Util (mulBR,mulBInt,mulIR)
import DateUtil ( splitByDate )
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
import qualified Control.Lens as Map
import Data.OpenApi (HasPatch(patch))
import Data.Text.Internal.Encoding.Fusion (streamUtf16BE)
debug = flip trace

type Delinquent = Centi
type Amounts = [Float]
type Principals = [Principal]
type Interests = [Interest]
type Prepayments = [Prepayment]
type Recoveries = [Recovery]
type Rates = [Rate]

type CumulativeStat = (CumPrincipal,CumPrepay,CumDelinq,CumDefault,CumRecovery,CumLoss)

opStats :: (Balance -> Balance -> Balance) -> Maybe CumulativeStat -> Maybe CumulativeStat -> Maybe CumulativeStat
opStats op (Just (a1,b1,c1,d1,e1,f1)) (Just (a2,b2,c3,d2,e2,f2)) = Just (op a1 a2,op b1 b2,op c1 c3,op d1 d2,op e1 e2,op f1 f2)
opStats op Nothing Nothing = Nothing
opStats op (Just a) Nothing = Just a
opStats op Nothing (Just a) = Just a

sumStats :: Maybe CumulativeStat -> Maybe CumulativeStat -> Maybe CumulativeStat
-- sumStats (a1,b1,c1,d1,e1,f1) (a2,b2,c3,d2,e2,f2) = (a1+a2,b1+b2,c1+c3,d1+d2,e1+e2,f1+f2)
sumStats s1 s2 = opStats (+) s1 s2

subStats :: Maybe CumulativeStat -> Maybe CumulativeStat -> Maybe CumulativeStat
-- subStats (a1,b1,c1,d1,e1,f1) (a2,b2,c3,d2,e2,f2) = (a1-a2,b1-b2,c1-c3,d1-d2,e1-e2,f1-f2)
subStats s1 s2 = opStats (-) s1 s2

maxStats :: Maybe CumulativeStat -> Maybe CumulativeStat -> Maybe CumulativeStat
-- maxStats (a1,b1,c1,d1,e1,f1) (a2,b2,c3,d2,e2,f2) = (max a1 a2,max b1 b2,max c1 c3,max d1 d2,max e1 e2,max f1 f2)
maxStats s1 s2 = opStats max s1 s2

splitStats :: Rational -> CumulativeStat -> CumulativeStat
splitStats r st1@(a1,b1,c1,d1,e1,f1) = ((`mulBR` r) a1,(`mulBR` r) b1,(`mulBR` r) c1,(`mulBR` r) d1,(`mulBR` r) e1,(`mulBR` r) f1)

type Depreciation = Balance
type NewDepreciation = Balance 

data TsRow = CashFlow Date Amount
           | BondFlow Date Balance Principal Interest
           | MortgageFlow Date Balance Principal Interest Prepayment Default Recovery Loss IRate (Maybe BorrowerNum) (Maybe PrepaymentPenalty) (Maybe CumulativeStat)
           | MortgageDelinqFlow Date Balance Principal Interest Prepayment Delinquent Default Recovery Loss IRate (Maybe BorrowerNum) (Maybe PrepaymentPenalty) (Maybe CumulativeStat)
           | LoanFlow Date Balance Principal Interest Prepayment Default Recovery Loss IRate (Maybe CumulativeStat)
           | LeaseFlow Date Balance Rental
           | FixedFlow Date Balance NewDepreciation Depreciation Balance Amount (Maybe Balance) (Maybe Balance)
                -- remain balance, amortized amount, unit, cash , mIncome, mExp
           deriving(Show,Eq,Ord,Generic)

instance TimeSeries TsRow where 
    getDate (CashFlow x _) = x
    getDate (BondFlow x  _ _ _) = x
    getDate (MortgageFlow x _ _ _ _ _ _ _ _ _ _ _) = x
    getDate (MortgageDelinqFlow x _ _ _ _ _ _ _ _ _ _ _ _) = x
    getDate (LoanFlow x _ _ _ _ _ _ _ _ _) = x
    getDate (LeaseFlow x _ _ ) = x
    getDate (FixedFlow x _ _ _ _ _ _ _) = x

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
cfInsertHead tr (CashFlowFrame trs) = CashFlowFrame $ tr:trs

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

addTs :: TsRow -> TsRow -> TsRow     
-- ^ left cashflow is ealier ,right one is later,combine both and yield cashflow with earlier date
addTs (CashFlow d1 a1 ) (CashFlow _ a2 ) = CashFlow d1 (a1 + a2)
addTs (BondFlow d1 b1 p1 i1 ) tr@(BondFlow _ b2 p2 i2 ) = BondFlow d1 (b1 - mflowAmortAmount tr) (p1 + p2) (i1 + i2)
addTs (MortgageFlow d1 b1 p1 i1 prep1 def1 rec1 los1 rat1 mbn1 pn1 st1) tr@(MortgageFlow _ b2 p2 i2 prep2 def2 rec2 los2 rat2 mbn2 pn2 st2)
  = let 
      bn = (+) <$> mbn1 <*> mbn2
      p =  (+) <$> pn1 <*> pn2
      st = subStats st1 st2
    in 
      MortgageFlow d1 (b1 - mflowAmortAmount tr) (p1 + p2) (i1 + i2) (prep1 + prep2) (def1 + def2) (rec1 + rec2) (los1+los2) (fromRational (weightedBy [b1,b2] (toRational <$> [rat1,rat2]))) bn p st
addTs (MortgageDelinqFlow d1 b1 p1 i1 prep1 delinq1 def1 rec1 los1 rat1 mbn1 pn1 st1) tr@(MortgageDelinqFlow _ b2 p2 i2 prep2 delinq2 def2 rec2 los2 rat2 mbn2 pn2 st2)
  = let 
      bn = (+) <$> mbn1 <*> mbn2
      p =  (+) <$> pn1 <*> pn2
      delinq = (+) delinq1 delinq2
      st = subStats st1 st2
    in 
      MortgageDelinqFlow d1 (b1 - mflowAmortAmount tr) (p1 + p2) (i1 + i2) (prep1 + prep2) delinq (def1 + def2) (rec1 + rec2) (los1+los2) (fromRational (weightedBy [b1,b2] (toRational <$> [rat1,rat2]))) bn p st

addTs (LoanFlow d1 b1 p1 i1 prep1 def1 rec1 los1 rat1 st1) tr@(LoanFlow _ b2 p2 i2 prep2 def2 rec2 los2 rat2 st2)
  = LoanFlow d1 (b1 - mflowAmortAmount tr) (p1 + p2) (i1 + i2) (prep1 + prep2) (def1 + def2) (rec1 + rec2) (los1+los2) (fromRational (weightedBy [b1,b2] (toRational <$> [rat1,rat2]))) (subStats st1 st2)

addTs (LeaseFlow d1 b1 r1) tr@(LeaseFlow d2 b2 r2) 
  = LeaseFlow d1 (b1 - mflowAmortAmount tr) (r1 + r2)

combineTs :: TsRow -> TsRow -> TsRow     

-- ^ combine two cashflow records from two entities, return cashflow with earlier date
combineTs (CashFlow d1 a1 ) (CashFlow _ a2 ) = CashFlow d1 (a1 + a2)

combineTs (BondFlow d1 b1 p1 i1 ) tr@(BondFlow _ b2 p2 i2 ) = BondFlow d1 (b1 + b2) (p1 + p2) (i1 + i2)

combineTs (MortgageDelinqFlow d1 b1 p1 i1 prep1 delinq1 def1 rec1 los1 rat1 mbn1 pn1 st1) tr@(MortgageDelinqFlow _ b2 p2 i2 prep2 delinq2 def2 rec2 los2 rat2 mbn2 pn2 st2)
  = let 
      bn = (+) <$> mbn1 <*> mbn2
      p =  (+) <$> pn1 <*> pn2
      delinq = (+) delinq1 delinq2
      st = sumStats st1 st2
    in 
      MortgageDelinqFlow d1 (b1 + b2) (p1 + p2) (i1 + i2) (prep1 + prep2) delinq (def1 + def2) (rec1 + rec2) (los1+los2) (fromRational (weightedBy [b1,b2] (toRational <$> [rat1,rat2]))) bn p st

combineTs (MortgageFlow d1 b1 p1 i1 prep1 def1 rec1 los1 rat1 mbn1 pn1 st1) tr@(MortgageFlow _ b2 p2 i2 prep2 def2 rec2 los2 rat2 mbn2 pn2 st2)
  = let 
      bn = (+) <$> mbn1 <*> mbn2
      p =  (+) <$> pn1 <*> pn2
      st = sumStats st1 st2
    in 
      MortgageFlow d1 (b1 + b2) (p1 + p2) (i1 + i2) (prep1 + prep2) (def1 + def2) (rec1 + rec2) (los1+los2) (fromRational (weightedBy [b1,b2] (toRational <$> [rat1,rat2]))) bn p st

combineTs (LoanFlow d1 b1 p1 i1 prep1 def1 rec1 los1 rat1 st1) tr@(LoanFlow _ b2 p2 i2 prep2 def2 rec2 los2 rat2 st2)
  = LoanFlow d1 (b1 + b2) (p1 + p2) (i1 + i2) (prep1 + prep2) (def1 + def2) (rec1 + rec2) (los1+los2) (fromRational (weightedBy [b1,b2] (toRational <$> [rat1,rat2]))) (sumStats st1 st2)

combineTs (LeaseFlow d1 b1 r1) tr@(LeaseFlow d2 b2 r2) 
  = LeaseFlow d1 (b1 + b2) (r1 + r2)

combineTss :: [TsRow] -> [TsRow] -> [TsRow] -> [TsRow]
-- ^ combine two cashflows from two entities,(auto patch a beg balance)
combineTss [] [] r = r
combineTss [] r [] = r
combineTss [] (r1:r1s) (r2:r2s) 
  | getDate r1 > getDate r2 = combineTss [] (r2:r2s) (r1:r1s)
  | getDate r1 == getDate r2 = combineTss [combineTs r1 r2] 
                                         r1s
                                         r2s
  | otherwise = combineTss [updateFlowBalance (mflowBegBalance r2+mflowBalance r1) r1]
                           r1s
                           (r2:r2s)
combineTss consols [] [] = reverse consols
combineTss (consol:consols) (r:rs) [] = combineTss ((appendTs consol r):consol:consols) rs []
combineTss (consol:consols) [] (tr:trs) = combineTss ((appendTs consol tr):consol:consols) [] trs
combineTss (consol:consols) (r:rs) (tr:trs)
  | getDate r == getDate tr = combineTss ((appendTs consol (combineTs r tr)):consol:consols) rs trs
  | getDate r < getDate tr = combineTss ((appendTs consol r):consol:consols) rs (tr:trs)
  | getDate r > getDate tr = combineTss ((appendTs consol tr):consol:consols) (r:rs) trs 
combineTss a b c = error $ "combineTss not supported "++show a++" "++show b++" "++show c


appendTs :: TsRow -> TsRow -> TsRow 
-- ^ combine two cashflow records from two entities ,(early row on left, later row on right)
appendTs bn1@(BondFlow d1 b1 _ _ ) bn2@(BondFlow d2 b2 p2 i2 ) 
  = updateFlowBalance (b1 - mflowAmortAmount bn2) bn2 -- `debug` ("b1 >> "++show b1++">>"++show (mflowAmortAmount bn2))
appendTs (MortgageDelinqFlow d1 b1 p1 i1 prep1 _ def1 rec1 los1 rat1 mbn1 _ _) bn2@(MortgageDelinqFlow _ b2 p2 i2 prep2 _ def2 rec2 los2 rat2 mbn2 _ _)
  = updateFlowBalance (b1 - mflowAmortAmount bn2) bn2
appendTs (MortgageFlow d1 b1 p1 i1 prep1 def1 rec1 los1 rat1 mbn1 _ _) bn2@(MortgageFlow _ b2 p2 i2 prep2 def2 rec2 los2 rat2 mbn2 _ _)
  = updateFlowBalance (b1 - mflowAmortAmount bn2) bn2
appendTs (LoanFlow d1 b1 p1 i1 prep1 def1 rec1 los1 rat1 _) bn2@(LoanFlow _ b2 p2 i2 prep2 def2 rec2 los2 rat2 _)
  = updateFlowBalance (b1 - mflowAmortAmount bn2) bn2
appendTs (LeaseFlow d1 b1 r1) bn2@(LeaseFlow d2 b2 r2) 
  = updateFlowBalance (b1 - mflowAmortAmount bn2) bn2
appendTs _1 _2 = error $ "appendTs failed with "++ show _1 ++ ">>" ++ show _2

addTsCF :: TsRow -> TsRow -> TsRow
-- ^ add up TsRow from same entity
addTsCF (CashFlow d1 a1 ) (CashFlow _ a2 ) = CashFlow d1 (a1 + a2)
addTsCF (BondFlow d1 b1 p1 i1 ) (BondFlow _ b2 p2 i2 ) = BondFlow d1 (min b1 b2) (p1 + p2) (i1 + i2)
addTsCF (MortgageFlow d1 b1 p1 i1 prep1 def1 rec1 los1 rat1 mbn1 pn1 st1) (MortgageFlow d2 b2 p2 i2 prep2 def2 rec2 los2 rat2 mbn2 pn2 st2)
  = let 
      bn = min <$> mbn1 <*> mbn2
      p =  (+) <$> pn1 <*> pn2
      st = maxStats st1 st2
    in 
      MortgageFlow d1 (min b1 b2) (p1 + p2) (i1 + i2) (prep1 + prep2) (def1 + def2) (rec1 + rec2) (los1+los2) (fromRational (weightedBy [b1,b2] (toRational <$> [rat1,rat2]))) bn p st
addTsCF (MortgageDelinqFlow d1 b1 p1 i1 prep1 delinq1 def1 rec1 los1 rat1 mbn1 pn1 st1) (MortgageDelinqFlow d2 b2 p2 i2 prep2 delinq2 def2 rec2 los2 rat2 mbn2 pn2 st2)
  = let 
      bn = min <$> mbn1 <*> mbn2
      p =  (+) <$> pn1 <*> pn2
      delinq = (+) delinq1 delinq2
      st = maxStats st1 st2
    in 
      MortgageDelinqFlow d1 (min b1 b2) (p1 + p2) (i1 + i2) (prep1 + prep2) delinq (def1 + def2) (rec1 + rec2) (los1+los2) (fromRational (weightedBy [b1,b2] (toRational <$> [rat1,rat2]))) bn p st
addTsCF (LoanFlow d1 b1 p1 i1 prep1 def1 rec1 los1 rat1 st1) (LoanFlow _ b2 p2 i2 prep2 def2 rec2 los2 rat2 st2)
  = LoanFlow d1 (min b1 b2) (p1 + p2) (i1 + i2) (prep1 + prep2) (def1 + def2) (rec1 + rec2) (los1+los2) (fromRational (weightedBy [b1,b2] (toRational <$> [rat1,rat2]))) (maxStats st1 st2)
addTsCF (LeaseFlow d1 b1 r1) (LeaseFlow d2 b2 r2) = LeaseFlow d1 (min b1 b2) (r1 + r2)

sumTs :: [TsRow] -> Date -> TsRow
sumTs trs d = tsSetDate (foldr1 addTs trs) d


sumTsCF :: [TsRow] -> Date -> TsRow
-- ^ group cashflow from same entity by a single date
sumTsCF trs d = tsSetDate (foldl1 addTsCF trs) d -- `debug` ("Summing"++show trs++">>"++ show (tsSetDate (foldr1 addTsCF trs) d))

tsTotalCash :: TsRow -> Balance
tsTotalCash (CashFlow _ x) = x
tsTotalCash (BondFlow _ _ a b) = a + b
tsTotalCash (MortgageDelinqFlow x _ a b c _ _ e _ _ _ mPn _ ) = a + b + c + e + fromMaybe 0 mPn
tsTotalCash (MortgageFlow x _ a b c _ e _ _ _ mPn _) = a + b + c + e + fromMaybe 0 mPn
tsTotalCash (LoanFlow _ _ a b c _ e _ _ _) =  a + b + c + e
tsTotalCash (LeaseFlow _ _ a) =  a
tsTotalCash (FixedFlow _ _ _ _ _ x mI mE) = x + fromMaybe 0 mI - fromMaybe 0 mE

tsDefaultBal :: TsRow -> Balance
tsDefaultBal CashFlow {} = error "not supported"
tsDefaultBal BondFlow {} = error "not supported"
tsDefaultBal (MortgageDelinqFlow _ _ _ _ _ _ x _ _ _ _ _ _) = x
tsDefaultBal (MortgageFlow _ _ _ _ _ x _ _ _ _ _ _) = x
tsDefaultBal (LoanFlow _ _ _ _ _ x _ _ _ _) = x
tsDefaultBal LeaseFlow {} = error "not supported"

tsCumDefaultBal :: TsRow -> Balance
tsCumDefaultBal (MortgageDelinqFlow _ _ _ _ _ _ _ _ _ _ _ _ (Just (a,b,c,d,e,f))) = d
tsCumDefaultBal (MortgageDelinqFlow _ _ _ _ _ _ _ _ _ _ _ _ Nothing) = 0.0
tsCumDefaultBal (MortgageFlow _ _ _ _ _ _ _ _ _ _ _ (Just (a,b,c,d,e,f))) = d
tsCumDefaultBal (MortgageFlow _ _ _ _ _ _ _ _ _ _ _ Nothing) = 0.0
tsCumDefaultBal (LoanFlow _ _ _ _ _ _ _ _ _ (Just (a,b,c,d,e,f))) = d
tsCumDefaultBal (LoanFlow _ _ _ _ _ _ _ _ _  Nothing ) = 0.0
tsCumDefaultBal x = error ("Failed to get cumulative default for record " ++ show x)

tsCumDelinqBal :: TsRow -> Balance
tsCumDelinqBal (MortgageDelinqFlow _ _ _ _ _ _ _ _ _ _ _ _ (Just (a,b,c,d,e,f))) = c
tsCumDelinqBal (MortgageDelinqFlow _ _ _ _ _ _ _ _ _ _ _ _ Nothing ) = 0.0
tsCumDelinqBal (MortgageFlow _ _ _ _ _ _ _ _ _ _ _ (Just (a,b,c,d,e,f))) = c
tsCumDelinqBal (MortgageFlow _ _ _ _ _ _ _ _ _ _ _ Nothing ) = 0.0
tsCumDelinqBal (LoanFlow _ _ _ _ _ _ _ _ _ (Just (a,b,c,d,e,f))) = c
tsCumDelinqBal (LoanFlow _ _ _ _ _ _ _ _ _ Nothing) = 0.0
tsCumDelinqBal x = error ("Failed to get cumulative delinq for record " ++ show x)

tsCumLossBal :: TsRow -> Balance
tsCumLossBal (MortgageDelinqFlow _ _ _ _ _ _ _ _ _ _ _ _ (Just (a,b,c,d,e,f))) = f
tsCumLossBal (MortgageDelinqFlow _ _ _ _ _ _ _ _ _ _ _ _ Nothing) = 0.0
tsCumLossBal (MortgageFlow _ _ _ _ _ _ _ _ _ _ _ (Just (a,b,c,d,e,f))) = f
tsCumLossBal (MortgageFlow _ _ _ _ _ _ _ _ _ _ _ Nothing) = 0.0
tsCumLossBal (LoanFlow _ _ _ _ _ _ _ _ _ (Just (a,b,c,d,e,f))) = f
tsCumLossBal (LoanFlow _ _ _ _ _ _ _ _ _ Nothing) = 0.0 
tsCumLossBal x = error ("Failed to get cumulative loss for record " ++ show x)

tsCumRecoveriesBal :: TsRow -> Balance
tsCumRecoveriesBal (MortgageDelinqFlow _ _ _ _ _ _ _ _ _ _ _ _ (Just (a,b,c,d,e,f))) = e
tsCumRecoveriesBal (MortgageDelinqFlow _ _ _ _ _ _ _ _ _ _ _ _ Nothing ) = 0.0
tsCumRecoveriesBal (MortgageFlow _ _ _ _ _ _ _ _ _ _ _ (Just (a,b,c,d,e,f))) = e
tsCumRecoveriesBal (MortgageFlow _ _ _ _ _ _ _ _ _ _ _ Nothing) = 0.0
tsCumRecoveriesBal (LoanFlow _ _ _ _ _ _ _ _ _ (Just (a,b,c,d,e,f))) = e
tsCumRecoveriesBal (LoanFlow _ _ _ _ _ _ _ _ _ Nothing) = 0.0
tsCumRecoveriesBal x = error ("Failed to get cumulative loss for record " ++ show x)


tsSetDate :: TsRow -> Date -> TsRow
tsSetDate (CashFlow _ a) x  = CashFlow x a
tsSetDate (BondFlow _ a b c) x = BondFlow x a b c
tsSetDate (MortgageDelinqFlow _ a b c d e f g h i j k l) x = MortgageDelinqFlow x a b c d e f g h i j k l
tsSetDate (MortgageFlow _ a b c d e f g h i j k) x = MortgageFlow x a b c d e f g h i j k
tsSetDate (LoanFlow _ a b c d e f g h i) x = LoanFlow x a b c d e f g h i
tsSetDate (LeaseFlow _ a b) x = LeaseFlow x a b

tsSetBalance :: Balance -> TsRow -> TsRow
tsSetBalance x (CashFlow _d a) = CashFlow _d x
tsSetBalance x (BondFlow _d a b c) = BondFlow _d x b c
tsSetBalance x (MortgageDelinqFlow _d a b c d e f g h i j k l) = MortgageDelinqFlow _d x b c d e f g h i j k l
tsSetBalance x (MortgageFlow _d a b c d e f g h i j k) = MortgageFlow _d x b c d e f g h i j k 
tsSetBalance x (LoanFlow _d a b c d e f g h i) = LoanFlow _d x b c d e f g h i
tsSetBalance x (LeaseFlow _d a b) = LeaseFlow _d x b

tsSetLoss :: Balance -> TsRow -> TsRow
tsSetLoss x (MortgageDelinqFlow _d a b c d e f g h i j k l) = MortgageDelinqFlow _d a b c d e f g x i j k l
tsSetLoss x (MortgageFlow _d a b c d e f g h i j k) = MortgageFlow _d a b c d e f x h i j k 
tsSetLoss x (LoanFlow _d a b c d e f g h i) = LoanFlow _d a b c d e f x h i
tsSetLoss x _ = error $ "Failed to set Loss for "++show x

tsSetRecovery :: Balance -> TsRow -> TsRow
tsSetRecovery x (MortgageDelinqFlow _d a b c d e f g h i j k l) = MortgageDelinqFlow _d a b c d e f x h i j k l
tsSetRecovery x (MortgageFlow _d a b c d e f g h i j k) = MortgageFlow _d a b c d e x g h i j k 
tsSetRecovery x (LoanFlow _d a b c d e f g h i) = LoanFlow _d a b c d e x g h i
tsSetRecovery x _ = error $ "Failed to set Recovery for "++show x



tsOffsetDate :: Integer -> TsRow -> TsRow
tsOffsetDate x (CashFlow _d a) = CashFlow (T.addDays x _d) a
tsOffsetDate x (BondFlow _d a b c) = BondFlow (T.addDays x _d) a b c
tsOffsetDate x (MortgageDelinqFlow _d a b c d e f g h i j k l) = MortgageDelinqFlow (T.addDays x _d) a b c d e f g h i j k l
tsOffsetDate x (MortgageFlow _d a b c d e f g h i j k) = MortgageFlow (T.addDays x _d) a b c d e f g h i j k
tsOffsetDate x (LoanFlow _d a b c d e f g h i) = LoanFlow (T.addDays x _d) a b c d e f g h i
tsOffsetDate x (LeaseFlow _d a b) = LeaseFlow (T.addDays x _d) a b


-- ^ consolidate cashflow from same entity , update balance of newer cashflow record
reduceTs :: [TsRow] -> TsRow -> [TsRow]
reduceTs [] _tr = [_tr]
reduceTs (tr:trs) _tr 
  | sameDate tr _tr = addTs tr _tr : trs 
  | otherwise = appendTs tr _tr : tr : trs 

aggregateTsByDate :: [TsRow] -> [TsRow] -> [TsRow]
aggregateTsByDate rs [] = reverse rs
aggregateTsByDate [] (tr:trs) = aggregateTsByDate [tr] trs
aggregateTsByDate (r:rs) (tr:trs) 
  | sameDate r tr = aggregateTsByDate ((combineTs r tr):rs) trs
  | otherwise = aggregateTsByDate (tr:r:rs) trs


firstDate :: CashFlowFrame -> Date 
firstDate (CashFlowFrame []) = error "empty cashflow frame to get first date"
firstDate (CashFlowFrame rs) = getDate $ head rs

combine :: CashFlowFrame -> CashFlowFrame -> CashFlowFrame 
combine (CashFlowFrame txn1) (CashFlowFrame txn2) = CashFlowFrame $ combineTss [] txn1 txn2

tsDateLT :: Date -> TsRow  -> Bool
tsDateLT td (CashFlow d _) = d < td
tsDateLT td (BondFlow d _ _ _) =  d < td
tsDateLT td (MortgageDelinqFlow d _ _ _ _ _ _ _ _ _ _ _ _) = d < td
tsDateLT td (MortgageFlow d _ _ _ _ _ _ _ _ _ _ _) = d < td
tsDateLT td (LoanFlow d _ _ _ _ _ _ _ _ _ ) = d < td
tsDateLT td (LeaseFlow d _ _ ) = d < td

tsDateLET :: Date -> TsRow  -> Bool
tsDateLET td (CashFlow d _) = d <= td
tsDateLET td (BondFlow d _ _ _) =  d <= td
tsDateLET td (MortgageDelinqFlow d _ _ _ _ _ _ _ _ _ _ _ _) = d <= td
tsDateLET td (MortgageFlow d _ _ _ _ _ _ _ _ _ _ _) = d <= td
tsDateLET td (LoanFlow d _ _ _ _ _ _ _ _ _) = d <= td
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
mflowPrincipal (MortgageFlow _ _ x _ _ _ _ _ _ _ _ _) = x
mflowPrincipal (MortgageDelinqFlow _ _ x _ _ _ _ _ _ _ _ _ _) = x
mflowPrincipal (LoanFlow _ _ x _ _ _ _ _ _ _) = x
mflowPrincipal _  = error "not supported"

mflowInterest :: TsRow -> Balance
mflowInterest (MortgageDelinqFlow _ _ _ x _ _ _ _ _ _ _ _ _) = x
mflowInterest (MortgageFlow _ _ _ x _ _ _ _ _ _ _ _) = x
mflowInterest (LoanFlow _ _ _ x _ _ _ _ _ _) = x
mflowInterest _  = error "not supported"

mflowPrepayment :: TsRow -> Balance
mflowPrepayment (MortgageFlow _ _ _ _ x _ _ _ _ _ _ _) = x
mflowPrepayment (MortgageDelinqFlow _ _ _ _ x _ _ _ _ _ _ _ _) = x
mflowPrepayment (LoanFlow _ _ _ _ x _ _ _ _ _) = x
mflowPrepayment _  = error "not supported"

mflowDefault :: TsRow -> Balance
mflowDefault (MortgageFlow _ _ _ _ _ x _ _ _ _ _ _) = x
mflowDefault (MortgageDelinqFlow _ _ _ _ _ _ x _ _ _ _ _ _) = x
mflowDefault (LoanFlow _ _ _ _ _ x _ _ _ _) = x
mflowDefault _  = 0

mflowRecovery :: TsRow -> Balance
mflowRecovery (MortgageFlow _ _ _ _ _ _ x _ _ _ _ _) = x
mflowRecovery (MortgageDelinqFlow _ _ _ _ _ _ _ x _ _ _ _ _) = x
mflowRecovery (LoanFlow _ _ _ _ _ _ x _ _ _) = x
mflowRecovery _  = error "not supported"

mflowBalance :: TsRow -> Balance
mflowBalance (MortgageFlow _ x _ _ _ _ _ _ _ _ _ _) = x
mflowBalance (MortgageDelinqFlow _ x _ _ _ _ _ _ _ _ _ _ _) = x
mflowBalance (LoanFlow _ x _ _ _ _ _ _ _ _) = x
mflowBalance (LeaseFlow _ x _ ) = x

addFlowBalance :: Balance -> TsRow -> TsRow 
addFlowBalance 0 x = x
addFlowBalance b (MortgageFlow a x c d e f g h i j k l) = MortgageFlow a (x+b) c d e f g h i j k l
addFlowBalance b (MortgageDelinqFlow a x c d e f g h i j k l m) = MortgageDelinqFlow a (x+b) c d e f g h i j k l m
addFlowBalance b (LoanFlow a x c d e f g i j k) = LoanFlow a (x+b) c d e f g i j k
addFlowBalance b (LeaseFlow a x c ) = LeaseFlow a (x+b) c

updateFlowBalance :: Balance -> TsRow -> TsRow 
updateFlowBalance b (MortgageDelinqFlow a x c d e f g h i j k l m ) = MortgageDelinqFlow a b c d e f g h i j k l m
updateFlowBalance b (MortgageFlow a x c d e f g h i j k l) = MortgageFlow a b c d e f g h i j k l
updateFlowBalance b (LoanFlow a x c d e f g i j k) = LoanFlow a b c d e f g i j k
updateFlowBalance b (LeaseFlow a x c ) = LeaseFlow a b c

mflowBegBalance :: TsRow -> Balance
mflowBegBalance (MortgageDelinqFlow _ x p _ ppy delinq def _ _ _ _ _ _) = x + p + ppy + delinq
mflowBegBalance (MortgageFlow _ x p _ ppy def _ _ _ _ _ _) = x + p + ppy + def
mflowBegBalance (LoanFlow _ x p _ ppy def _ _ _ _) = x + p + ppy + def
mflowBegBalance (LeaseFlow _ b r) = b + r

mflowLoss :: TsRow -> Balance
mflowLoss (MortgageFlow _ _ _ _ _ _ _ x _ _ _ _) = x
mflowLoss (MortgageDelinqFlow _ _ _ _ _ _ _ _ x _ _ _ _) = x
mflowLoss (LoanFlow _ _ _ _ _ _ _ x _ _) = x
mflowLoss _ = 0

mflowDelinq :: TsRow -> Balance
mflowDelinq (MortgageDelinqFlow _ _ _ _ _ x _ _ _ _ _ _ _) = x
mflowDelinq _ = 0

mflowRate :: TsRow -> IRate
-- ^ get rate(weigthed avg rate) for a cashflow record
mflowRate (MortgageFlow _ _ _ _ _ _ _ _ x _ _ _) = x
mflowRate (MortgageDelinqFlow _ _ _ _ _ _ _ _ _ x _ _ _) = x
mflowRate (LoanFlow _ _ _ _ _ _ _ _ x _) = x

mflowRental :: TsRow -> Amount
mflowRental (LeaseFlow _ _ x ) = x

mflowDate :: TsRow -> Date
-- ^ get date for a cashflow record
mflowDate (MortgageFlow x _ _ _ _ _ _ _ _ _ _ _) = x
mflowDate (MortgageDelinqFlow x _ _ _ _ _ _ _ _ _ _ _ _) = x
mflowDate (LoanFlow x _ _ _ _ _ _ _ _ _) = x
mflowDate (LeaseFlow x _ _ ) = x

mflowAmortAmount :: TsRow -> Balance
-- ^ calculate amortized amount for cashflow (for defaults only)
mflowAmortAmount (MortgageFlow _ _ p _ ppy def _ _ _ _ _ _) = p + ppy + def
mflowAmortAmount (MortgageDelinqFlow _ _ p _ ppy delinq _ _ _ _ _ _ _) = p + ppy + delinq
mflowAmortAmount (LoanFlow _ _ x _ y z _ _ _ _) = x + y + z
mflowAmortAmount (LeaseFlow _ _ x ) = x

mflowBorrowerNum :: TsRow -> Maybe BorrowerNum
-- ^ get borrower numfer for Mortgage Flow
mflowBorrowerNum (MortgageFlow _ _ _ _ _ _ _ _ _ x _ _) = x
mflowBorrowerNum (MortgageDelinqFlow _ _ _ _ _ _ _ _ _ _ x _ _) = x
mflowBorrowerNum _ = undefined

mflowPrepaymentPenalty :: TsRow -> Balance
-- ^ get prepayment penalty for a cashflow record
mflowPrepaymentPenalty (MortgageFlow _ _ _ _ _ _ _ _ _ _ (Just x) _) = x
mflowPrepaymentPenalty (MortgageFlow _ _ _ _ _ _ _ _ _ _ Nothing _) = 0
mflowPrepaymentPenalty (MortgageDelinqFlow _ _ _ _ _ _ _ _ _ _ _ (Just x) _) = x
mflowPrepaymentPenalty (MortgageDelinqFlow _ _ _ _ _ _ _ _ _ _ _ Nothing _) = 0
mflowPrepaymentPenalty _ = undefined

mflowWeightAverageBalance :: Date -> Date -> [TsRow] -> Balance
mflowWeightAverageBalance sd ed trs
  = sum $ zipWith mulBR _bals _dfs  -- `debug` ("CalcingAvgBal=>"++show sd++show ed++show txns  )
    where
     txns = filter (\x -> (mflowDate x>=sd)&&(mflowDate x)<=ed) trs
     _ds = map mflowDate txns -- `debug` ("fee base txns"++show txns)
     _bals = map mflowBegBalance txns
     _dfs =  getIntervalFactors $ sd:_ds

appendCashFlow :: CashFlowFrame -> [TsRow] -> CashFlowFrame
-- ^ append cashflows to a cashflow frame
appendCashFlow (CashFlowFrame _tsr) tsr 
  = CashFlowFrame $ _tsr ++ tsr

emptyTsRow :: Date -> TsRow -> TsRow 
-- ^ reset all cashflow fields to zero and init with a date
emptyTsRow _d (MortgageDelinqFlow a x c d e f g h i j k l m) = MortgageDelinqFlow _d 0 0 0 0 0 0 0 0 0 Nothing Nothing Nothing
emptyTsRow _d (MortgageFlow a x c d e f g h i j k l) = MortgageFlow _d 0 0 0 0 0 0 0 0 Nothing Nothing Nothing
emptyTsRow _d (LoanFlow a x c d e f g i j k) = LoanFlow _d 0 0 0 0 0 0 0 0 Nothing
emptyTsRow _d (LeaseFlow a x c ) = LeaseFlow _d 0 0

buildBegTsRow :: Date -> TsRow -> TsRow
-- ^ given a cashflow,build a new cf row with begin balance
buildBegTsRow d tr 
  = tsSetBalance (mflowBalance tr + mflowAmortAmount tr) (emptyTsRow d tr)

insertBegTsRow :: Date -> CashFlowFrame -> CashFlowFrame
insertBegTsRow d (CashFlowFrame []) = CashFlowFrame []
insertBegTsRow d (CashFlowFrame (txn:txns))
  = let
      begRow = buildBegTsRow d txn
    in 
      CashFlowFrame (begRow:txn:txns)

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
-- ^ merge two cashflow frame but no patching beg balance
mergePoolCf cf (CashFlowFrame []) = cf
mergePoolCf (CashFlowFrame []) cf = cf
mergePoolCf cf1@(CashFlowFrame txns1) cf2@(CashFlowFrame txns2) -- first day of left is earlier than right one
  | startDate1 > startDate2 = mergePoolCf cf2 cf1 
  | otherwise 
      = let 
          splitDate = firstDate cf2  -- (ls,rs) = splitByDate txns d st
          (txn0,txnToMerged) = splitByDate txns1 splitDate EqToRight
          txn1 = combineTss [] txnToMerged txns2 -- `debug` ("left"++show cfToBeMerged++">> right"++ show cf2)
        in 
          CashFlowFrame (txn0++txn1) -- `debug` ("Txn1"++show txn1)
  where 
    [startDate1,startDate2] =  firstDate <$> [cf1,cf2]
    -- rightToLeft = startDate1 >= startDate2

shiftCfToStartDate :: Date -> CashFlowFrame -> CashFlowFrame
shiftCfToStartDate d cf@(CashFlowFrame (txn:txns))
  = let 
      fstDate = firstDate cf 
      diffDays = daysBetween fstDate d
    in 
      CashFlowFrame $ tsOffsetDate diffDays <$> (txn:txns)

sumPoolFlow :: CashFlowFrame -> PoolSource -> Balance
sumPoolFlow (CashFlowFrame trs) ps 
  = sum $ lookup ps <$> trs
    where
      lookup CollectedPrepayment  = mflowPrepayment
      lookup CollectedPrincipal = mflowPrincipal
      lookup CollectedRecoveries = mflowRecovery
      lookup CollectedRental = mflowRental
      lookup CollectedInterest = mflowInterest
      lookup NewDefaults = mflowDefault
      lookup NewLosses = mflowLoss
      lookup NewDelinquencies = mflowDelinq

lookupSource :: TsRow -> PoolSource -> Balance 
lookupSource tr CollectedPrepayment  = mflowPrepayment tr
lookupSource tr CollectedPrincipal = mflowPrincipal tr
lookupSource tr CollectedRecoveries = mflowRecovery tr
lookupSource tr CollectedRental = mflowRental tr
lookupSource tr CollectedInterest = mflowInterest tr
lookupSource tr CollectedPrepaymentPenalty = mflowPrepaymentPenalty tr
lookupSource tr NewDelinquencies = mflowDelinq tr
lookupSource tr NewDefaults = mflowDefault tr
lookupSource tr NewLosses = mflowLoss tr

setPrepaymentPenalty :: Balance -> TsRow -> TsRow
setPrepaymentPenalty bal (MortgageDelinqFlow a b c d e f g h i j k l m) = MortgageDelinqFlow a b c d e f g h i j k (Just bal) m
setPrepaymentPenalty bal (MortgageFlow b c d e f g h i j k l m) = MortgageFlow b c d e f g h i j k (Just bal) m
setPrepaymentPenalty _ _ = error "prepay pental only applies to MortgageFlow"

setPrepaymentPenaltyFlow :: [Balance] -> [TsRow] -> [TsRow]
setPrepaymentPenaltyFlow bals trs = [ setPrepaymentPenalty bal tr | (bal,tr) <- zip bals trs]

splitTs :: Rate -> TsRow -> TsRow 
splitTs r (MortgageDelinqFlow d bal p i ppy delinq def recovery loss rate mB mPPN mStat)
  = MortgageDelinqFlow d (mulBR bal r) (mulBR p r) (mulBR i r) (mulBR ppy r)
                       (mulBR delinq r) (mulBR def r) (mulBR recovery r) (mulBR loss r)
                       rate ((\x -> round (toRational x * r)) <$> mB) ((`mulBR` r) <$> mPPN)
                       (splitStats r <$> mStat)
splitTs r (MortgageFlow d bal p i ppy def recovery loss rate mB mPPN mStat)
  = MortgageFlow d (mulBR bal r) (mulBR p r) (mulBR i r) (mulBR ppy r)
                       (mulBR def r) (mulBR recovery r) (mulBR loss r)
                       rate ((\x -> round (toRational x * r)) <$> mB) ((`mulBR` r) <$> mPPN)
                       (splitStats r <$> mStat)
splitTs _ tr = error $ "Not support for spliting TsRow"++show tr

splitTrs :: Rate -> [TsRow] -> [TsRow]
splitTrs r trs = splitTs r <$> trs 

-- type CumulativeStat = (CumPrincipal,CumPrepay,CumDelinq,CumDefault,CumRecovery,CumLoss)
patchCumulative :: CumulativeStat -> [TsRow] -> [TsRow] -> [TsRow]
patchCumulative _ rs [] = reverse rs
patchCumulative (cPrin,cPrepay,cDelinq,cDefault,cRecovery,cLoss)
                rs
                ((MortgageDelinqFlow d bal p i ppy delinq def recovery loss rate mB mPPN _):trs)
  = patchCumulative newSt
                    ((MortgageDelinqFlow d bal p i ppy delinq def recovery loss rate mB mPPN (Just newSt)):rs)
                    trs
                 where 
                   newSt = (cPrin+p,cPrepay+ppy,cDelinq+delinq,cDefault+def,cRecovery+recovery,cLoss+loss)
patchCumulative (cPrin,cPrepay,cDelinq,cDefault,cRecovery,cLoss)
               rs
               ((MortgageFlow d bal p i ppy def recovery loss rate mB mPPN _):trs)
  = patchCumulative newSt
                   ((MortgageFlow d bal p i ppy def recovery loss rate mB mPPN (Just newSt)):rs)
                   trs
                where 
                  newSt = (cPrin+p,cPrepay+ppy,cDelinq,cDefault+def,cRecovery+recovery,cLoss+loss)
patchCumulative (cPrin,cPrepay,cDelinq,cDefault,cRecovery,cLoss)
              rs
              ((LoanFlow d bal p i ppy def recovery loss rate _):trs)
  = patchCumulative newSt
                  ((LoanFlow d bal p i ppy def recovery loss rate (Just newSt)):rs)
                  trs
               where 
                 newSt = (cPrin+p,cPrepay+ppy,cDelinq,cDefault+def,cRecovery+recovery,cLoss+loss)

patchCumulative a b c = error ("faile to patch cumulative stats for "++show a ++">>"++show b++">>"++show c)



-- ^ split cashflow by rate while build missing defaults/losses stats
cutoffTrs :: Date -> [TsRow] -> ([TsRow],Map.Map CutoffFields Balance)
cutoffTrs d [] = ([],Map.empty)
cutoffTrs d trs 
  = let 
      beforeTrs = cutBy Exc Past d trs
      cumuDefaults = sum $ mflowDefault <$> beforeTrs 
      cumuDelinquency = sum $ mflowDelinq <$> beforeTrs  
      cumuLoss = sum $ mflowLoss <$> beforeTrs 
      m = Map.fromList [(HistoryDefaults,cumuDefaults),(HistoryDelinquency,cumuDelinquency),(HistoryLoss,cumuLoss)]
      
      afterTrs  = cutBy Inc Future d trs
    in
      (patchCumulative (0.0,0.0,0.0,0.0,0.0,0.0) [] afterTrs, m)

patchBeginBalance :: Date -> CashFlowFrame -> CashFlowFrame
patchBeginBalance _ (CashFlowFrame []) = CashFlowFrame []
patchBeginBalance d cf@(CashFlowFrame txns) 
  = let 
      begRow = buildBegTsRow d (head txns)
    in 
      CashFlowFrame (begRow:txns)

extendTxns :: TsRow -> [Date] -> [TsRow]      
extendTxns tr ds = [ emptyTsRow d tr | d <- ds ]

isEmptyRow :: TsRow -> Bool 
isEmptyRow (MortgageDelinqFlow _ 0 0 0 0 0 0 0 0 _ _ _ _) = True
isEmptyRow (MortgageDelinqFlow {}) = False
isEmptyRow (MortgageFlow _ 0 0 0 0 0 0 0 _ _ _ _) = True
isEmptyRow (MortgageFlow {}) = False
isEmptyRow (LoanFlow _ 0 0 0 0 0 0 0 i j ) = True
isEmptyRow (LoanFlow {}) = False
isEmptyRow (LeaseFlow _ 0 0) = True
isEmptyRow (LeaseFlow {}) = False
isEmptyRow (FixedFlow _ 0 0 0 0 0 Nothing Nothing) = True
isEmptyRow (FixedFlow _ 0 0 0 0 0 (Just 0) Nothing) = True
isEmptyRow (FixedFlow _ 0 0 0 0 0 (Just 0) (Just 0)) = True
isEmptyRow (FixedFlow _ 0 0 0 0 0 Nothing (Just 0)) = True
isEmptyRow (FixedFlow {}) = False


dropTailEmptyTxns :: [TsRow] -> [TsRow]
dropTailEmptyTxns trs 
  = reverse $ dropWhile isEmptyRow (reverse trs)

$(deriveJSON defaultOptions ''TsRow)
$(deriveJSON defaultOptions ''CashFlowFrame)
