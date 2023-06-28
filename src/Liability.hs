{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Liability
  (Bond(..),BondType(..),OriginalInfo(..),SinkFundSchedule(..)
  ,payInt,payPrin,consolTxn,consolStmt,backoutDueIntByYield
  ,priceBond,PriceResult(..),pv,InterestInfo(..),RateReset(..)
  ,weightAverageBalance,fv2,calcZspread,payYield
  ,buildRateResetDates)
  where

import Language.Haskell.TH
import Data.Aeson       hiding (json)
import Data.Aeson.TH
import Data.Fixed

import qualified Data.Time as T
import Lib (Period(..),Ts(..) ,TsPoint(..)
           ,toDate,daysBetween ,getIntervalFactors,daysBetweenI)

import Util
import Types
import Data.Ratio 

import qualified Stmt as S 

import Data.List (findIndex,zip6,find)
import qualified Cashflow as CF

import GHC.Generics

import Debug.Trace
debug = flip trace

type RateReset = DatePattern 
type StepUpDates = DatePattern 


data InterestInfo = Floater Index Spread RateReset DayCount (Maybe Floor) (Maybe Cap)
                  | Fix IRate DayCount 
                  | StepUpFix IRate DayCount StepUpDates Spread
                  | InterestByYield IRate
                  deriving (Show, Eq, Generic)

data OriginalInfo = OriginalInfo {
  originBalance::Balance
  ,originDate::Date
  ,originRate::Rate
  ,maturityDate :: Maybe Date
} deriving (Show, Eq, Generic)

type SinkFundSchedule = Ts
type PlannedAmorSchedule = Ts

data BondType = Sequential
              | SinkFund SinkFundSchedule
              | PAC PlannedAmorSchedule
              | PAC_Anchor PlannedAmorSchedule [String]
              | Lockout Date
              | Z
              | Equity
              deriving (Show, Eq, Generic)

data Bond = Bond {
  bndName :: String
  ,bndType :: BondType
  ,bndOriginInfo :: OriginalInfo
  ,bndInterestInfo :: InterestInfo
  ,bndBalance :: Balance
  ,bndRate :: IRate
  ,bndDuePrin :: Balance
  ,bndDueInt :: Balance
  ,bndDueIntDate :: Maybe Date
  ,bndLastIntPay :: Maybe Date
  ,bndLastPrinPay :: Maybe Date
  ,bndStmt :: Maybe S.Statement
} deriving (Show, Eq, Generic)

consolTxn :: [S.Txn] -> S.Txn -> [S.Txn]
consolTxn (txn:txns) txn0
  = if txn==txn0 then 
      (S.combineTxn txn txn0):txns
    else
      txn0:txn:txns 
consolTxn [] txn = [txn]

consolStmt :: Bond -> Bond
consolStmt b@Bond{bndStmt = Just (S.Statement (txn:txns))}
  =  b {bndStmt = Just (S.Statement (reverse (foldl consolTxn [txn] txns)))} -- `debug` ("Consoling stmt for "++ show (bndName b))

consolStmt b@Bond{bndStmt = Nothing} =  b {bndStmt = Nothing}

payInt :: Date -> Amount -> Bond -> Bond
payInt d amt bnd@(Bond bn Equity oi iinfo bal r duePrin dueInt dueIntDate lpayInt lpayPrin stmt)
  = bnd { bndDueInt=new_due, bndStmt = Just new_stmt}
  where
    new_due = dueInt - amt
    new_stmt = S.appendStmt stmt (S.BondTxn d bal amt 0 r amt (S.PayYield bn))

payInt d amt bnd@(Bond bn bt oi iinfo bal r duePrin dueInt dueIntDate lpayInt lpayPrin stmt)
  = bnd {bndDueInt=new_due, bndStmt=Just new_stmt, bndLastIntPay = Just d}
  where
    new_due = dueInt - amt -- `debug` (">>pay INT to "++ show bn ++ ">>" ++ show amt)
    new_stmt = S.appendStmt stmt (S.BondTxn d bal amt 0 r amt (S.PayInt [bn]))

payYield :: Date -> Amount -> Bond -> Bond 
payYield d amt bnd@(Bond bn bt oi iinfo bal r duePrin dueInt dueIntDate lpayInt lpayPrin stmt)
  = bnd {bndStmt=Just new_stmt}
  where
    new_stmt = S.appendStmt stmt (S.BondTxn d bal amt 0 r amt (S.PayYield bn))

payPrin :: Date -> Amount -> Bond -> Bond
payPrin d amt bnd@(Bond bn bt oi iinfo bal r duePrin dueInt dueIntDate lpayInt lpayPrin stmt)
  = bnd {bndDuePrin =new_due, bndBalance = new_bal , bndStmt=Just new_stmt}
  where
    new_bal = bal - amt
    new_due = duePrin - amt
    new_stmt = S.appendStmt stmt (S.BondTxn d new_bal 0 amt 0 amt (S.PayPrin [bn] ))

pv :: Ts -> Date -> Date -> Amount -> Amount
pv pc today d amt = 
   realToFrac $ (realToFrac amt) * (1 / factor) -- `debug` ("DF:"++show factor++" PV AMT"++show amt)
  where
   distance::Double =  fromIntegral $ daysBetween today d
   discount_rate = fromRational $ getValByDate pc Exc d -- `debug` ("Get val by ts"++show pc ++">>d"++ show d)
   factor::Double = (1 + realToFrac discount_rate) ** (distance / 365)  -- `debug` ("discount_rate"++show(discount_rate) ++" dist days=>"++show(distance))

fv2 :: IRate -> Date -> Date -> Amount -> Amount
fv2 discount_rate today futureDay amt =
    -- mulBI (realToFrac amt) factor 
    realToFrac $ (realToFrac amt) * factor 
    --mulBI amt ((1+discount_rate) ** (distance / 365))
  where
    factor::Double = (1 + realToFrac discount_rate) ** (distance / 365)
    distance::Double = fromIntegral $ daysBetween today futureDay

priceBond :: Date -> Ts -> Bond -> PriceResult
priceBond d rc b@(Bond _ _ (OriginalInfo obal od _ _) _ bal cr _ _ _ lastIntPayDay _ (Just (S.Statement txns)))
  = PriceResult
     presentValue
     (fromRational (100*(toRational presentValue)/(toRational obal)))
     (realToFrac wal)
     (realToFrac duration)
     (realToFrac convexity)
     accruedInt -- `debug` ("Convexity->"++ show convexity)
     where
       futureCf = filter (\x -> (S.getDate x) > d) txns
       presentValue = foldr (\x acc -> acc + (pv rc d (S.getDate x) (S.getTxnAmt x))) 0 futureCf
       cutoffBalance = case (S.getTxnAsOf txns d) of
                          Nothing ->  (S.getTxnBalance fstTxn) + (S.getTxnPrincipal fstTxn) --  `debug` (show(getTxnBalance fstTxn))
                                     where
                                      fstTxn = head txns
                          Just _txn -> S.getTxnBalance _txn  -- `debug` ("presentValue"++show presentValue)
       accruedInt = case _t of
                      Nothing -> (fromIntegral (max 0 (T.diffDays d leftPayDay))/365) * (mulBI leftBal cr)
                      Just _ -> 0  -- `debug` ("all txn"++show(_t))-- `debug` ("l day, right"++show(leftPayDay)++show(d)++show(T.diffDays leftPayDay d))
                    where
                      _t = find (\x -> (S.getDate x) == d) txns
                      leftTxns = takeWhile (\txn -> (S.getDate txn) < d) txns
                      (leftPayDay,leftBal) =
                        case leftTxns of
                          [] -> case lastIntPayDay of
                                 Nothing ->  (od,bal)
                                 Just _d -> (_d,bal)
                          _ -> let
                                leftTxn = last leftTxns
                              in
                                (S.getDate leftTxn,S.getTxnBalance leftTxn)
       wal =  ((foldr 
                 (\x acc ->
                   (acc + ((fromIntegral (daysBetween d (S.getDate x)))*(S.getTxnPrincipal x)/365)))
                   0.0
                   futureCf) / cutoffBalance)  
       duration = (foldr (\x acc ->
                           (mulBR  
                             ((pv rc d (S.getDate x) (S.getTxnAmt x)) / presentValue) 
                             (yearCountFraction DC_ACT_365F d (S.getDate x)))
                           + acc)
                    0
                    futureCf)  -- `debug` ("WAL-->"++show wal) 
       convexity = let 
                     b = (foldr (\x acc ->
                                         let 
                                            _t = yearCountFraction DC_ACT_365F d (S.getDate x) -- `debug` ("calc _T"++show d++">>"++show (S.getTxnDate x))
                                            _t2 = _t * _t + _t -- `debug` ("T->"++show _t)
                                            _cash_date = S.getDate x
                                            _yield = getValByDate rc Exc _cash_date
                                            _y = (1+ _yield) * (1+ _yield) -- `debug` ("yield->"++ show _yield++"By date"++show d)
                                            _x = ((mulBR  (pv rc d _cash_date (S.getTxnAmt x)) _t2) / (fromRational _y)) -- `debug` ("PV:->"++show (pv rc d (S.getTxnDate x) (S.getTxnAmt x))++"Y->"++ show _y++"T2-->"++ show _t2)
                                         in 
                                            _x + acc) 
                                 0
                                 futureCf)
                   in 
                     b/presentValue -- `debug` ("Duration->"++show duration) -- `debug` ("B->"++show b++"PV"++show presentValue)

priceBond d rc b@(Bond _ _ _ _ _ _ _ _ _ _ _ Nothing ) = PriceResult 0 0 0 0 0 0


_calcIRR :: Balance -> IRR -> Date -> Ts -> IRR
_calcIRR amt initIrr today (BalanceCurve cashflows)
   = if ((abs(diff) < 0.005) || (abs(nextIrr-initIrr)<0.0001)) then
       initIrr
     else
       _calcIRR amt nextIrr today (BalanceCurve cashflows)  -- `debug` ("NextIRR -> "++show(nextIrr))
     where
       discount (TsPoint _d _a) _r =  (toRational _a) / ((1+_r)^(div (fromIntegral (T.diffDays _d today)) 365))
       pv = foldr (\_ts acc -> (discount _ts initIrr) + acc) 0 cashflows -- `debug` ("")
       diff = pv - (toRational amt)  -- `debug` ("pv->"++show(pv))
       nextIrr = if diff > 0 then
                   initIrr * 1.01
                 else
                   initIrr * 0.99

calcBondYield :: Date -> Balance ->  Bond -> Rate
calcBondYield _ _ (Bond _ _ _ _ _ _ _ _ _ _ _ Nothing) = 0
calcBondYield d cost b@(Bond _ _ _ _ _ _ _ _ _ _ _ (Just (S.Statement txns)))
 =  _calcIRR cost 0.05 d (BalanceCurve cashflows)
   where
     cashflows = [ TsPoint (S.getDate txn) (S.getTxnAmt txn)  | txn <- txns ]



backoutDueIntByYield :: Date -> Bond -> Balance
backoutDueIntByYield d b@(Bond _ _ (OriginalInfo obal odate _ _) (InterestByYield y) currentBalance  _ _ _ _ _ _ stmt)
  -- = obal_fv - fvs `debug` ("FVS->"++show fvs++"FV of Obal"++show (fv2 y odate d obal)++"y"++show y++"odate"++show odate++"d"++ show d++"obal"++show obal)
  = proj_fv - fvs - currentBalance -- `debug` ("Date"++ show d ++"FV->"++show proj_fv++">>"++show fvs++">>cb"++show currentBalance)
    where
     proj_fv = fv2 y odate d obal 
     fvs = sum $ [ fv2 y d (fst cf) (snd cf)  | cf <- cashflows ]
     cashflows = case stmt of
                   Just (S.Statement txns) -> [ ((S.getDate txn),(S.getTxnAmt txn))  | txn <- txns ]
                   Nothing -> []

weightAverageBalance :: Date -> Date -> Bond -> Balance
weightAverageBalance sd ed b@(Bond _ _ _ _ currentBalance _ _ _ _ _ _ stmt)
  = sum $ zipWith mulBR _bals _dfs -- `debug` ("dfs"++show(sd)++show(ed)++show(_ds)++show(_bals)++show(_dfs))  -- `debug` (">> stmt"++show(sliceStmt (bndStmt _b) sd ed))
    where
     _dfs =  getIntervalFactors $ [sd]++ _ds ++ [ed]
     _bals = [currentBalance] ++ map S.getTxnBegBalance txns -- `debug` ("txn"++show(txns))
     _ds = S.getDates txns -- `debug` ("Slice"++show((sliceStmt (bndStmt _b) sd ed)))
     _b = consolStmt b   
     txns =  case S.sliceStmt (bndStmt _b) sd ed of
                Nothing -> []
                Just (S.Statement _txns) -> _txns-- map getTxnBalance _txns

calcZspread :: (Balance,Date) -> Int -> (Balance,Rational) -> Maybe S.Statement -> Ts -> Spread
calcZspread _ _ _ Nothing _ = 0

calcZspread (tradePrice,priceDay) count (trialPrice,spd) (Just (S.Statement txns)) riskFreeCurve  
  | count >= 10000 = fromRational spd
  | otherwise =
    let 
      (_,futureTxns) = splitByDate txns priceDay EqToRight
      cashflow = map S.getTxnAmt futureTxns
      ds = map S.getDate futureTxns
      pvCurve = shiftTsByAmt riskFreeCurve spd
      pvs = [ pv pvCurve priceDay _d _amt | (_d, _amt) <- zip ds cashflow ]
      newPrice = sum pvs -- `debug` ("PVS->>"++ show pvs)
      newSpd_ = if newPrice > tradePrice then 
                 if spd>0 then 
                    spd * 1.005
                 else
                    spd * 0.995
               else 
                 if spd>0 then 
                    spd * 0.995
                 else
                    spd * 1.005

      newSpd = if newSpd_ <= 0.00001 && newSpd_>0 then 
                 newSpd_ * (-1)
               else
                 newSpd_
    in 
      if abs(newPrice - tradePrice) <= 0.0001 then 
         fromRational spd -- `debug` ("trail price"++show (fromRational spd))
      else
        calcZspread (tradePrice,priceDay) (succ count) (newPrice, newSpd) (Just (S.Statement txns)) riskFreeCurve -- `debug` ("Run with newprice"++show newPrice++"Count->"++show count ) 

buildRateResetDates :: Bond -> StartDate -> EndDate -> [Date]
buildRateResetDates b sd ed 
 = case bndInterestInfo b of 
     (StepUpFix _ _ dp _ ) -> genSerialDatesTill2 EE sd dp ed
     (Floater _ _ dp _ _ _) -> genSerialDatesTill2 EE sd dp ed
     _ -> []


$(deriveJSON defaultOptions ''InterestInfo)
$(deriveJSON defaultOptions ''OriginalInfo)
$(deriveJSON defaultOptions ''BondType)
$(deriveJSON defaultOptions ''Bond)
