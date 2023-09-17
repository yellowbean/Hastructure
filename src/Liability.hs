{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Liability
  (Bond(..),BondType(..),OriginalInfo(..)
  ,payInt,payPrin,consolStmt,backoutDueIntByYield
  ,priceBond,PriceResult(..),pv,InterestInfo(..),RateReset(..)
  ,weightAverageBalance,fv2,calcZspread,payYield
  ,buildRateResetDates,convertToFace,isPaidOff)
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
import Analytics
import Data.Ratio 

import qualified Stmt as S 

import Data.List (findIndex,zip6,find)
import qualified Cashflow as CF

import GHC.Generics

import Debug.Trace
debug = flip trace

type RateReset = DatePattern 
type StepUpDates = DatePattern 


data InterestInfo = Floater IRate Index Spread RateReset DayCount (Maybe Floor) (Maybe Cap)
                  | Fix IRate DayCount                                    -- ^ fixed rate
                  | StepUpFix IRate DayCount StepUpDates Spread           -- ^ rate steps up base on dates
                  | StepUpByDate IRate Date InterestInfo InterestInfo     -- ^ Rate can be selective base on `pre`
                  | InterestByYield IRate
                  | RefRate IRate DealStats Float RateReset               -- ^ interest rate depends to a formula
                  | CapRate InterestInfo IRate                            -- ^ cap rate 
                  | FloorRate InterestInfo IRate                          -- ^ floor rate
                  deriving (Show, Eq, Generic)

data OriginalInfo = OriginalInfo {
  originBalance::Balance           -- ^ issuance balance
  ,originDate::Date                -- ^ issuance date
  ,originRate::Rate                -- ^ issuance rate of the bond
  ,maturityDate :: Maybe Date      -- ^ optional maturity date
} deriving (Show, Eq, Generic)

type PlannedAmorSchedule = Ts

data BondType = Sequential                                 -- ^ Pass through type tranche
              | PAC PlannedAmorSchedule                    -- ^ bond with schedule amortization 
              | PAC_Anchor PlannedAmorSchedule [BondName]  -- ^ pay till schdule balance if bonds from bond names has oustanding balance, if other bonds are paid off ,then pay oustanding balance
              | Lockout Date                               -- ^ No principal due till date
              | Z                                          -- ^ Z tranche
              | Equity                                     -- ^ Equity type tranche
              deriving (Show, Eq, Generic)

data Bond = Bond {
  bndName :: String
  ,bndType :: BondType                 -- ^ bond type ,which describle the how principal due was calculated
  ,bndOriginInfo :: OriginalInfo       -- ^ fact data on origination
  ,bndInterestInfo :: InterestInfo     -- ^ interest info which used to update interest rate
  ,bndBalance :: Balance               -- ^ current balance
  ,bndRate :: IRate                    -- ^ current rate
  ,bndDuePrin :: Balance               -- ^ principal due
  ,bndDueInt :: Balance                -- ^ interest due
  ,bndDueIntDate :: Maybe Date         -- ^ last interest due calc date
  ,bndLastIntPay :: Maybe Date         -- ^ last interest pay date
  ,bndLastPrinPay :: Maybe Date        -- ^ last principal pay date
  ,bndStmt :: Maybe S.Statement        -- ^ transaction history
} deriving (Show, Eq, Generic)



consolStmt :: Bond -> Bond
consolStmt b@Bond{bndName = bn, bndStmt = Just (S.Statement (txn:txns))}
  =  b {bndStmt = Just (S.Statement (reverse (foldl S.consolTxn [txn] txns)))} 

consolStmt b@Bond{bndName = bn, bndStmt = Nothing} = b 

-- | if no any principal due and interest due /oustanding balance ,then the bond is paid off
isPaidOff :: Bond -> Bool
isPaidOff b@Bond{bndBalance=bal,bndDuePrin=dp, bndDueInt=di}
  | bal==0 && dp==0 && di==0 = True 
  | otherwise = False 

payInt :: Date -> Amount -> Bond -> Bond
payInt d 0 bnd@(Bond bn bt oi iinfo 0 r 0 0 dueIntDate lpayInt lpayPrin stmt) = bnd
payInt d amt bnd@(Bond bn Equity oi iinfo bal r duePrin dueInt dueIntDate lpayInt lpayPrin stmt)
  = bnd { bndDueInt=new_due, bndStmt = new_stmt}
  where
    new_due = dueInt - amt
    new_stmt = S.appendStmt stmt (S.BondTxn d bal amt 0 r amt (S.PayYield bn))

payInt d amt bnd@(Bond bn bt oi iinfo bal r duePrin dueInt dueIntDate lpayInt lpayPrin stmt)
  = bnd {bndDueInt=new_due, bndStmt=new_stmt, bndLastIntPay = Just d}
  where
    new_due = dueInt - amt 
    new_stmt = S.appendStmt stmt (S.BondTxn d bal amt 0 r amt (S.PayInt [bn]))

payYield :: Date -> Amount -> Bond -> Bond 
payYield d amt bnd@(Bond bn bt oi iinfo bal r duePrin dueInt dueIntDate lpayInt lpayPrin stmt)
  = bnd {bndStmt= new_stmt}
  where
    new_stmt = S.appendStmt stmt (S.BondTxn d bal amt 0 r amt (S.PayYield bn))

payPrin :: Date -> Amount -> Bond -> Bond
payPrin d 0 bnd@(Bond bn bt oi iinfo 0 r 0 0 dueIntDate lpayInt lpayPrin stmt) = bnd
payPrin d amt bnd@(Bond bn bt oi iinfo bal r duePrin dueInt dueIntDate lpayInt lpayPrin stmt)
  = bnd {bndDuePrin =new_due, bndBalance = new_bal , bndStmt=new_stmt}
  where
    new_bal = bal - amt
    new_due = duePrin - amt
    new_stmt = S.appendStmt stmt (S.BondTxn d new_bal 0 amt 0 amt (S.PayPrin [bn] ))

convertToFace :: Balance -> Bond -> Balance
convertToFace bal b@Bond{bndOriginInfo = info}
  = bal / originBalance info

fv2 :: IRate -> Date -> Date -> Amount -> Amount
fv2 discount_rate today futureDay amt 
  = realToFrac $ (realToFrac amt) * factor 
  where
    factor::Double = (1 + realToFrac discount_rate) ** (distance / 365)
    distance::Double = fromIntegral $ daysBetween today futureDay

priceBond :: Date -> Ts -> Bond -> PriceResult
priceBond d rc b@(Bond bn _ (OriginalInfo obal od _ _) _ bal cr _ _ _ lastIntPayDay _ (Just (S.Statement txns)))
  | sum (S.getTxnAmt <$> futureCf) == 0 = PriceResult 0 0 0 0 0 0 
  | otherwise = 
                let
                  presentValue = foldr (\x acc -> acc + (pv rc d (S.getDate x) (S.getTxnAmt x))) 0 futureCf -- `debug` "PRICING -A"
                  cutoffBalance = case S.getTxnAsOf txns d of
                                      Nothing ->  (S.getTxnBalance fstTxn) + (S.getTxnPrincipal fstTxn) --  `debug` (show(getTxnBalance fstTxn))
                                                 where
                                                  fstTxn = head txns
                                      Just _txn -> S.getTxnBalance _txn   
                  accruedInt = case _t of
                                  Nothing -> (fromIntegral (max 0 (T.diffDays d leftPayDay))/365) * (mulBI leftBal cr)
                                  Just _ -> 0 
                                where
                                  _t = find (\x -> (S.getDate x) == d) txns
                                  leftTxns = takeWhile (\txn -> (S.getDate txn) < d) txns
                                  (leftPayDay,leftBal) = case leftTxns of
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
                               futureCf) / cutoffBalance) -- `debug` ("cut off balace"++show cutoffBalance)
                  duration = (foldr (\x acc ->
                                       ((*)  
                                         (divideBB (pv rc d (S.getDate x) (S.getTxnAmt x)) presentValue) 
                                         (yearCountFraction DC_ACT_365F d (S.getDate x)))
                                       + acc)
                                0
                                futureCf) -- `debug` "PRICING -C" -- `debug` ("WAL-->"++show wal) 
                  convexity = let 
                                b = (foldr (\x acc ->
                                                    let 
                                                        _t = yearCountFraction DC_ACT_365F d (S.getDate x) -- `debug` ("calc _T"++show d++">>"++show (S.getTxnDate x))
                                                        _t2 = _t * _t + _t -- `debug` ("T->"++show _t)
                                                        _cash_date = S.getDate x
                                                        _yield = getValByDate rc Exc _cash_date
                                                        _y = (1+ _yield) * (1+ _yield) -- `debug` ("yield->"++ show _yield++"By date"++show d)
                                                        _x = ((mulBR  (pv rc d _cash_date (S.getTxnAmt x)) _t2) / (fromRational _y)) -- `debug` ("PRICING -E") -- `debug` ("PV:->"++show (pv rc d (S.getTxnDate x) (S.getTxnAmt x))++"Y->"++ show _y++"T2-->"++ show _t2)
                                                    in 
                                                        _x + acc) 
                                            0
                                            futureCf) -- `debug` ("PRICING VALUE"++ show presentValue)
                              in 
                                b/presentValue -- `debug` "PRICING -D" -- `debug` ("B->"++show b++"PV"++show presentValue)
                in 
                  PriceResult presentValue (fromRational (100*(toRational presentValue)/(toRational obal))) (realToFrac wal) (realToFrac duration) (realToFrac convexity) accruedInt  `debug` ("Obal->"++ show obal++"Rate>>"++ show (bndRate b))
  where 
    futureCf = filter (\x -> (S.getDate x) > d) txns


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

calcZspread :: (Rational,Date) -> Int -> (Float, (Rational,Rational),Rational) -> Bond -> Ts -> Spread
calcZspread _ _ _ b@Bond{bndStmt = Nothing} _ = error "No Cashflow for bond"
calcZspread (tradePrice,priceDay) count (level ,(lastSpd,lastSpd2),spd) b@Bond{bndStmt = Just (S.Statement txns), bndOriginInfo = bInfo} riskFreeCurve  
  | count >= 10000 =  fromRational spd -- error "Failed to find Z spread with 10000 times try"
  | otherwise =
    let 
      (_,futureTxns) = splitByDate txns priceDay EqToRight
     
      cashflow = S.getTxnAmt <$> futureTxns
      ds = S.getDate <$> futureTxns
      oBalance = originBalance bInfo

      pvCurve = shiftTsByAmt riskFreeCurve spd -- `debug` ("Shfiting using spd"++ show (fromRational spd))
      pvs = [ pv pvCurve priceDay _d _amt | (_d, _amt) <- zip ds cashflow ] -- `debug` (" using pv curve"++ show pvCurve)
      newPrice = 100 * (sum pvs) -- `debug` ("PVS->>"++ show pvs)
      pricingFaceVal = toRational $ newPrice / oBalance -- `debug` ("new price"++ show newPrice)
      gap = (pricingFaceVal - tradePrice) -- `debug` ("Face val"++show pricingFaceVal++"T price"++show tradePrice)
      newSpd = case [gap ==0 ,gap > 0, spd > 0] of
                 [True,_,_]   -> spd
                 [_,True,_]   -> spd + f -- `debug` ("1 -> "++ show f)
                 [_,False,_]  -> spd - f -- `debug` ("3 -> "++ show f)
                 where 
                   f = let 
                        thresholds = toRational  <$> (level *) <$> [50,20,10,5,2,0.1,0.05,0.01,0.005]
                        shiftPcts = (level *) <$> [0.5,0.2,0.1,0.05,0.02,0.01,0.005,0.001,0.0005]
                       in 
                         case find (\(a,b) -> a < (abs(toRational gap))) (zip thresholds shiftPcts ) of
                           Just (_,v) -> toRational v  -- `debug` ("shifting ->"++ show v)
                           Nothing -> toRational (level * 0.00001) --  `debug` ("shifting-> <> 0.00005")
                  
      newLevel = case [abs(newSpd) < 0.0001
                       ,abs(newSpd-lastSpd)<0.000001
                       ,abs(newSpd-lastSpd2)<0.000001] of
                   [True,_,_] -> level * 0.5
                   [_,True,_] -> level * 0.5
                   [_,_,True] -> level * 0.5
                   _ -> level
    in 
      if abs(pricingFaceVal - tradePrice) <= 0.01 then 
        fromRational spd  -- `debug` ("Curve -> "++show pvCurve)
      else
        calcZspread (tradePrice,priceDay) (succ count) (newLevel, (spd, lastSpd), newSpd) b riskFreeCurve -- `debug` ("new price"++ show pricingFaceVal++"trade price"++ show tradePrice++ "new spd"++ show (fromRational newSpd))


buildRateResetDates :: InterestInfo -> StartDate -> EndDate -> [Date]
buildRateResetDates ii sd ed 
  = case ii of 
      (StepUpFix _ _ dp _ ) -> genSerialDatesTill2 NO_IE sd dp ed
      (Floater _ _ _ dp _ _ _) -> genSerialDatesTill2 NO_IE sd dp ed
      (StepUpByDate _ d (Floater _ _ _ dp1 _ _ _) (Floater _ _ _ dp2 _ _ _)) -> genSerialDatesTill2 NO_IE sd (AllDatePattern [dp1,dp2]) ed
      (StepUpByDate _ d _ (Floater _ _ _ dp _ _ _)) -> genSerialDatesTill2 NO_IE sd dp ed
      (StepUpByDate _ d (Floater _ _ _ dp _ _ _) _ ) -> genSerialDatesTill2 NO_IE sd dp ed
      (CapRate _ii _)  -> buildRateResetDates _ii sd ed 
      (FloorRate _ii _)  -> buildRateResetDates _ii sd ed 
      (RefRate _ _ _ dp)  -> genSerialDatesTill2 NO_IE sd dp ed 
      _ -> error "Failed to mach interest info when building rate reset dates"  
       


instance S.QueryByComment Bond where 
    queryStmt Bond{bndStmt = Nothing} tc = []
    queryStmt Bond{bndStmt = Just (S.Statement txns)} tc
      = filter (\x -> S.getTxnComment x == tc) txns

$(deriveJSON defaultOptions ''InterestInfo)
$(deriveJSON defaultOptions ''OriginalInfo)
$(deriveJSON defaultOptions ''BondType)
$(deriveJSON defaultOptions ''Bond)
