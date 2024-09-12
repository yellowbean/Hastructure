{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module Liability
  (Bond(..),BondType(..),OriginalInfo(..)
  ,payInt,payPrin,consolStmt,backoutDueIntByYield,isPaidOff,getCurBalance
  ,priceBond,PriceResult(..),pv,InterestInfo(..),RateReset(..)
  ,weightAverageBalance,calcZspread,payYield,scaleBond,totalDueInt
  ,buildRateResetDates,isAdjustble,StepUp(..),isStepUp,getDayCountFromInfo
  ,calcWalBond,patchBondFactor,fundWith,writeOff,InterestOverInterestType(..)
  ,getCurBalance,setBondOrigDate
  ,bndOriginInfoLens,bndIntLens)
  where

import Language.Haskell.TH
import Data.Aeson       hiding (json)
import Data.Aeson.TH
import Data.Fixed

import qualified Data.Time as T
import Lib (Period(..),Ts(..) ,TsPoint(..)
           , toDate, daysBetween, getIntervalFactors, daysBetweenI)

import Util
import DateUtil
import Types hiding (BondGroup)
import Analytics

import Data.Ratio 
import Data.Maybe

import qualified Stmt as S 

import Data.List (findIndex,zip6,find)
import qualified Cashflow as CF
import qualified InterestRate as IR
import qualified Lib

import GHC.Generics
import qualified Data.Map as Map

import Debug.Trace
import InterestRate (UseRate(getIndexes))
import Control.Lens hiding (Index)
import Control.Lens.TH
import Language.Haskell.TH.Lens (_BytesPrimL)
import Stmt (getTxnAmt)
import Data.Char (GeneralCategory(NotAssigned))
import qualified Stmt as L
-- import Deal.DealBase (UnderlyingDeal(futureCf))

debug = flip trace

-- | test if a bond may changes its interest rate
isAdjustble :: InterestInfo -> Bool 
isAdjustble Floater {} = True
isAdjustble RefRate {} = True
isAdjustble Fix {} = False
isAdjustble InterestByYield {} = False
isAdjustble (CapRate r _ ) = isAdjustble r
isAdjustble (FloorRate r _ ) = isAdjustble r
isAdjustble (WithIoI r _) = isAdjustble r

isStepUp :: Bond -> Bool
isStepUp Bond{bndStepUp = Nothing} = False
isStepUp _  = True


getIndexFromInfo :: InterestInfo -> Maybe [Index]
getIndexFromInfo (Floater _ idx _ _  _ _ _) = Just [idx]
getIndexFromInfo Fix {} = Nothing 
getIndexFromInfo InterestByYield {} = Nothing 
getIndexFromInfo RefRate {} = Nothing 
getIndexFromInfo (CapRate info _) = getIndexFromInfo info
getIndexFromInfo (FloorRate info _) = getIndexFromInfo info
getIndexFromInfo (WithIoI info _) = getIndexFromInfo info

getDayCountFromInfo :: InterestInfo -> Maybe DayCount
getDayCountFromInfo (Floater _ _ _ _ dc _ _) = Just dc
getDayCountFromInfo (Fix _ dc) = Just dc
getDayCountFromInfo InterestByYield {} = Nothing 
getDayCountFromInfo RefRate {} = Nothing 
getDayCountFromInfo (CapRate info _) = getDayCountFromInfo info
getDayCountFromInfo (FloorRate info _) = getDayCountFromInfo info
getDayCountFromInfo (WithIoI info _) = getDayCountFromInfo info
getDayCountFromInfo _ = Nothing

type RateReset = DatePattern 
type PlannedAmorSchedule = Ts

data InterestOverInterestType = OverCurrRateBy Rational -- ^ inflat ioi rate by pct over current rate
                             | OverFixSpread Spread -- ^ inflat ioi rate by fix spread
                             deriving (Show, Eq, Generic, Ord, Read)


--------------------------- start Rate, index, spread, reset dates, daycount, floor, cap
data InterestInfo = Floater IRate Index Spread RateReset DayCount (Maybe Floor) (Maybe Cap)
                  | Fix IRate DayCount                                    -- ^ fixed rate
                  | InterestByYield IRate
                  | RefRate IRate DealStats Float RateReset               -- ^ interest rate depends to a formula
                  | CapRate InterestInfo IRate                            -- ^ cap rate 
                  | FloorRate InterestInfo IRate                          -- ^ floor rate
                  | WithIoI InterestInfo InterestOverInterestType         -- ^ Interest Over Interest(normal on left,IoI on right)
                  deriving (Show, Eq, Generic, Ord, Read)
                  
data StepUp = PassDateSpread Date Spread                   -- ^ add a spread on a date and effective afterwards
            | PassDateLadderSpread Date Spread RateReset   -- ^ add a spread on the date pattern
            deriving (Show, Eq, Generic, Ord, Read)

data OriginalInfo = OriginalInfo {
  originBalance::Balance           -- ^ issuance balance
  ,originDate::Date                -- ^ issuance date
  ,originRate::Rate                -- ^ issuance rate of the bond
  ,maturityDate :: Maybe Date      -- ^ optional maturity date
} deriving (Show, Eq, Generic, Ord, Read)

data BondType = Sequential                                 -- ^ Pass through type tranche
              | PAC PlannedAmorSchedule                    -- ^ bond with schedule amortization 
              | PacAnchor PlannedAmorSchedule [BondName]   -- ^ pay till schdule balance if bonds from bond names has oustanding balance, if other bonds are paid off ,then pay oustanding balance
              | Lockout Date                               -- ^ No principal due till date
              | Z                                          -- ^ Z tranche
              | Equity                                     -- ^ Equity type tranche
              deriving (Show, Eq, Generic, Ord, Read)

data Bond = Bond {
              bndName :: String
              ,bndType :: BondType                 -- ^ bond type ,which describe the how principal due was calculated
              ,bndOriginInfo :: OriginalInfo       -- ^ fact data on origination
              ,bndInterestInfo :: InterestInfo     -- ^ interest info which used to update interest rate
              ,bndStepUp :: Maybe StepUp           -- ^ step up which update interest rate
              -- status
              ,bndBalance :: Balance               -- ^ current balance
              ,bndRate :: IRate                    -- ^ current rate
              ,bndDuePrin :: Balance               -- ^ principal due for current period
              ,bndDueInt :: Balance                -- ^ interest due
              ,bndDueIntOverInt :: Balance         -- ^ IoI
              ,bndDueIntDate :: Maybe Date         -- ^ last interest due calc date
              ,bndLastIntPay :: Maybe Date         -- ^ last interest pay date
              ,bndLastPrinPay :: Maybe Date        -- ^ last principal pay date
              ,bndStmt :: Maybe S.Statement        -- ^ transaction history
            } 
            | BondGroup (Map.Map String Bond)      -- ^ bond group
            deriving (Show, Eq, Generic, Ord, Read)            

consolStmt :: Bond -> Bond
consolStmt (BondGroup bMap) = BondGroup $ Map.map consolStmt bMap
consolStmt b@Bond{bndName = bn, bndStmt = Nothing} = b    
consolStmt b@Bond{bndName = bn, bndStmt = Just (S.Statement (txn:txns))}
  = let 
      combinedBondTxns = foldl S.consolTxn [txn] txns    
      droppedTxns = dropWhile S.isEmptyTxn combinedBondTxns 
    in 
      b {bndStmt = Just (S.Statement (reverse droppedTxns))}

setBondOrigDate :: Date -> Bond -> Bond
setBondOrigDate d b@Bond{bndOriginInfo = oi} = b {bndOriginInfo = oi{originDate = d}}
setBondOrigDate d (BondGroup bMap) = BondGroup $ Map.map (setBondOrigDate d) bMap

-- | build bond factors
patchBondFactor :: Bond -> Bond
patchBondFactor (BondGroup bMap) = BondGroup $ Map.map patchBondFactor bMap
patchBondFactor b@Bond{bndOriginInfo = bo, bndStmt = Nothing} = b
patchBondFactor b@Bond{bndOriginInfo = bo, bndStmt = Just (S.Statement txns) } 
  | originBalance bo == 0 = b
  | otherwise = let 
                  oBal = originBalance bo
                  toFactor (BondTxn d b i p r0 c e f Nothing t) = (BondTxn d b i p r0 c e f (Just (fromRational (divideBB b oBal))) t)
                  newStmt = S.Statement $ toFactor <$> txns
                in 
                  b {bndStmt = Just newStmt} 

payInt :: Date -> Amount -> Bond -> Bond
-- pay 0 interest, do nothing
payInt d 0 bnd@(Bond bn bt oi iinfo _ 0 r 0 0 dueIoI dueIntDate lpayInt lpayPrin stmt) = bnd

-- pay interest to equity tranche with interest
payInt d amt bnd@(Bond bn Equity oi iinfo _ bal r duePrin dueInt dueIoI dueIntDate lpayInt lpayPrin stmt)
  = bnd { bndDueInt = newDue, bndStmt = newStmt, bndDueIntOverInt = newDueIoI,  bndLastIntPay = Just d}
  where
    rs = Lib.paySeqLiabilitiesAmt amt [dueIoI,dueInt]
    newDueIoI = dueIoI - head rs
    newDue = dueInt - rs !! 1
    newStmt = S.appendStmt stmt (BondTxn d bal amt 0 r amt newDue newDueIoI Nothing (S.PayYield bn))

-- pay interest
payInt d amt bnd@(Bond bn bt oi iinfo _ bal r duePrin dueInt dueIoI dueIntDate lpayInt lpayPrin stmt)
  = bnd {bndDueInt=newDue, bndStmt=newStmt, bndLastIntPay = Just d, bndDueIntOverInt = newDueIoI}
  where
    rs = Lib.paySeqLiabilitiesAmt amt [dueIoI, dueInt] -- `debug` ("date"++ show d++"due "++show dueIoI++">>"++show dueInt)
    newDueIoI = dueIoI - head rs
    newDue = dueInt - rs !! 1 -- `debug` ("Avail fund"++ show amt ++" int paid out plan"++ show rs)
    newStmt = S.appendStmt stmt (BondTxn d bal amt 0 r amt newDue newDueIoI Nothing (S.PayInt [bn])) -- `debug` ("date after"++ show d++"due "++show newDueIoI++">>"++show newDue)

payYield :: Date -> Amount -> Bond -> Bond 
payYield d amt bnd@(Bond bn bt oi iinfo _ bal r duePrin dueInt dueIoI dueIntDate lpayInt lpayPrin stmt)
  = bnd {bndStmt= newStmt}
  where
    newStmt = S.appendStmt stmt (BondTxn d bal amt 0 r amt dueInt dueIoI Nothing (S.PayYield bn))

payPrin :: Date -> Amount -> Bond -> Bond
payPrin d 0 bnd@(Bond bn bt oi iinfo _ 0 r 0 0 dueIoI dueIntDate lpayInt lpayPrin stmt) = bnd
payPrin d _ bnd@(Bond bn bt oi iinfo _ 0 r 0 0 dueIoI dueIntDate lpayInt lpayPrin stmt) = bnd
payPrin d amt bnd@(Bond bn bt oi iinfo _ bal r duePrin dueInt dueIoI dueIntDate lpayInt lpayPrin stmt)
  = bnd {bndDuePrin =newDue, bndBalance = newBal , bndStmt=newStmt} -- `debug` ("after pay prin:"++ show d ++">"++ show bn++"due"++show newDue++"bal"++ show newBal )
  where
    newBal = bal - amt
    newDue = duePrin - amt 
    newStmt = S.appendStmt stmt (BondTxn d newBal 0 amt 0 amt dueInt dueIoI Nothing (S.PayPrin [bn] ))

writeOff :: Date -> Amount -> Bond -> Bond
writeOff d 0 b = b -- `debug` ("Zero on wirte off")
writeOff d amt bnd@(Bond bn bt oi iinfo _ bal r duePrin dueInt dueIoI dueIntDate lpayInt lpayPrin stmt)
  = bnd {bndBalance = newBal , bndStmt=newStmt}
  where
    newBal = bal - amt
    newStmt = S.appendStmt stmt (BondTxn d newBal 0 0 0 0 dueInt dueIoI Nothing (S.WriteOff bn amt ))

fundWith :: Date -> Amount -> Bond -> Bond
fundWith d 0 b = b
fundWith d amt bnd@(Bond bn bt oi iinfo _ bal r duePrin dueInt dueIoI dueIntDate lpayInt lpayPrin stmt)
  = bnd  {bndBalance = newBal 
          , bndStmt=newStmt
          } 
  where
    newBal = bal + amt 
    newStmt = S.appendStmt stmt (BondTxn d newBal 0 (negate amt) 0 0 dueInt dueIoI Nothing (S.FundWith bn amt ))


priceBond :: Date -> Ts -> Bond -> PriceResult
priceBond d rc b@(Bond bn _ (OriginalInfo obal od _ _) iinfo _ bal cr _ _ _ _ lastIntPayDay _ (Just (S.Statement txns)))
  | sum (S.getTxnAmt <$> futureCfs) == 0 = PriceResult 0 0 0 0 0 0 []
  | otherwise = 
                let
                  presentValue = foldr (\x acc -> acc + pv rc d (S.getDate x) (S.getTxnAmt x)) 0 futureCfs -- `debug` "PRICING -A"
                  cutoffBalance = case S.getTxnAsOf txns d of
                                      Nothing ->  (S.getTxnBegBalance . head) txns
                                      Just _txn -> S.getTxnBegBalance _txn
                  accruedInt = case _t of
                                  Nothing -> max 0 $ IR.calcInt leftBal leftPayDay d cr dcToUse 
                                  Just _ -> 0 
                                where
                                  dcToUse = fromMaybe DC_ACT_365F $ getDayCountFromInfo iinfo
                                  _t = find (\x -> S.getDate x == d) txns
                                  leftTxns = cutBy Exc Past d txns
                                  (leftPayDay,leftBal) = case leftTxns of
                                                           [] -> case lastIntPayDay of
                                                                   Nothing -> (od,bal)
                                                                   Just _d -> (_d,bal)
                                                           _ -> let
                                                                  leftTxn = last leftTxns
                                                                in
                                                                  (S.getDate leftTxn,S.getTxnBalance leftTxn)
                  wal = calcWalBond d b
                  duration = let 
                               ps = zip futureCfDates futureCfFlow
                             in 
                               calcDuration d ps rc
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
                                            futureCfs) -- `debug` ("PRICING VALUE"++ show presentValue)
                              in 
                                b/presentValue -- `debug` "PRICING -D" -- `debug` ("B->"++show b++"PV"++show presentValue)
                in 
                  PriceResult presentValue (fromRational (100*(toRational presentValue)/(toRational obal))) (realToFrac wal) (realToFrac duration) (realToFrac convexity) accruedInt futureCfs-- `debug` ("Obal->"++ show obal++"Rate>>"++ show (bndRate b))
  where 
    futureCfs = cutBy Exc Future d txns
    futureCfDates = getDate <$> futureCfs
    futureCfFlow = getTxnAmt <$> futureCfs


priceBond d rc b@(Bond _ _ _ _ _ _ _ _ _ _ _ _ _ Nothing ) = PriceResult 0 0 0 0 0 0 []

calcWalBond :: Date -> Bond -> Rational
calcWalBond d b@Bond{bndStmt = Nothing} = 0.0
calcWalBond d b@Bond{bndStmt = Just (S.Statement _txns)}
 = let 
      txns = cutBy Exc Future d _txns  
      cutoffBalance =  (S.getTxnBegBalance . head ) txns 
      lastBalance = (S.getTxnBalance . last) txns 
      firstTxnDate = d 
      gapDays = (daysBetween firstTxnDate) . S.getDate <$> txns
      weightPrins = zipWith (*) (S.getTxnPrincipal <$> txns) (fromIntegral <$> gapDays) 
      wal = sum weightPrins / 365 / cutoffBalance 
    in 
      if lastBalance > 0 then
        0  
      else
        toRational wal -- `debug` ("WAL-->"++show (bndName b)++">>"++show wal)



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
calcBondYield _ _ (Bond _ _ _ _ _ _ _ _ _ _ _ _ _ Nothing) = 0
calcBondYield d cost b@(Bond _ _ _ _ _ _ _ _ _ _ _ _ _ (Just (S.Statement txns)))
  = _calcIRR cost 0.05 d (BalanceCurve cashflows)
    where
      cashflows = [ TsPoint (S.getDate txn) (S.getTxnAmt txn)  | txn <- txns ]

-- ^ backout interest due for a Yield Maintainace type bond
backoutDueIntByYield :: Date -> Bond -> Balance
backoutDueIntByYield d b@(Bond _ _ (OriginalInfo obal odate _ _) (InterestByYield y) _ currentBalance _  _ _ _ _ _ _ stmt)
  = projFv - fvs - currentBalance  -- `debug` ("Date"++ show d ++"FV->"++show projFv++">>"++show fvs++">>cb"++show currentBalance)
    where
      projFv = fv2 y odate d obal 
      fvs = sum $ [ fv2 y cfDate d cfAmt  | (cfDate,cfAmt) <- cashflows ] -- `debug` (show d ++ ":CFS"++ show cashflows)
      cashflows = case stmt of
                    Just (S.Statement txns) -> [ ((S.getDate txn),(S.getTxnAmt txn))  | txn <- txns ] -- `debug` (show d ++":TXNS"++ show txns)
                    Nothing -> []


-- weightAverageBalance :: Date -> Date -> Bond -> Balance
weightAverageBalance sd ed b@(Bond _ _ (OriginalInfo ob bd _ _ )  _ _ currentBalance _ _ _ _ _ _ _ Nothing) 
  = mulBR currentBalance (yearCountFraction DC_ACT_365F (max bd sd) ed) 
weightAverageBalance sd ed b@(Bond _ _ (OriginalInfo ob bd _ _ )  _ _ currentBalance _ _ _ _ _ _ _ (Just stmt))
  = L.weightAvgBalance' 
      (max bd sd) 
      ed 
      (view S.statementTxns stmt)

weightAverageBalance sd ed bg@(BondGroup bMap)
  = sum $ weightAverageBalance sd ed <$> Map.elems bMap -- `debug` (">>>"++ show (weightAverageBalance sd ed <$> Map.elems bMap))

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
      newPrice = 100 * sum pvs -- `debug` ("PVS->>"++ show pvs)
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
                         case find (\(a,b) -> a < abs(toRational gap)) (zip thresholds shiftPcts ) of
                           Just (_,v) -> toRational v  -- `debug` ("shifting ->"++ show v)
                           Nothing -> toRational (level * 0.00001) --  `debug` ("shifting-> <> 0.00005")
                  
      newLevel = case [abs newSpd < 0.0001
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

totalDueInt :: Bond -> Balance
totalDueInt Bond{bndDueInt = a,bndDueIntOverInt = b } = a + b 
totalDueInt (BondGroup bMap) = sum $ totalDueInt <$> Map.elems bMap

buildRateResetDates :: Bond -> StartDate -> EndDate -> [Date]
buildRateResetDates (BondGroup bMap) sd ed  =  concat $ (\x -> buildRateResetDates x sd ed) <$> Map.elems bMap
buildRateResetDates b@Bond{bndInterestInfo = ii,bndStepUp = mSt } sd ed 
  = let 
      floaterRateResetDates = case ii of 
                                (Floater _ _ _ dp _ _ _) -> genSerialDatesTill2 NO_IE sd dp ed  -- `debug` ("building rest2"++show (bndName b )++"dp"++show dp++"ed"++show ed++"sd"++show sd  )
                                (CapRate ii _)  -> buildRateResetDates b {bndInterestInfo = ii} sd ed 
                                (FloorRate ii _)  -> buildRateResetDates b {bndInterestInfo = ii} sd ed 
                                (RefRate _ _ _ dp)  -> genSerialDatesTill2 NO_IE sd dp ed 
                                x -> []  -- `debug` ("fall out"++ show x)
      stepUpDates = case mSt of
                      Nothing -> []
                      Just (PassDateSpread d _) -> [d]
                      Just (PassDateLadderSpread fstSd _ dp) -> genSerialDatesTill2 IE fstSd dp ed
    in 
      floaterRateResetDates ++ stepUpDates -- `debug` ("building rest1"++show floaterRateResetDates++"bname"++ show (bndName b ))




scaleBond :: Rate -> Bond -> Bond
scaleBond r (BondGroup bMap) = BondGroup $ Map.map (scaleBond r) bMap
scaleBond r b@Bond{ bndOriginInfo = oi, bndInterestInfo = iinfo, bndStmt = mstmt
                  , bndBalance = bal, bndDuePrin = dp, bndDueInt = di, bndDueIntDate = did
                  , bndLastIntPay = lip, bndLastPrinPay = lpp
                  , bndType = bt} 
  = b {
    bndType = scaleBndType r bt
    ,bndOriginInfo = scaleBndOriginInfo r oi
    ,bndBalance = mulBR bal r
    ,bndDuePrin = mulBR dp r
    ,bndDueInt = mulBR di r
    ,bndStmt = scaleStmt r mstmt
  }
    where 
      scaleBndType r (PAC ts) = let 
                                  vs = (flip mulBR r . fromRational <$> getTsVals ts)
                                  ds = getTsDates ts
                                in 
                                  PAC $ BalanceCurve [ TsPoint d v | (d,v) <- zip ds vs]
      scaleBndType r _bt = _bt
      
      scaleBndOriginInfo r oi@OriginalInfo{originBalance = ob} = oi {originBalance = mulBR ob r}
      
      scaleStmt r Nothing = Nothing
      scaleStmt r (Just (S.Statement txns)) = Just (S.Statement (S.scaleTxn r <$> txns))

instance S.QueryByComment Bond where 
  queryStmt Bond{bndStmt = Nothing} tc = []
  queryStmt Bond{bndStmt = Just (S.Statement txns)} tc
    = filter (\x -> S.getTxnComment x == tc) txns

instance Liable Bond where 
  isPaidOff b@Bond{bndName = bn,bndBalance=bal,bndDuePrin=dp, bndDueInt=di, bndDueIntOverInt=dioi}
    | bal==0 && di==0 && dioi==0 = True 
    | otherwise = False  -- `debug` (bn ++ ":bal"++show bal++"dp"++show dp++"di"++show di)
 
  isPaidOff (BondGroup bMap) = all (==True) $ isPaidOff <$> Map.elems bMap

  getCurBalance b@Bond{bndBalance=bal} = bal
  getCurBalance (BondGroup bMap) = sum $ getCurBalance <$> Map.elems bMap
  
  getOriginBalance b@Bond{ bndOriginInfo = bo } = originBalance bo
  getOriginBalance (BondGroup bMap) = sum $ getOriginBalance <$> Map.elems bMap

  getDueInt b@Bond{bndDueInt=di,bndDueIntOverInt=dioi} = di + dioi
  getDueInt (BondGroup bMap) = sum $ getDueInt <$> Map.elems bMap

  getOutstandingAmount b = getDueInt b + getCurBalance b

instance IR.UseRate Bond where 
  isAdjustbleRate :: Bond -> Bool
  isAdjustbleRate Bond{bndInterestInfo = iinfo} = isAdjustble iinfo
  -- getIndex Bond{bndInterestInfo = iinfo }
  getIndexes Bond{bndInterestInfo = iinfo}  = getIndexFromInfo iinfo
  getIndexes (BondGroup bMap)  = if null combined then Nothing else Just combined
                                  where combined = concat . catMaybes  $ (\b -> getIndexFromInfo (bndInterestInfo b)) <$> Map.elems bMap
     
makeLensesFor [("bndType","bndTypeLens"),("bndOriginInfo","bndOriginInfoLens"),("bndInterestInfo","bndIntLens"),("bndStmt","bndStmtLens")] ''Bond
makeLensesFor [("bndOriginDate","bndOriginDateLens"),("bndOriginBalance","bndOriginBalanceLens"),("bndOriginRate","bndOriginRateLens")] ''OriginalInfo

$(deriveJSON defaultOptions ''InterestOverInterestType)
$(deriveJSON defaultOptions ''InterestInfo)
$(deriveJSON defaultOptions ''OriginalInfo)
$(deriveJSON defaultOptions ''BondType)
$(deriveJSON defaultOptions ''StepUp)
$(deriveJSON defaultOptions ''Bond)
