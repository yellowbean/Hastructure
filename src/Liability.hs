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
  ,getDueInt
  ,weightAverageBalance,calcZspread,payYield,getTotalDueInt
  ,buildRateResetDates,isAdjustble,StepUp(..),isStepUp,getDayCountFromInfo
  ,calcWalBond,patchBondFactor,fundWith,writeOff,InterestOverInterestType(..)
  ,getCurBalance,setBondOrigDate,isFloaterBond
  ,bndOriginInfoLens,bndIntLens,getBeginRate,_Bond,_BondGroup
  ,totalFundedBalance,getIndexFromInfo,buildStepUpDates
  ,accrueInt,stepUpInterestInfo,payIntByIndex,_MultiIntBond
  ,getDueIntAt,getDueIntOverIntAt,getDueIntOverInt,getTotalDueIntAt
  ,getCurRate
  ,bondCashflow
  )
  where

import Language.Haskell.TH
import Data.Aeson       hiding (json)
import Data.Aeson.TH
import Data.Fixed

import qualified Data.Time as T
import Lib (Period(..),Ts(..) ,TsPoint(..) ,daysBetween, weightedBy,paySeqLiabResi)

import Util
import DateUtil
import Types
import Analytics

import Data.Ratio 
import Data.Maybe
import Data.List
import qualified Data.Set as Set


import qualified Stmt as S 

import qualified Cashflow as CF
import qualified InterestRate as IR
import qualified Lib

import GHC.Generics
import qualified Data.Map as Map

import Debug.Trace
import InterestRate (UseRate(getIndexes))
import Control.Lens hiding (Index)
import Control.Lens.TH
import Language.Haskell.TH.Lens 
import Stmt (getTxnAmt)

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

isFloaterBond :: InterestInfo -> Bool
isFloaterBond Floater {} = True
isFloaterBond _ = False

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

data InterestOverInterestType = OverCurrRateBy Rational -- ^ inflat ioi rate by pct over current rate
                              | OverFixSpread Spread -- ^ inflat ioi rate by fix spread
                              deriving (Show, Eq, Generic, Ord, Read)


-- ^ the way how interest due amount is calculated
--------------------------- start Rate, index, spread, reset dates, daycount, floor, cap
data InterestInfo = Floater IRate Index Spread RateReset DayCount (Maybe Floor) (Maybe Cap)
                  | Fix IRate DayCount                                    -- ^ fixed rate
                  | InterestByYield IRate
                  | RefRate IRate DealStats Float RateReset               -- ^ interest rate depends to a formula
                  | CapRate InterestInfo IRate                            -- ^ cap rate 
                  | FloorRate InterestInfo IRate                          -- ^ floor rate
                  | WithIoI InterestInfo InterestOverInterestType         -- ^ Interest Over Interest(normal on left,IoI on right)
                  deriving (Show, Eq, Generic, Ord, Read)


-- data StepUp = PassDateSpread Date Spread                   -- ^ add a spread on a date and effective afterwards
--             | PassDateLadderSpread Date Spread RateReset   -- ^ add a spread on the date pattern
stepUpInterestInfo :: StepUp -> InterestInfo -> InterestInfo
stepUpInterestInfo sp ii =
  case ii of 
    (Floater a idx s dp dc f c) -> Floater a idx (s+getSpread sp) dp dc f c
    (Fix r dc) -> Fix (r+getSpread sp) dc
    (CapRate ii' r) -> CapRate (stepUpInterestInfo sp ii') r
    (FloorRate ii' r) -> FloorRate (stepUpInterestInfo sp ii') r
    (WithIoI ii' ooi) -> WithIoI (stepUpInterestInfo sp ii') ooi
    _ -> ii
  where
    getSpread (PassDateSpread _ s) = s
    getSpread (PassDateLadderSpread _ s _) = s

getDpFromIntInfo :: InterestInfo -> Maybe DatePattern
getDpFromIntInfo (Floater _ _ _ dp _ _ _) = Just dp
getDpFromIntInfo (RefRate _ _ _ dp) = Just dp
getDpFromIntInfo (CapRate ii _) = getDpFromIntInfo ii
getDpFromIntInfo (FloorRate ii _) = getDpFromIntInfo ii
getDpFromIntInfo (WithIoI ii _) = getDpFromIntInfo ii
getDpFromIntInfo _ = Nothing

getBeginRate :: InterestInfo -> IRate 
getBeginRate (Floater a _ _ _ _ _ _ ) = a
getBeginRate (Fix a _ ) = a
getBeginRate (RefRate a _ _ _ ) = a
getBeginRate (CapRate a  _ ) = getBeginRate a
getBeginRate (FloorRate a  _ ) = getBeginRate a
getBeginRate (WithIoI a _) = getBeginRate a
getBeginRate InterestByYield {} = 0.0

data StepUp = PassDateSpread Date Spread                   -- ^ add a spread on a date and effective afterwards
            | PassDateLadderSpread Date Spread RateReset   -- ^ add a spread on the date pattern
            deriving (Show, Eq, Generic, Ord, Read)

data OriginalInfo = OriginalInfo {
  originBalance::Balance           -- ^ issuance balance
  ,originDate::Date                -- ^ issuance date
  ,originRate::Rate                -- ^ issuance rate of the bond
  ,maturityDate :: Maybe Date      -- ^ optional maturity date
} deriving (Show, Eq, Generic, Ord, Read)


type PlannedAmorSchedule = Ts
-- ^ the way of principal due is calculated
data BondType = Sequential                                 -- ^ Pass through type tranche
              | PAC PlannedAmorSchedule                    -- ^ bond with schedule amortization 
              | PacAnchor PlannedAmorSchedule [BondName]   -- ^ pay till schdule balance if bonds from bond names has oustanding balance, if other bonds are paid off ,then pay oustanding balance
              | Lockout Date                               -- ^ No principal due till date
              | Z                                          -- ^ Z tranche
              | Equity                                     -- ^ Equity type tranche
              deriving (Show, Eq, Generic, Ord, Read)


-- TODO: for multi int bond, should origin rate be a list of rates?
--     : sofar remain orginate rate as a single rate for multi int bond

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
            | MultiIntBond {
              bndName :: String
              ,bndType :: BondType                 -- ^ bond type ,which describe the how principal due was calculated
              ,bndOriginInfo :: OriginalInfo       -- ^ fact data on origination
              ,bndInterestInfos :: [InterestInfo]     -- ^ interest info which used to update interest rate
              ,bndStepUps :: Maybe [StepUp]           -- ^ step up which update interest rate
              -- status
              ,bndBalance :: Balance               -- ^ current balance
              ,bndRates :: [IRate]                    -- ^ current rate
              ,bndDuePrin :: Balance               -- ^ principal due for current period
              ,bndDueInts :: [Balance]                -- ^ interest due
              ,bndDueIntOverInts :: [Balance]         -- ^ IoI
              ,bndDueIntDates :: Maybe [Date]         -- ^ last interest due calc date
              ,bndLastIntPays :: Maybe [Date]         -- ^ last interest pay date
              ,bndLastPrinPay :: Maybe Date        -- ^ last principal pay date
              ,bndStmt :: Maybe S.Statement        -- ^ transaction history
            }
            | BondGroup (Map.Map String Bond)      -- ^ bond group
            deriving (Show, Eq, Generic, Ord, Read)            



bndTxns :: Lens' Bond (Maybe S.Statement)
bndTxns = lens getter setter
  where 
    getter Bond{bndStmt = mStmt} = mStmt
    getter MultiIntBond{bndStmt = mStmt} = mStmt
    setter Bond{bndStmt = _} mStmt = Bond{bndStmt = mStmt}
    setter MultiIntBond{bndStmt = _} mStmt = MultiIntBond{bndStmt = mStmt}

bondCashflow :: Bond -> ([Date], [Amount])
bondCashflow b = 
  let t = (S.getAllTxns b)
  in 
    (S.getDate <$> t, S.getTxnAmt <$> t)

-- ^ remove empty transaction frgetBondByName :: Ast.Assetom a bond
consolStmt :: Bond -> Bond
consolStmt (BondGroup bMap) = BondGroup $ consolStmt <$> bMap
consolStmt b
  | S.hasEmptyTxn b = b
  | otherwise = let 
                  txn:txns = S.getAllTxns b
                  combinedBondTxns = foldl S.consolTxn [txn] txns    
                  droppedTxns = dropWhile S.isEmptyTxn combinedBondTxns 
                in 
                  b {bndStmt = Just (S.Statement (reverse droppedTxns))}

setBondOrigDate :: Date -> Bond -> Bond
setBondOrigDate d b@Bond{bndOriginInfo = oi} = b {bndOriginInfo = oi{originDate = d}}
setBondOrigDate d b@MultiIntBond{bndOriginInfo = oi} = b {bndOriginInfo = oi{originDate = d}}
setBondOrigDate d (BondGroup bMap) = BondGroup $ (setBondOrigDate d) <$> bMap

-- ^ build bond factors
patchBondFactor :: Bond -> Bond
patchBondFactor (BondGroup bMap) = BondGroup $ patchBondFactor <$> bMap
patchBondFactor bnd
  | (S.hasEmptyTxn bnd) = bnd
  | (originBalance (bndOriginInfo bnd)) == 0 = bnd
  | otherwise = let 
                  oBal = originBalance (bndOriginInfo bnd)
                  toFactor (BondTxn d b i p r0 c e f Nothing t) = (BondTxn d b i p r0 c e f (Just (fromRational (divideBB b oBal))) t)
                  newStmt = S.Statement $ toFactor <$> (S.getAllTxns bnd)
                in 
                  bnd {bndStmt = Just newStmt} 

payInt :: Date -> Amount -> Bond -> Bond
-- pay 0 interest, do nothing
payInt d 0 b = b

-- pay interest
payInt d amt bnd@(Bond bn bt oi iinfo _ bal r duePrin dueInt dueIoI dueIntDate lpayInt lpayPrin stmt)
  = bnd {bndDueInt=newDue, bndStmt=newStmt, bndLastIntPay = Just d, bndDueIntOverInt = newDueIoI}
  where
    rs = Lib.paySeqLiabilitiesAmt amt [dueIoI, dueInt] -- `debug` ("date"++ show d++"due "++show dueIoI++">>"++show dueInt)
    newDueIoI = dueIoI - head rs
    newDue = dueInt - rs !! 1 -- `debug` ("Avail fund"++ show amt ++" int paid out plan"++ show rs)
    newStmt = case bt of 
                Equity -> S.appendStmt (BondTxn d bal amt 0 r amt newDue newDueIoI Nothing (S.PayYield bn)) stmt 
                _ -> S.appendStmt (BondTxn d bal amt 0 r amt newDue newDueIoI Nothing (S.PayInt [bn])) stmt  -- `debug` ("date after"++ show d++"due "++show newDueIoI++">>"++show newDue)

-- pay multi-int bond ,IOI first and then interest due, sequentially
payInt d amt bnd@(MultiIntBond bn bt oi iinfo _ bal rs duePrin dueInts dueIoIs dueIntDate lpayInt lpayPrin stmt)
  = bnd {bndDueInts=newDues, bndStmt=newStmt
        , bndLastIntPays = Just (replicate l d), bndDueIntOverInts = newDueIoIs}
  where
    l = length iinfo
    ioiPaid = Lib.paySeqLiabilitiesAmt amt dueIoIs
    afterIoI = amt - sum ioiPaid
    duePaid = Lib.paySeqLiabilitiesAmt afterIoI dueInts
    newDueIoIs = zipWith (-) dueIoIs ioiPaid
    newDues = zipWith (-) dueInts duePaid
    newDueIoI = sum newDueIoIs
    newDue = sum newDues
    newStmt = case bt of 
                Equity -> S.appendStmt (BondTxn d bal amt 0 (sum rs) amt newDue newDueIoI Nothing (S.PayYield bn)) stmt 
                _ -> S.appendStmt (BondTxn d bal amt 0 (sum rs) amt newDue newDueIoI Nothing (S.PayInt [bn])) stmt  -- `debug` ("date after"++ show d++"due "++show newDueIoI++">>"++show newDue)

payIntByIndex :: Date -> Int -> Amount -> Bond -> Bond
-- pay 0 interest, do nothing
payIntByIndex d _ 0 b = b
payIntByIndex d idx amt bnd@(MultiIntBond bn bt oi iinfo _ bal rs duePrin dueInts dueIoIs dueIntDate lpayInt lpayPrin stmt) 
  = let
      dueIoI = dueIoIs !! idx 
      dueInt = dueInts !! idx -- `debug` ("date"++ show d++"in pay index fun"++ show amt)
      [newDueIoI,newDue] = Lib.paySeqLiabResi amt [dueIoI, dueInt] -- `debug` ("date"++ show d++" before pay due "++show dueIoI++">>"++show dueInt)
      newStmt = S.appendStmt (BondTxn d bal amt 0 (sum rs) amt newDue newDueIoI Nothing (S.PayInt [bn])) stmt -- `debug` ("date after"++ show d++"due(ioi) "++show newDueIoI++">> due "++show newDue)
      od = getOriginDate bnd
      ods = replicate (length iinfo) od
    in 
      bnd {bndDueInts = dueInts & ix idx .~ newDue
          ,bndDueIntOverInts = dueIoIs & ix idx .~ newDueIoI
          ,bndStmt = newStmt
          ,bndLastIntPays = case lpayInt of 
                              Nothing -> Just $ ods & ix idx .~ d
                              Just ds -> Just $ ds & ix idx .~ d}


-- ^ pay interest to single bond regardless any interest due
payYield :: Date -> Amount -> Bond -> Bond 
payYield d amt bnd@(Bond bn bt oi iinfo _ bal r duePrin dueInt dueIoI dueIntDate lpayInt lpayPrin stmt)
  = bnd {bndDueInt = newDue,bndDueIntOverInt=newDueIoI, bndStmt= newStmt}
  where
    [newDue,newDueIoI] = paySeqLiabResi amt [dueIoI, dueInt]
    newStmt = S.appendStmt (BondTxn d bal amt 0 r amt newDue newDueIoI Nothing (S.PayYield bn)) stmt 


-- ^ pay principal to single bond principal with limit of principal due
payPrin :: Date -> Amount -> Bond -> Bond
-- ^ no cash payment , do nothing
payPrin d 0 bnd = bnd
-- ^ no oustanding balance , do nothing
payPrin d _ bnd@(Bond bn bt oi iinfo _ 0 r 0 0 dueIoI dueIntDate lpayInt lpayPrin stmt) = bnd

payPrin d amt bnd = bnd {bndDuePrin =newDue, bndBalance = newBal , bndStmt=newStmt} 
  where
    newBal = (bndBalance bnd) - amt
    newDue = (bndDuePrin bnd) - amt 
    bn = bndName bnd
    stmt = bndStmt bnd
    dueIoI = getDueIntOverInt bnd
    dueInt = getDueInt bnd
    r = getCurRate bnd
    newStmt = S.appendStmt (BondTxn d newBal 0 amt r amt dueInt dueIoI Nothing (S.PayPrin [bn] )) stmt 


-- TODO  check maximum amount to write off
writeOff :: Date -> Amount -> Bond -> Bond
writeOff d 0 b = b
writeOff d amt bnd = bnd {bndBalance = newBal , bndStmt=newStmt}
    where
      newBal = (bndBalance bnd) - amt
      dueIoI = getDueIntOverInt bnd
      dueInt = getDueInt bnd
      bn = bndName bnd
      stmt = bndStmt bnd
      newStmt = S.appendStmt (BondTxn d newBal 0 0 0 0 dueInt dueIoI Nothing (S.WriteOff bn amt )) stmt 

fundWith :: Date -> Amount -> Bond -> Bond
fundWith d 0 b = b
fundWith d amt bnd
  = bnd {bndBalance = newBal, bndStmt=newStmt } 
  where
    dueIoI = getDueIntOverInt bnd
    dueInt = getDueInt bnd
    bn = bndName bnd
    stmt = bndStmt bnd
    newBal = (bndBalance bnd) + amt
    newStmt = S.appendStmt (BondTxn d newBal 0 (negate amt) 0 0 dueInt dueIoI Nothing (S.FundWith bn amt )) stmt 


-- TODO: add how to handle different rate for IOI
getIoI :: InterestInfo -> IRate -> IRate
getIoI (WithIoI _ (OverCurrRateBy r)) rate = rate * (1+ fromRational r)
getIoI (WithIoI _ (OverFixSpread r)) rate = rate + r
getIoI _ rate = rate


accrueInt :: Date -> Bond -> Bond
accrueInt d b@Bond{bndInterestInfo = ii,bndDueIntDate = mDueIntDate, bndDueInt= dueInt
                  , bndDueIntOverInt = dueIoI, bndRate = r, bndBalance = bal} 
  | d == beginDate = b
  | otherwise = let 
                  period = yearCountFraction (((fromMaybe DC_ACT_365F) . getDayCountFromInfo) ii) beginDate d
                  r2 = getIoI ii r
                  newDue = mulBR bal $ toRational r * period
                  newIoiDue = mulBR dueInt (toRational r2 * period)
                in 
                  b {bndDueInt = newDue+dueInt, bndDueIntOverInt = dueIoI+newIoiDue
                    ,bndDueIntDate = Just d}
  where
    beginDate = case mDueIntDate of
                  Just _d -> _d
                  Nothing -> getOriginDate b


-- TODO:  HOW to accrue a single index ? 
accrueInt d b@MultiIntBond{bndInterestInfos = iis, bndDueIntDates = mDueIntDates 
                            , bndDueInts = dueInts, bndDueIntOverInts = dueIoIs
                            , bndRates = rs, bndBalance = bal}
  | all (==d) beginDates = b
  | otherwise 
      = let 
        l = length iis -- `debug` ("bond Name>>> "++ show (bndName b))
        daycounts = (fromMaybe DC_ACT_365F) . getDayCountFromInfo <$> iis
        beginDates = case mDueIntDates of
                      Just ds -> ds
                      Nothing -> getOriginDate b <$ [1..l]
        periods = zipWith3 yearCountFraction daycounts beginDates (repeat d) -- `debug` ((bndName b) ++"  date"++ show d++"daycounts"++show daycounts++"beginDates "++show beginDates++ show "end dates"++ show d)
        newDues = zipWith3 (\r p due -> (mulBR (mulBIR bal r) p) + due) rs periods dueInts -- `debug` ((bndName b) ++"  date"++ show d++"rs"++show rs++"periods "++show periods++">>"++show dueInts)
        newIoiDues = zipWith5 (\r p due dueIoI ii -> 
                                (mulBR (mulBIR due (getIoI ii r)) p) + dueIoI)
                              rs
                              periods 
                              dueInts
                              dueIoIs
                              iis
      in
        b {bndDueInts = newDues, bndDueIntOverInts = newIoiDues, bndDueIntDates = Just (replicate l d) }
    where 
      l = length iis
      beginDates = case mDueIntDates of
                    Just ds -> ds
                    Nothing -> (getOriginDate b) <$ [1..l]

accrueInt d (BondGroup bMap) = BondGroup $ accrueInt d <$> bMap



-- ^ TODO WAL for bond group
calcWalBond :: Date -> Bond -> Rational
calcWalBond d b@Bond{bndStmt = Nothing} = 0.0
calcWalBond d b@MultiIntBond{bndStmt = Nothing} = 0.0
calcWalBond d b
  = let 
      txns = cutBy Exc Future d $ S.getAllTxns b 
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


getTxnRate :: Txn -> IRate
getTxnRate (BondTxn _ _ _ _ r _ _ _ _ _) = r
getTxnRate _ = 0.0

-- ^TODO to be tested
calcAccrueInt :: Date -> Bond -> Balance
calcAccrueInt d bnd@(BondGroup bMap) = sum $ calcAccrueInt d <$> Map.elems bMap
calcAccrueInt d bnd@(Bond {bndStmt = mstmt, bndRate = r, bndBalance = bal}) 
  | isNothing mstmt = IR.calcInt bal (getOriginDate bnd) d r DC_ACT_365F
  | d <= getOriginDate bnd = 0
  | isJust mstmt = 
      let
        txns = S.getAllTxns bnd
        ds = S.getDate <$> txns
      in
        case (S.getTxnAsOf txns d, elem d ds) of
          (_ , True) -> 0
          (Nothing,_) -> IR.calcInt bal (getOriginDate bnd) d r DC_ACT_365F -- `debug` (">>> "++ show (getOriginDate bnd) ++ ">>> "++ show d++"bal"++show bal++"rate"++show r++"r"++ show ( IR.calcInt bal (getOriginDate bnd) d r DC_ACT_365F)++ ">>\n txns"++ show txns)
          (Just txn,_) -> IR.calcInt (S.getTxnBalance txn) (S.getDate txn) d r DC_ACT_365F -- `debug` ("Accrue Int"++show d++">>"++show (S.getDate txn)++ ">>"++show (S.getTxnBalance txn)++"Rate"++show r)

calcAccrueInt d bnd@(MultiIntBond {bndStmt = mstmt, bndRates = rs, bndBalance = bal}) 
  | isNothing mstmt = IR.calcInt bal (getOriginDate bnd) d (sum rs) DC_ACT_365F
  | d <= getOriginDate bnd = 0
  | isJust mstmt = 
      let
        txns = S.getAllTxns bnd
        ds = S.getDate <$> txns
      in
        case (S.getTxnAsOf txns d, elem d ds) of
          (_, True) -> 0
          (Nothing,_) -> IR.calcInt bal (getOriginDate bnd) d (sum rs) DC_ACT_365F
          (Just txn,_) ->  IR.calcInt (S.getTxnBalance txn) (S.getDate txn) d (getTxnRate txn) DC_ACT_365F


priceBond :: Date -> Ts -> Bond -> PriceResult
priceBond d rc b@(Bond _ _ _ _ _ _ _ _ _ _ _ _ _ Nothing ) = PriceResult 0 0 0 0 0 0 []
priceBond d rc b@(MultiIntBond _ _ _ _ _ _ _ _ _ _ _ _ _ Nothing ) = PriceResult 0 0 0 0 0 0 []
priceBond d rc bnd
  | sum (S.getTxnAmt <$> futureCfs) == 0 = PriceResult 0 0 0 0 0 0 []
  | otherwise 
      = let
          presentValue = foldr (\x acc -> acc + pv rc d (S.getDate x) (S.getTxnAmt x)) 0 futureCfs -- `debug` "PRICING -A"
          cutoffBalance = case S.getTxnAsOf txns d of
                              Nothing ->  (S.getTxnBegBalance . head) txns
                              Just _txn -> S.getTxnBegBalance _txn
          accruedInt = calcAccrueInt d bnd
          wal = calcWalBond d bnd
          duration = calcDuration d (zip futureCfDates futureCfFlow) rc
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
          PriceResult presentValue (fromRational (100* (safeDivide' presentValue obal))) (realToFrac wal) (realToFrac duration) (realToFrac convexity) accruedInt futureCfs-- `debug` ("Obal->"++ show obal++"Rate>>"++ show (bndRate b))
  where 
    cr = getCurRate bnd
    bal = getCurBalance bnd
    txns = S.getAllTxns bnd
    futureCfs = cutBy Exc Future d txns
    futureCfDates = getDate <$> futureCfs
    futureCfFlow = getTxnAmt <$> futureCfs
    obal = getOriginBalance bnd
    od = getOriginDate bnd


-- ^ backout interest due for a Yield Maintainace type bond
-- ^ TODO: need to handle MuitIntBond here
backoutDueIntByYield :: Date -> Bond -> Balance
backoutDueIntByYield d b@(Bond _ _ (OriginalInfo obal odate _ _) (InterestByYield y) _ currentBalance _  _ _ _ _ _ _ stmt)
  = projFv - fvs - currentBalance  -- `debug` ("Date"++ show d ++"FV->"++show projFv++">>"++show fvs++">>cb"++show currentBalance)
    where
      projFv = fv2 y odate d obal 
      fvs = sum $ [ fv2 y cfDate d cfAmt  | (cfDate,cfAmt) <- cashflows ] -- `debug` (show d ++ ":CFS"++ show cashflows)
      cashflows = case stmt of
                    Just (S.Statement txns) -> [ ((S.getDate txn),(S.getTxnAmt txn))  | txn <- txns ] -- `debug` (show d ++":TXNS"++ show txns)
                    Nothing -> []

  
weightAverageBalance :: Date -> Date -> Bond -> Balance
weightAverageBalance sd ed b@(Bond _ _ (OriginalInfo ob bd _ _ )  _ _ currentBalance _ _ _ _ _ _ _ Nothing) 
  = mulBR currentBalance (yearCountFraction DC_ACT_365F (max bd sd) ed) 
weightAverageBalance sd ed b@(MultiIntBond _ _ (OriginalInfo ob bd _ _ )  _ _ currentBalance _ _ _ _ _ _ _ Nothing) 
  = mulBR currentBalance (yearCountFraction DC_ACT_365F (max bd sd) ed) 

weightAverageBalance sd ed b@(Bond _ _ (OriginalInfo ob bd _ _ )  _ _ currentBalance _ _ _ _ _ _ _ (Just stmt))
  = S.weightAvgBalance' 
        (max bd sd) 
        ed 
        (view S.statementTxns stmt)

weightAverageBalance sd ed b@(MultiIntBond _ _ (OriginalInfo ob bd _ _ )  _ _ currentBalance _ _ _ _ _ _ _ (Just stmt))
  = S.weightAvgBalance' 
        (max bd sd) 
        ed 
        (view S.statementTxns stmt)


weightAverageBalance sd ed bg@(BondGroup bMap)
  = sum $ weightAverageBalance sd ed <$> Map.elems bMap -- `debug` (">>>"++ show (weightAverageBalance sd ed <$> Map.elems bMap))

calcZspread :: (Rational,Date) -> Int -> (Float, (Rational,Rational),Rational) -> Bond -> Ts -> Spread
calcZspread _ _ _ b@Bond{bndStmt = Nothing} _ = error "No Cashflow for bond"
calcZspread _ _ _ b@MultiIntBond{bndStmt = Nothing} _ = error "No Cashflow for bond"
calcZspread (tradePrice,priceDay) count (level ,(lastSpd,lastSpd2),spd) b riskFreeCurve  
  | count >= 10000 =  fromRational spd -- error "Failed to find Z spread with 10000 times try"
  | otherwise =
    let 
      txns = S.getAllTxns b
      bInfo = bndOriginInfo b
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


totalFundedBalance :: Bond -> Balance
totalFundedBalance (BondGroup bMap) = sum $ totalFundedBalance <$> Map.elems bMap
totalFundedBalance b
  = let 
      txns = S.getAllTxns b
      isFundingTxn (FundWith _ _) = True
      isFundingTxn _ = False
      fundingTxns = S.filterTxn isFundingTxn txns
    in 
      sum $ (\(BondTxn d b i p r0 c di dioi f t) -> abs p) <$> fundingTxns

buildRateResetDates :: Bond -> StartDate -> EndDate -> [Date]
buildRateResetDates (BondGroup bMap) sd ed  =  concat $ (\x -> buildRateResetDates x sd ed) <$> Map.elems bMap
buildRateResetDates b@Bond{bndInterestInfo = ii,bndStepUp = mSt } sd ed 
  = let
      resetDp = getDpFromIntInfo ii 
      floaterRateResetDates (Just dp) = genSerialDatesTill2 NO_IE sd dp ed 
      floaterRateResetDates Nothing = []
    in 
      floaterRateResetDates resetDp

buildRateResetDates b@MultiIntBond{bndInterestInfos = iis} sd ed 
  = let 
      floaterRateResetDates (Just dp) = genSerialDatesTill2 NO_IE sd dp ed 
      floaterRateResetDates Nothing = []
    in 
      -- TODO: perf: sort and distinct
      concat $ (floaterRateResetDates . getDpFromIntInfo) <$> iis



buildStepUpDates :: Bond -> StartDate -> EndDate -> [Date]
buildStepUpDates (BondGroup bMap) sd ed  =  concat $ (\x -> buildStepUpDates x sd ed) <$> Map.elems bMap
buildStepUpDates b@Bond{bndStepUp = mSt } sd ed 
  = case mSt of
      Nothing -> []
      Just (PassDateSpread d _) -> [d]
      Just (PassDateLadderSpread fstSd _ dp) -> genSerialDatesTill2 IE fstSd dp ed

buildStepUpDates b@MultiIntBond{bndStepUps = mSt } sd ed 
  = case mSt of
      Nothing -> []
      Just sts -> Set.toList $
                    Set.fromList $
                      concat $
                        (\y ->
                          case y of 
                            (PassDateLadderSpread fstSd _ dp) -> genSerialDatesTill2 IE fstSd dp ed
                            (PassDateSpread d _) -> [d]
                            ) <$> sts


instance S.QueryByComment Bond where 
  queryStmt Bond{bndStmt = Nothing} tc = []
  queryStmt MultiIntBond{bndStmt = Nothing} tc = []
  queryStmt Bond{bndStmt = Just (S.Statement txns)} tc
    = Data.List.filter (\x -> S.getTxnComment x == tc) txns
  queryStmt MultiIntBond{bndStmt = Just (S.Statement txns)} tc
    = Data.List.filter (\x -> S.getTxnComment x == tc) txns

instance Liable Bond where 

  isPaidOff b@Bond{bndBalance=bal, bndDueInt=di, bndDueIntOverInt=dioi}
    | bal==0 && di==0 && dioi==0 = True 
    | otherwise = False
  isPaidOff MultiIntBond{bndBalance=bal, bndDueInts=dis, bndDueIntOverInts=diois}
    | bal==0 && sum dis==0 && sum diois==0 = True 
    | otherwise = False  -- `debug` (bn ++ ":bal"++show bal++"dp"++show dp++"di"++show di)
  isPaidOff (BondGroup bMap) = all (==True) $ isPaidOff <$> Map.elems bMap

  getCurBalance b@Bond {bndBalance = bal } = bal
  getCurBalance b@MultiIntBond {bndBalance = bal } = bal
  getCurBalance (BondGroup bMap) = sum $ getCurBalance <$> Map.elems bMap

  getCurRate Bond{bndRate = r} = r
  getCurRate MultiIntBond{bndRates = rs} = sum rs
  getCurRate (BondGroup bMap) = 
    fromRational $
      weightedBy
        (toRational . getCurBalance <$> Map.elems bMap)
        (toRational . getCurRate <$> Map.elems bMap)
  
  getOriginBalance b = originBalance $ bndOriginInfo b
  getOriginBalance (BondGroup bMap) = sum $ getOriginBalance <$> Map.elems bMap

  getOriginDate b = originDate $ bndOriginInfo b


  getDueInt b@Bond{bndDueInt=di} = di 
  getDueInt MultiIntBond{bndDueInts=dis} = sum dis
  getDueInt (BondGroup bMap) = sum $ getDueInt <$> Map.elems bMap

  getDueIntAt MultiIntBond{bndDueInts=dis} idx = dis !! idx
  getDueIntOverIntAt MultiIntBond{bndDueIntOverInts=diois} idx = diois !! idx 
  getTotalDueIntAt b idx = getDueIntAt b idx + getDueIntOverIntAt b idx

  getDueIntOverInt b@Bond{bndDueIntOverInt=dioi} = dioi
  getDueIntOverInt MultiIntBond{bndDueIntOverInts=diois} = sum diois
  getDueIntOverInt (BondGroup bMap) = sum $ getDueIntOverInt <$> Map.elems bMap

  getTotalDueInt b@Bond{bndDueInt=di,bndDueIntOverInt=dioi} = di + dioi
  getTotalDueInt MultiIntBond{bndDueInts=dis,bndDueIntOverInts=diois} = sum dis + sum diois
  getTotalDueInt (BondGroup bMap) = sum $ getTotalDueInt <$> Map.elems bMap

  getOutstandingAmount b = getTotalDueInt b + getCurBalance b

instance IR.UseRate Bond where 
  isAdjustbleRate :: Bond -> Bool
  isAdjustbleRate Bond{bndInterestInfo = iinfo} = isAdjustble iinfo
  -- getIndex Bond{bndInterestInfo = iinfo }
  getIndexes Bond{bndInterestInfo = iinfo}  = getIndexFromInfo iinfo
  getIndexes (BondGroup bMap)  = if Data.List.null combined then Nothing else Just combined
                                  where combined = concat . catMaybes  $ (\b -> getIndexFromInfo (bndInterestInfo b)) <$> Map.elems bMap
  getIndexes MultiIntBond{bndInterestInfos = iis} 
    = Just $ concat $ concat <$> getIndexFromInfo <$> iis
-- txnsLens :: Lens' Bond [Txn]
-- txnsLens = bndStmtLens . _Just . S.statementTxns
instance S.HasStmt Bond where 
  
  getAllTxns Bond{bndStmt = Nothing} = []
  getAllTxns Bond{bndStmt = Just (S.Statement txns)} = txns
  getAllTxns MultiIntBond{bndStmt = Nothing} = []
  getAllTxns MultiIntBond{bndStmt = Just (S.Statement txns)} = txns
  getAllTxns (BondGroup bMap) = concat $ S.getAllTxns <$> Map.elems bMap

  hasEmptyTxn Bond{bndStmt = Nothing} = True
  hasEmptyTxn Bond{bndStmt = Just (S.Statement [])} = True
  hasEmptyTxn MultiIntBond{bndStmt = Nothing} = True
  hasEmptyTxn MultiIntBond{bndStmt = Just (S.Statement [])} = True
  hasEmptyTxn (BondGroup bMap) = all S.hasEmptyTxn $ Map.elems bMap
  hasEmptyTxn _ = False


makeLensesFor [("bndType","bndTypeLens"),("bndOriginInfo","bndOriginInfoLens"),("bndInterestInfo","bndIntLens"),("bndStmt","bndStmtLens")] ''Bond
makeLensesFor [("bndOriginDate","bndOriginDateLens"),("bndOriginBalance","bndOriginBalanceLens"),("bndOriginRate","bndOriginRateLens")] ''OriginalInfo

makePrisms ''Bond

$(deriveJSON defaultOptions ''InterestOverInterestType)
$(deriveJSON defaultOptions ''InterestInfo)
$(deriveJSON defaultOptions ''OriginalInfo)
$(deriveJSON defaultOptions ''BondType)
$(deriveJSON defaultOptions ''StepUp)
$(deriveJSON defaultOptions ''Bond)