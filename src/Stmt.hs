{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE InstanceSigs #-}

module Stmt
  (Statement(..)
   ,getTxns,getTxnComment,getTxnAmt,toDate,getTxnPrincipal,getTxnAsOf,getTxnBalance
   ,appendStmt,combineTxn,sliceStmt,getTxnBegBalance,getDate,getDates
   ,TxnComment(..),QueryByComment(..)
   ,weightAvgBalanceByDates,weightAvgBalance,weightAvgBalance',sumTxn, consolTxn
   ,getFlow,FlowDirection(..), aggByTxnComment,scaleByFactor
   ,scaleTxn,isEmptyTxn, statementTxns, viewBalanceAsOf
  )
  where

import Lib (toDate,getIntervalFactors)
import Util (mulBR, mulBInt)
import Types 
import Language.Haskell.TH
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Aeson hiding (json)
import Text.Regex.Base
import Text.Regex.PCRE
import Data.Fixed
import Data.List
import Data.Maybe
import GHC.Generics
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Map as M

import Control.Applicative (liftA2)

import Control.Lens hiding (element,Empty)
import Control.Lens.TH

import Debug.Trace
debug = flip trace


aggByTxnComment :: [Txn] -> M.Map TxnComment [Txn] -> M.Map TxnComment Balance
aggByTxnComment [] m = M.map sumTxn m 
aggByTxnComment (txn:txns) m 
  | M.member c m = aggByTxnComment txns (M.adjust ([txn] ++) c m)
  | otherwise = aggByTxnComment txns (M.insert c [txn] m)
  where 
    c = normalized $ getTxnComment txn
    normalized (FundWith bn _) = FundWith bn 0
    normalized (PurchaseAsset n _) = PurchaseAsset n 0
    normalized (TxnComments txns) = TxnComments [ normalized x | x <- txns ]
    normalized cmt = cmt

scaleTxn :: Rate -> Txn -> Txn
scaleTxn r (BondTxn d b i p r0 c di dioi f t) = BondTxn d (mulBR b r) (mulBR i r) (mulBR p r) r0 (mulBR c r) (mulBR di r) (mulBR dioi r) f t
scaleTxn r (AccTxn d b a t) = AccTxn d (mulBR b r) (mulBR a r) t
scaleTxn r (ExpTxn d b a b0 t) = ExpTxn d (mulBR b r) (mulBR a r) (mulBR b0 r) t
scaleTxn r (SupportTxn d b b0 i p c t) = SupportTxn d (flip mulBR r <$> b) (mulBR b0 r) (mulBR i r) (mulBR p r) (mulBR c r) t
scaleTxn r (IrsTxn d b a i0 i1 b0 t) = IrsTxn d (mulBR b r) (mulBR a r) i0 i1 (mulBR b0 r) t
scaleTxn r (EntryTxn d b a t) = EntryTxn d (mulBR b r)  (mulBR a r) t

scaleByFactor :: Rate -> [Txn] -> [Txn]
scaleByFactor r [] = []
scaleByFactor r txns = map (scaleTxn r) txns

sumTxn :: [Txn] -> Balance
sumTxn txns = sum $ getTxnAmt <$> txns

getTxnComment :: Txn -> TxnComment
getTxnComment (BondTxn _ _ _ _ _ _ _ _ _ t) = t
getTxnComment (AccTxn _ _ _ t ) = t
getTxnComment (ExpTxn _ _ _ _ t ) = t
getTxnComment (SupportTxn _ _ _ _ _ _ t ) = t
getTxnComment (IrsTxn _ _ _ _ _ _ t ) = t
getTxnComment (EntryTxn _ _ _ t ) = t
getTxnComment (TrgTxn _ _ t) = t

getTxnBalance :: Txn -> Balance
getTxnBalance (BondTxn _ t _ _ _ _ _ _ _ _) = t
getTxnBalance (AccTxn _ t _ _ ) = t
getTxnBalance (ExpTxn _ t _ _ _ ) = t
getTxnBalance (SupportTxn _ _ t _ _ _ _ ) = t -- drawed balance
getTxnBalance (EntryTxn _ t _ _) = t


-- | SupportTxn Date (Maybe Balance) Balance DueInt DuePremium Cash TxnComment    

getTxnBegBalance :: Txn -> Balance
getTxnBegBalance (BondTxn _ t _ p _ _ _ _ _ _) = t + p
getTxnBegBalance (AccTxn _ b a _ ) = b - a
getTxnBegBalance (SupportTxn _ _ a b _ _ _) = b + a
getTxnBegBalance (EntryTxn _ a b _) = a + b 

getTxnPrincipal :: Txn -> Balance
getTxnPrincipal (BondTxn _ _ _ t _ _ _ _ _ _) = t

getTxnAmt :: Txn -> Balance
getTxnAmt (BondTxn _ _ _ _ _ t _ _ _ _) = t
getTxnAmt (AccTxn _ _ t _ ) = t
getTxnAmt (ExpTxn _ _ t _ _ ) = t
getTxnAmt (SupportTxn _ _ _ _ _ t _) = t
getTxnAmt (IrsTxn _ _ t _ _ _ _ ) = t
getTxnAmt (EntryTxn _ _ t _) = t
getTxnAmt TrgTxn {} = 0.0

getTxnAsOf :: [Txn] -> Date -> Maybe Txn
getTxnAsOf txns d = find (\x -> getDate x <= d) $ reverse txns

emptyTxn :: Txn -> Date -> Txn
emptyTxn BondTxn {} d = BondTxn d 0 0 0 0 0 0 0 Nothing Empty
emptyTxn AccTxn {} d = AccTxn d 0 0 Empty
emptyTxn ExpTxn {} d = ExpTxn d 0 0 0 Empty
emptyTxn SupportTxn {} d = SupportTxn d Nothing 0 0 0 0 Empty
emptyTxn IrsTxn {} d = IrsTxn d 0 0 0 0 0 Empty
emptyTxn EntryTxn {} d = EntryTxn d 0 0 Empty
emptyTxn TrgTxn {} d = TrgTxn d False Empty

isEmptyTxn :: Txn -> Bool
isEmptyTxn (BondTxn _ 0 0 0 _ 0 0 0 _ Empty) = True
isEmptyTxn (AccTxn _ 0 0 Empty) = True
isEmptyTxn (ExpTxn _ 0 0 0 Empty) = True
isEmptyTxn (SupportTxn _ Nothing 0 0 0 0 Empty) = True
isEmptyTxn (IrsTxn _ 0 0 0 0 0 Empty) = True
isEmptyTxn (EntryTxn _ 0 0 Empty) = True
isEmptyTxn _ = False


sliceStmt :: Date -> Date -> Statement -> Statement
sliceStmt sd ed (Statement txns) 
  = Statement $ sliceBy II sd ed txns

viewBalanceAsOf :: Date -> [Txn] -> Balance
viewBalanceAsOf d [] = 0.0 
viewBalanceAsOf d txns 
  | d < begDate = getTxnBegBalance fstTxn -- `debug` (" get first txn")
  | d > endDate = getTxnBalance lstTxn -- `debug` (" get last txn")
  | otherwise = getTxnBalance $ fromJust $ getTxnAsOf txns d -- `debug` ("Found txn>>>>>"++show d++show (getTxnAsOf txns d))
  where 
    fstTxn = head txns
    lstTxn = last txns
    begDate = getDate fstTxn
    endDate = getDate lstTxn

weightAvgBalanceByDates :: [Date] -> [Txn] -> [Balance]
weightAvgBalanceByDates ds txns 
  = (\(_sd,_ed) -> weightAvgBalance _sd _ed txns) <$> intervals -- `debug` ("interval"++ show intervals++ show txns)
  where 
      intervals = zip (init ds) (tail ds) 

-- ^ Txn must be full transactions
weightAvgBalance :: Date -> Date -> [Txn] -> Balance -- txn has to be between sd & ed
weightAvgBalance sd ed txns 
  = sum $ zipWith mulBR bals dsFactor -- `debug` ("WavgBalace "++show bals++show dsFactor)
  where 
      _txns = sliceBy IE sd ed txns
      bals = map getTxnBegBalance _txns ++ [getTxnBalance (last _txns)]
      ds = [sd] ++ map getDate _txns ++ [ed] 
      dsFactor = getIntervalFactors ds  -- `debug` ("DS>>>"++show ds)

weightAvgBalance' :: Date -> Date -> [Txn] -> Balance 
weightAvgBalance' sd ed [] = 0.0 
weightAvgBalance' sd ed (_txn:_txns)
  = let 
      -- txns = sliceBy EE sd ed txns
      txns = reverse $ foldl consolTxn [_txn] _txns
      viewDs = sort $ [sd,ed] ++ (getDate <$> (sliceBy EE  sd ed txns))
      balances = flip viewBalanceAsOf txns <$> viewDs -- `debug` ("get bal snapshot"++ show viewDs++ ">>>"++show txns)
      factors = getIntervalFactors viewDs
    in 
      sum $ zipWith mulBR balances factors --`debug` ("In weight avg bal: Factors"++show factors++"Balances"++show balances ++ "interval "++ show (sd,ed))   

data Statement = Statement [Txn]
              deriving (Show, Generic, Eq, Ord, Read)

appendStmt :: Maybe Statement -> Txn -> Maybe Statement
appendStmt (Just stmt@(Statement txns)) txn = Just $ Statement (txns++[txn])
appendStmt Nothing txn = Just $ Statement [txn]


statementTxns :: Lens' Statement [Txn]
statementTxns = lens getter setter
  where 
    getter (Statement txns) = txns
    setter (Statement _) txns = Statement txns


consolTxn :: [Txn] -> Txn -> [Txn]
consolTxn [] txn = [txn]
consolTxn (txn:txns) txn0
  | getDate txn == getDate txn0 = combineTxn txn txn0:txns
  | otherwise = txn0:txn:txns 

getTxns :: Maybe Statement -> [Txn]
getTxns Nothing = []
getTxns (Just (Statement txn)) = txn

-- BondTxn Date Balance Interest Principal IRate Cash DueInt DueIoI (Maybe Float) TxnComment     -- ^ bond transaction record for interest and principal 

combineTxn :: Txn -> Txn -> Txn
combineTxn (BondTxn d1 b1 i1 p1 r1 c1 f1 g1 h1 m1) (BondTxn d2 b2 i2 p2 r2 c2 f2 g2 h2 m2)
    = BondTxn d1 b2 (i1 + i2) (p1 + p2) r2 (c1+c2) f2 g2 h2 (TxnComments [m1,m2]) 
combineTxn (SupportTxn d1 b1 b0 i1 p1 c1 m1) (SupportTxn d2 b2 b02 i2 p2 c2 m2)
    = SupportTxn d1 b2  b02 (i1 + i2) (p1 + p2) (c1 + c2) (TxnComments [m1,m2])


data FlowDirection = Inflow 
                   | Outflow
                   | Interflow
                   | Noneflow
                   deriving (Eq,Show,Generic)

getFlow :: TxnComment -> FlowDirection
getFlow comment =
    case comment of 
      PayInt _ -> Outflow
      PayYield _ -> Outflow
      PayPrin _ -> Outflow
      PayGroupPrin _ -> Outflow
      PayGroupInt _ -> Outflow 
      WriteOff _ _ -> Noneflow
      FundWith _ _ -> Inflow
      PayPrinResidual _ -> Outflow
      PayFee _ -> Outflow
      SeqPayFee _ -> Outflow
      PayFeeYield _ -> Outflow
      Transfer _ _ -> Interflow 
      TransferBy _ _ _ -> Interflow 
      PoolInflow _ _ -> Inflow
      LiquidationProceeds _ -> Inflow
      LiquidationSupport _ -> Inflow
      LiquidationDraw -> Noneflow
      LiquidationRepay _ -> Outflow
      LiquidationSupportInt _ _ -> Noneflow
      BankInt -> Inflow
      SupportDraw -> Noneflow
      Empty -> Noneflow 
      Tag _ -> Noneflow
      UsingDS _ -> Noneflow
      SwapAccrue  -> Noneflow
      SwapInSettle -> Inflow
      SwapOutSettle -> Outflow
      PurchaseAsset _ _-> Outflow
      IssuanceProceeds _ -> Inflow
      TxnDirection _ -> Noneflow
      TxnComments cmts ->  --TODO the direction of combine txns
        let 
          directionList = getFlow <$> cmts 
        in 
          if Outflow `elem` directionList then
            Outflow
          else if any (Inflow ==) directionList then
            Inflow
          else
            Noneflow
      _ -> error ("Missing in GetFlow >> "++ show comment)

instance Ord Txn where
  compare :: Txn -> Txn -> Ordering
  compare (BondTxn d1 _ _ _ _ _ _ _ _ _) (BondTxn d2 _ _ _ _ _ _ _ _ _) = compare d1 d2
  compare (AccTxn d1 _ _ _ ) (AccTxn d2 _ _ _  ) = compare d1 d2

-- instance Eq Txn where
--  (BondTxn d1 _ _ _ _ _ _ _) == (BondTxn d2 _ _ _ _ _ _ _) = d1 == d2

instance TimeSeries Txn where 
  getDate (BondTxn t _ _ _ _ _ _ _ _ _ ) = t
  getDate (AccTxn t _ _ _ ) = t
  getDate (ExpTxn t _ _ _ _ ) = t
  getDate (SupportTxn t _ _ _ _ _ _) = t
  getDate (IrsTxn t _ _ _ _ _ _) = t
  getDate (EntryTxn t _ _ _) = t

class QueryByComment a where 
    queryStmt :: a -> TxnComment -> [Txn]
    queryStmtAsOf :: a -> Date -> TxnComment -> [Txn]
    queryStmtAsOf a d tc =  [ txn | txn <- queryStmt a tc, getDate txn <= d]
    queryTxnAmt :: a -> TxnComment -> Balance
    queryTxnAmt a tc = sum $ map getTxnAmt $ queryStmt a tc
    queryTxnAmtAsOf :: a -> Date -> TxnComment -> Balance 
    queryTxnAmtAsOf a d tc =  sum $ getTxnAmt <$> queryStmtAsOf a d tc
-- queryTxn :: [Txn] -> TxnComment -> [Txn]
-- queryTxn txns comment = [ txn | txn <- txns, getTxnComment txn == comment]
-- 
-- queryTxnAmt :: [Txn] -> TxnComment -> Balance
-- queryTxnAmt txns comment 
--   = sum $ geTxnAmt <$> queryTxn txns comment

$(deriveJSON defaultOptions ''Statement)
