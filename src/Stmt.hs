{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}

module Stmt
  (Statement(..),Txn(..)
   ,extractTxns,groupTxns,getTxns,getTxnComment,getTxnAmt,toDate,getTxnPrincipal,getTxnAsOf,getTxnBalance
   ,appendStmt,combineTxn,sliceStmt,getTxnBegBalance,getDate,getDates
   ,sliceTxns,TxnComment(..),QueryByComment(..)
   ,weightAvgBalanceByDates,weightAvgBalance
   ,getFlow,FlowDirection(..), aggByTxnComment
  )
  where

import Lib (toDate,getIntervalFactors)
import Util (mulBR)
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

import Debug.Trace
debug = flip trace

data TxnComment = PayInt [BondName]
                | PayYield BondName 
                | PayPrin [BondName] 
                | PayFee FeeName
                | SeqPayFee [FeeName] 
                | PayFeeYield FeeName
                | Transfer AccName AccName 
                | PoolInflow PoolSource
                | LiquidationProceeds
                | LiquidationSupport String
                | LiquidationDraw
                | LiquidationRepay
                | LiquidationSupportInt Balance Balance
                | BankInt
                | Empty 
                | Tag String
                | UsingDS DealStats
                | UsingFormula FormulaType
                | SwapAccure
                | SwapInSettle
                | SwapOutSettle
                | PurchaseAsset
                | TxnComments [TxnComment]
                deriving (Eq, Show, Ord , Generic)

instance ToJSON TxnComment where 
  toJSON (PayInt bns ) = String $ T.pack $ "<PayInt:"++ show bns ++ ">"
  toJSON (PayYield bn ) = String $ T.pack $ "<PayYield:"++ show bn ++">"
  toJSON (PayPrin bns ) =  String $ T.pack $ "<PayPrin:"++ show bns ++ ">"
  toJSON (PayFee fn ) =  String $ T.pack $ "<PayFee:" ++ fn ++ ">"
  toJSON (SeqPayFee fns) =  String $ T.pack $ "<SeqPayFee:"++show fns++">"
  toJSON (PayFeeYield fn) =  String $ T.pack $ "<PayFeeYield:"++ fn++">"
  toJSON (Transfer an1 an2) =  String $ T.pack $ "<Transfer:"++ an1 ++","++ an2++">"
  toJSON (PoolInflow ps) =  String $ T.pack $ "<PoolInflow:"++ show ps++">"
  toJSON LiquidationProceeds =  String $ T.pack $ "<Liquidation>"
  toJSON (UsingDS ds) =  String $ T.pack $ "<DS:"++ show ds++">"
  toJSON (UsingFormula fm) =  String $ T.pack $ "<Formula:"++ show fm++">"
  toJSON BankInt =  String $ T.pack $ "<BankInterest:>"
  toJSON Empty =  String $ T.pack $ "" 
  toJSON (TxnComments tcms) = Array $ V.fromList $ map toJSON tcms
  toJSON (LiquidationSupport source) = String $ T.pack $ "<Support:"++source++">"
  toJSON (LiquidationSupportInt b1 b2) =  String $ T.pack $ "<SupportExp:(Int:"++ show b1 ++ ",Fee:" ++ show b2 ++")>"
  toJSON LiquidationDraw = String $ T.pack $ "<Draw:>"
  toJSON LiquidationRepay = String $ T.pack $ "<Repay:>"
  toJSON SwapAccure = String $ T.pack $ "<Accure:>"
  toJSON SwapInSettle = String $ T.pack $ "<SettleIn:>"
  toJSON SwapOutSettle = String $ T.pack $ "<SettleOut:>"

-- instance FromJSON TxnComment

instance FromJSON TxnComment where
    parseJSON = withText "Empty" parseTxn

parseTxn :: T.Text -> Parser TxnComment 
parseTxn "" = return Empty 
parseTxn "<BankInt>" = return BankInt
parseTxn t = case tagName of 
  "Transfer" -> let 
                  sv = T.splitOn (T.pack ",") $ T.pack contents
                in 
                  return $ Transfer (T.unpack (head sv)) (T.unpack (sv!!1))
  where 
      pat = "<(\\S+):(\\S+)>"::String
      sr = (T.unpack t =~ pat)::[[String]]
      tagName =  head sr!!1::String
      contents = head sr!!2::String

--instance Eq TxnComment where 
--   PayInt _ == PayInt _  = True
--   PayYield _ == PayYield _ = True
--   PayPrin _ _ == PayPrin _ _ = True 
--   PayFee _ _ == PayFee _ _ = True
--   SeqPayFee _ == SeqPayFee _ = True
--   PayFeeYield _ == PayFeeYield _ = True
--   Transfer _ _ == Transfer _ _ = True
--   PoolInflow _ == PoolInflow _ = True 
--   LiquidationProceeds _ == LiquidationProceeds _ = True
--   LiquidationSupport _ == LiquidationSupport _ = True 
--   LiquidationSupportInt _ _ == LiquidationSupportInt _ _ = True
--   Tag _ == Tag _ = True
--   UsingDS _ == UsingDS _ = True
--   UsingFormula _ == UsingFormula _ = True 
--   TxnComments as == TxnComments bs = 
--     let 
--      aset = Set.fromList as
--      bset = Set.fromList bs
--     in 
--      aset == bset
--   LiquidationDraw == LiquidationDraw = True
--   LiquidationRepay == LiquidationRepay = True
--   BankInt == BankInt = True
--   Empty == Empty = True
--   SwapAccure == SwapAccure = True
--   SwapInSettle == SwapInSettle = True
--   SwapOutSettle == SwapOutSettle = True

type DueInt = Maybe Balance
type DuePremium =  Maybe Balance

data Txn = BondTxn Date Balance Interest Principal IRate Cash TxnComment
         | AccTxn Date Balance Amount TxnComment
         | ExpTxn Date Balance Amount Balance TxnComment
         | SupportTxn Date (Maybe Balance) Amount Balance DueInt DuePremium TxnComment
         | IrsTxn Date Balance Amount IRate IRate Balance TxnComment
         deriving (Show, Generic)

aggByTxnComment :: [Txn] -> M.Map TxnComment [Txn] -> M.Map TxnComment Balance
aggByTxnComment [] m = M.map sumTxn m 
aggByTxnComment (txn:txns) m 
  | M.member c m = aggByTxnComment txns (M.adjust ([txn] ++) c m)
  | otherwise = aggByTxnComment txns (M.insert c [txn] m)
  where 
    c = getTxnComment txn

sumTxn :: [Txn] -> Balance
sumTxn txns = sum $ getTxnAmt <$> txns

getTxnComment :: Txn -> TxnComment
getTxnComment (BondTxn _ _ _ _ _ _ t ) = t
getTxnComment (AccTxn _ _ _ t ) = t
getTxnComment (ExpTxn _ _ _ _ t ) = t
getTxnComment (SupportTxn _ _ _ _ _ _ t ) = t
getTxnComment (IrsTxn _ _ _ _ _ _ t ) = t


getTxnBalance :: Txn -> Balance
getTxnBalance (BondTxn _ t _ _ _ _ _ ) = t
getTxnBalance (AccTxn _ t _ _ ) = t
getTxnBalance (ExpTxn _ t _ _ _ ) = t
getTxnBalance (SupportTxn _ _ _ t _ _ _ ) = t -- credit offered

getTxnBegBalance :: Txn -> Balance
getTxnBegBalance (BondTxn _ t _ p _ _ _ ) = t + p
getTxnBegBalance (AccTxn _ b a _ ) = b - a
getTxnBegBalance (SupportTxn _ _ a b _ _ _) = b - a

getTxnPrincipal :: Txn -> Balance
getTxnPrincipal (BondTxn _ _ _ t _ _ _ ) = t

getTxnAmt :: Txn -> Balance
getTxnAmt (BondTxn _ _ _ _ _ t _ ) = t
getTxnAmt (AccTxn _ _ t _ ) = t
getTxnAmt (ExpTxn _ _ t _ _ ) = t
getTxnAmt (SupportTxn _ _ t _ _ _ _) = t
getTxnAmt (IrsTxn _ _ t _ _ _ _ ) = t

getTxnAsOf :: [Txn] -> Date -> Maybe Txn
getTxnAsOf txns d = find (\x -> getDate x <= d) $ reverse txns

emptyTxn :: Txn -> Date -> Txn
emptyTxn (BondTxn _ _ _ _ _ _ _ ) d = (BondTxn d 0 0 0 0 0 Empty )
emptyTxn (AccTxn _ _ _ _  ) d = (AccTxn d 0 0 Empty )
emptyTxn (ExpTxn _ _ _ _ _ ) d = (ExpTxn d 0 0 0 Empty )
emptyTxn (SupportTxn _ _ _ _ _ _ _) d = (SupportTxn d Nothing 0 0 Nothing Nothing Empty)
emptyTxn (IrsTxn _ _ _ _ _ _ _) d = IrsTxn d 0 0 0 0 0 Empty

getTxnByDate :: [Txn] -> Date -> Maybe Txn
getTxnByDate ts d = find (\x -> (d == (getDate x))) ts

sliceStmt :: Maybe Statement -> Date -> Date -> Maybe Statement
sliceStmt Nothing sd ed  = Nothing
sliceStmt (Just (Statement txns)) sd ed 
  = Just $ Statement $ filter 
                  (\x -> ((getDate x) >= sd) && ((getDate x) <= ed)) txns 

sliceTxns :: [Txn] -> Date -> Date -> [Txn]
sliceTxns txns sd ed 
  = filter (\x -> (getDate x)>=sd && (getDate x)<ed) txns

weightAvgBalanceByDates :: [Date] -> [Txn] -> [Balance]
weightAvgBalanceByDates ds txns 
  = map (\(_sd,_ed) -> weightAvgBalance _sd _ed txns) intervals -- `debug` ("interval"++ show intervals++ show txns)
  where 
      intervals = zip (init ds) (tail ds) 

weightAvgBalance :: Date -> Date -> [Txn] -> Balance -- txn has to be between sd & ed
weightAvgBalance sd ed txns 
  = sum $ zipWith mulBR bals dsFactor -- `debug` ("WavgBalace "++show bals++show dsFactor)
  where 
      _txns = sliceTxns txns sd ed
      bals = (map getTxnBegBalance _txns) ++ [getTxnBalance (last _txns)]
      ds = [sd]++(map getDate _txns)++[ed] 
      dsFactor = getIntervalFactors ds  -- `debug` ("DS>>>"++show ds)

class QueryByComment a where 
    queryStmt :: a -> TxnComment -> [Txn]
    queryTxnAmt :: a -> TxnComment -> Balance


data Statement = Statement [Txn]
        deriving (Show,Eq,Generic)

appendStmt :: Maybe Statement -> Txn -> Statement
appendStmt (Just stmt@(Statement txns)) txn = Statement (txns++[txn])
appendStmt Nothing txn = Statement [txn]

extractTxns :: [Txn] -> [Statement] -> [Txn]
extractTxns rs ((Statement _txns):stmts) = extractTxns (rs++_txns) stmts
extractTxns rs [] = rs

getTxns :: Maybe Statement -> [Txn]
getTxns Nothing = []
getTxns (Just (Statement txn)) = txn

groupTxns :: Maybe Statement -> M.Map Date [Txn]
groupTxns (Just (Statement txns))
  = M.fromAscListWith (++) $ [(getDate txn,[txn]) | txn <- txns]

combineTxn :: Txn -> Txn -> Txn
combineTxn (BondTxn d1 b1 i1 p1 r1 c1 m1) (BondTxn d2 b2 i2 p2 r2 c2 m2)
    = BondTxn d1 (min b1 b2) (i1 + i2) (p1 + p2) (r1+r2) (c1+c2) (TxnComments [m1,m2])

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
      PayFee _ -> Outflow
      SeqPayFee _ -> Outflow
      PayFeeYield _ -> Outflow
      Transfer _ _ -> Interflow 
      PoolInflow _ -> Inflow
      LiquidationProceeds -> Inflow
      LiquidationSupport _ -> Inflow
      LiquidationDraw -> Noneflow
      LiquidationRepay -> Outflow
      LiquidationSupportInt _ _ -> Noneflow
      BankInt -> Inflow
      Empty -> Noneflow 
      Tag _ -> Noneflow
      UsingDS _ -> Noneflow
      UsingFormula _ -> Noneflow
      SwapAccure  -> Noneflow
      SwapInSettle -> Inflow
      SwapOutSettle -> Outflow
      TxnComments cmts -> 
        let 
          directionList = getFlow <$> cmts 
        in 
          if any (Outflow ==) directionList then
            Outflow
          else if any (Inflow ==) directionList then
            Inflow
          else
            Noneflow

instance Ord Txn where
  compare (BondTxn d1 _ _ _ _ _ _ ) (BondTxn d2 _ _ _ _ _ _ ) = compare d1 d2
  compare (AccTxn d1 _ _ _ ) (AccTxn d2 _ _ _  ) = compare d1 d2

instance Eq Txn where
  (BondTxn d1 _ _ _ _ _ _ ) == (BondTxn d2 _ _ _ _ _ _ )
    = d1 == d2

instance TimeSeries Txn where 
  getDate (BondTxn t _ _ _ _ _ _ ) = t
  getDate (AccTxn t _ _ _ ) = t
  getDate (ExpTxn t _ _ _ _ ) = t
  getDate (SupportTxn t _ _ _ _ _ _) = t
  getDate (IrsTxn t _ _ _ _ _ _) = t


$(deriveJSON defaultOptions ''Txn)
$(deriveJSON defaultOptions ''Statement)
