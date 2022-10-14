{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}

module Stmt
  (Statement(..),Txn(..)
   ,extractTxns,groupTxns,getTxns,getTxnComment,getTxnDate,getTxnAmt,toDate,getTxnPrincipal,getTxnAsOf,getTxnBalance
   ,queryStmtAmt,appendStmt,combineTxn,sliceStmt,getTxnBegBalance
   ,sliceTxns,TxnComment(..)
  )
  where

import Lib (Date,Balance,Amount,Interest,Principal,IRate,Cash,Comment
            ,toDate)
import Util (mulBR)
import Types 
import Language.Haskell.TH
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Aeson hiding (json)
-- import Text.Regex.TDFA
import Text.Regex.Base
import Text.Regex.PCRE
import Data.Fixed
import Data.List
import Data.Maybe
import GHC.Generics
-- import Text.RawString.QQ
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Map as M

data TxnComment = PayInt [BondName] (Maybe Balance)
                | PayYield BondName
                | PayPrin [BondName] (Maybe Balance)
                | PayFee FeeName Balance
                | SeqPayFee [FeeName] 
                | PayFeeYield FeeName
                | Transfer AccName AccName 
                | PoolInflow PoolSource
                | LiquidationProceeds Balance
                | BankInt
                | Empty 
                | Tag String
                | TxnComments [TxnComment]
                deriving (Eq,Show,Generic)

instance ToJSON TxnComment where 
  toJSON (PayInt bns mb) = String $ T.pack $ "<PayInt:"++ show bns ++ ","++ show mb ++ ">"
  toJSON (PayYield bn) = String $ T.pack $ "<PayYield:"++ bn++">"
  toJSON (PayPrin bns mb) =  String $ T.pack $ "<PayPrin:"++ show bns ++ ","++ show mb ++ ">"
  toJSON (PayFee fn b) =  String $ T.pack $ "<PayFee:" ++ fn ++ ","++ show b ++ ">"
  toJSON (SeqPayFee fns) =  String $ T.pack $ "<SeqPayFee:"++show fns++">"
  toJSON (PayFeeYield fn) =  String $ T.pack $ "<PayFeeYield:"++ fn++">"
  toJSON (Transfer an1 an2) =  String $ T.pack $ "<Transfer:"++ an1 ++","++ an2++">"
  toJSON (PoolInflow ps) =  String $ T.pack $ "<PoolInflow:"++ show ps++">"
  toJSON (LiquidationProceeds b) =  String $ T.pack $ "<Liquidation:"++show b++">"
  toJSON BankInt =  String $ T.pack $ "<BankInterest:>"
  toJSON Empty =  String $ T.pack $ "" 
  toJSON (TxnComments tcms) = Array $ V.fromList $ map toJSON tcms
 
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
      sr = ((T.unpack t) =~ pat)::[[String]]
      tagName =  head sr!!1::String
      contents = head sr!!2::String
                                       

data Txn = BondTxn Date Balance Interest Principal IRate Cash TxnComment
          | AccTxn Date Balance Amount TxnComment
          | ExpTxn Date Balance Amount Balance TxnComment
          deriving (Show)

getTxnComment :: Txn -> TxnComment
getTxnComment (BondTxn _ _ _ _ _ _ t ) = t
getTxnComment (AccTxn _ _ _ t ) = t
getTxnComment (ExpTxn _ _ _ _ t ) = t

getTxnDate :: Txn -> Date
getTxnDate (BondTxn t _ _ _ _ _ _ ) = t
getTxnDate (AccTxn t _ _ _ ) = t
getTxnDate (ExpTxn t _ _ _ _ ) = t

getTxnBalance :: Txn -> Balance
getTxnBalance (BondTxn _ t _ _ _ _ _ ) = t
getTxnBalance (AccTxn _ t _ _ ) = t
getTxnBalance (ExpTxn _ t _ _ _ ) = t

getTxnBegBalance :: Txn -> Balance
getTxnBegBalance (BondTxn _ t _ p _ _ _ ) = t + p
getTxnBegBalance (AccTxn _ b a _ ) = b - a

getTxnPrincipal :: Txn -> Balance
getTxnPrincipal (BondTxn _ _ _ t _ _ _ ) = t

getTxnAmt :: Txn -> Balance
getTxnAmt (BondTxn _ _ _ _ _ t _ ) = t
getTxnAmt (AccTxn _ _ t _ ) = t
getTxnAmt (ExpTxn _ _ t _ _ ) = t

getTxnAsOf :: [Txn] -> Date -> Maybe Txn
getTxnAsOf txns d = find (\x -> (getTxnDate x) <= d) $ reverse txns

emptyTxn :: Txn -> Date -> Txn
emptyTxn (BondTxn _ _ _ _ _ _ _ ) d = (BondTxn d 0 0 0 0 0 Empty )
emptyTxn (AccTxn _ _ _ _  ) d = (AccTxn d 0 0 Empty )
emptyTxn (ExpTxn _ _ _ _ _ ) d = (ExpTxn d 0 0 0 Empty )

getTxnByDate :: [Txn] -> Date -> Maybe Txn
getTxnByDate ts d = find (\x -> (d == (getTxnDate x))) ts

queryStmtAmt :: Maybe Statement -> String -> Balance
queryStmtAmt (Just (Statement txns)) q =
  let
    resultTxns =  txns --TODO BUG!! -- filter (\txn -> (getTxnComment txn) =~ q)  txns
  in
    abs $ foldr (\x a -> (getTxnAmt x) + a) 0 resultTxns -- `debug` ("DEBUG Query"++show(resultTxns))

queryStmtAmt Nothing _ = 0

sliceStmt :: Maybe Statement -> Date -> Date -> Maybe Statement
sliceStmt Nothing sd ed  = Nothing
sliceStmt (Just (Statement txns)) sd ed 
  = Just $ Statement $ filter 
                  (\x -> ((getTxnDate x) >= sd) && ((getTxnDate x) <= ed)) txns 

sliceTxns :: [Txn] -> Date -> Date -> [Txn]
sliceTxns txns sd ed 
  = filter (\x -> (getTxnDate x)>=sd && (getTxnDate x)<ed) txns

data Statement = Statement [Txn]
        deriving (Show,Eq)

--instance TsList Statement where
--  subByRange s@(Statement txns) Nothing Nothing = s
--  subByRange s@(Statement txns) Nothing (Just ed) =
--  subByRange s@(Statement txns) (Just sd) Nothing =
--  subByRange s@(Statement txns) (Just sd) (Just ed) =

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
  = M.fromAscListWith (++) $ [(getTxnDate txn,[txn]) | txn <- txns]

combineTxn :: Txn -> Txn -> Txn
combineTxn (BondTxn d1 b1 i1 p1 r1 c1 m1) (BondTxn d2 b2 i2 p2 r2 c2 m2)
    = BondTxn d1 (min b1 b2) (i1 + i2) (p1 + p2) (r1+r2) (c1+c2) (TxnComments [m1,m2])

instance Ord Txn where
  compare (BondTxn d1 _ _ _ _ _ _ ) (BondTxn d2 _ _ _ _ _ _ )
    = compare d1 d2

instance Eq Txn where
  (BondTxn d1 _ _ _ _ _ _ ) == (BondTxn d2 _ _ _ _ _ _ )
    = d1 == d2

$(deriveJSON defaultOptions ''Txn)
$(deriveJSON defaultOptions ''Statement)
-- $(deriveJSON defaultOptions ''TxnComment)
