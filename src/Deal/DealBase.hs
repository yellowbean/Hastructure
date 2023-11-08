{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Deal.DealBase (TestDeal(..),SPV(..),dealBonds,dealFees,dealAccounts) 
  where
import qualified Accounts as A
import qualified Ledger as LD
import qualified Asset as P
import qualified Expense as F
import qualified Liability as L
import qualified CreditEnhancement as CE
import qualified Hedge as HE
import qualified Waterfall as W
import qualified Cashflow as CF
import qualified Assumptions as AP
import qualified AssetClass.AssetBase as ACM
import qualified Call as C
import qualified InterestRate as IR
import Stmt
import Lib
import Util
import Types
import Revolving
import Triggers

import qualified Data.Map as Map
import qualified Data.Time as T
import qualified Data.Set as S
import Data.List
import Data.Fixed
import Data.Maybe
import Data.Aeson hiding (json)
import qualified Data.Aeson.Encode.Pretty as Pretty
import Language.Haskell.TH
import Data.Aeson.TH
import Data.Aeson.Types
import GHC.Generics
import Control.Lens hiding (element)
import Control.Lens.TH


class SPV a where
  getBondByName :: a -> Maybe [String] -> Map.Map String L.Bond
  getBondBegBal :: a -> String -> Balance
  getBondStmtByName :: a -> Maybe [String] -> Map.Map String (Maybe Statement)
  getFeeByName :: a -> Maybe [String] -> Map.Map String F.Fee
  getAccountByName :: a -> Maybe [String] -> Map.Map String A.Account
  
data TestDeal a = TestDeal { name :: String
                             ,status :: DealStatus
                             ,dates :: DateDesp
                             ,accounts :: Map.Map AccountName A.Account
                             ,fees :: Map.Map FeeName F.Fee
                             ,bonds :: Map.Map BondName L.Bond
                             ,pool ::  P.Pool a 
                             ,waterfall :: Map.Map W.ActionWhen W.DistributionSeq
                             ,collects :: [W.CollectionRule]
                             ,call :: Maybe [C.CallOption]
                             ,liqProvider :: Maybe (Map.Map String CE.LiqFacility)
                             ,rateSwap :: Maybe (Map.Map String HE.RateSwap)
                             ,currencySwap :: Maybe (Map.Map String HE.CurrencySwap)
                             ,custom:: Maybe (Map.Map String CustomDataType)
                             ,triggers :: Maybe (Map.Map DealCycle (Map.Map String Trigger))
                             ,overrides :: Maybe [OverrideType]
                             ,ledgers :: Maybe (Map.Map String LD.Ledger)
                           } deriving (Show,Generic)

instance SPV (TestDeal a) where
  getBondByName t bns
    = case bns of
         Nothing -> bonds t
         Just _bns -> Map.filterWithKey (\k _ -> S.member k (S.fromList _bns)) (bonds t)

  getBondStmtByName t bns
    = Map.map L.bndStmt bndsM
      where
      bndsM = Map.map L.consolStmt $ getBondByName t bns

  getBondBegBal t bn 
    = case L.bndStmt b of
        Just (Statement stmts) -> getTxnBegBalance $ head stmts -- `debug` ("Getting beg bal"++bn++"Last smt"++show (head stmts))
        Nothing -> L.bndBalance b  -- `debug` ("Getting beg bal nothing"++bn)
        where
            b = bonds t Map.! bn

  getFeeByName t fns
    = case fns of
         Nothing -> fees t
         Just _fns -> Map.filterWithKey (\k _ ->  S.member k (S.fromList _fns)) (fees t)
  
  getAccountByName t ans
    = case ans of
         Nothing -> accounts t
         Just _ans -> Map.filterWithKey (\k _ ->  S.member k (S.fromList _ans)) (accounts t)

dealBonds :: P.Asset a => Lens' (TestDeal a) (Map.Map BondName L.Bond)
dealBonds = lens getter setter 
  where 
    getter d = bonds d 
    setter d newBndMap = d {bonds = newBndMap}

dealAccounts :: P.Asset a => Lens' (TestDeal a) (Map.Map AccountName A.Account) 
dealAccounts = lens getter setter 
  where 
    getter d = accounts d 
    setter d newAccMap = d {accounts = newAccMap}

dealFees :: P.Asset a => Lens' (TestDeal a) (Map.Map FeeName F.Fee) 
dealFees = lens getter setter 
  where 
    getter d = fees d 
    setter d newFeeMap = d {fees = newFeeMap}





$(deriveJSON defaultOptions ''TestDeal)
