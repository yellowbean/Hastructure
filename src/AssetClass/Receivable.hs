{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module AssetClass.Receivable
  ()
  where

import qualified Data.Time as T
import qualified Cashflow as CF 
import qualified Assumptions as A
import Asset as Ast
import Types
import Lib
import Util
import DateUtil
import InterestRate as IR

import qualified Data.Map as Map
import Data.List
import Data.Ratio
import Data.Maybe
import GHC.Generics
import Data.Aeson hiding (json)
import Language.Haskell.TH
import Data.Aeson.TH
import Data.Aeson.Types

import AssetClass.AssetBase
import AssetClass.AssetCashflow
import Debug.Trace
import Assumptions (AssetPerfAssumption(MortgageAssump))
import GHC.Float.RealFracMethods (truncateFloatInteger)
import Cashflow (extendTxns)
import Liability (backoutDueIntByYield)
import qualified Asset as A


debug = flip trace


calcDueFactorFee :: Receivable -> Date -> Balance
calcDueFactorFee r@(Invoice (ReceivableInfo sd ob oa dd ft) st) asOfDay
  = case ft of
      Nothing -> 0
      Just (FixedFee b) -> b 
      Just (FixedRateFee r) -> mulBR ob r
      Just (FactorFee r daysInPeriod rnd) -> 
        let 
            periods = case rnd of 
                        Up ->  ceiling ((fromIntegral (daysBetween sd dd)) / (fromIntegral daysInPeriod)) :: Int 
                        Down -> floor ((fromIntegral (daysBetween sd dd)) / (fromIntegral daysInPeriod)) :: Int  
        in 
            fromRational $ (toRational periods) * toRational (mulBR ob r) 
      Just (AdvanceFee r) -> mulBR oa (r  * (yearCountFraction DC_ACT_365F sd dd))
      Just (CompoundFee fs) -> 
        let 
            newReceivables = [ Invoice (ReceivableInfo sd ob oa dd (Just newFeeType)) st  | newFeeType <- fs] 
        in 
            sum $ (`calcDueFactorFee` asOfDay) <$> newReceivables


instance Asset Receivable where 

  getPaymentDates r@(Invoice (ReceivableInfo sd ob oa dd ft) st) _ = [dd]

  calcCashflow r@(Invoice (ReceivableInfo sd ob oa dd ft) st) asOfDay _ 
    = CF.CashFlowFrame $ initTxn ++ cutBy Inc Future asOfDay txns
    where
      payDate = dd
      feeDue = calcDueFactorFee r payDate
      initTxn = [CF.ReceivableFlow sd ob 0 0 0 0 0 0 Nothing]

      feePaid = min ob feeDue
      principal = max 0 $ ob - feeDue

      txns = [CF.ReceivableFlow payDate 0 0 principal feePaid 0 0 0 Nothing]

  getCurrentBal r@(Invoice (ReceivableInfo sd ob oa dd ft) st) = ob

  isDefaulted r@(Invoice (ReceivableInfo sd ob oa dd ft) Current) = False
  isDefaulted r@(Invoice (ReceivableInfo sd ob oa dd ft) _) = True

  getOriginDate r@(Invoice (ReceivableInfo sd ob oa dd ft) st) = sd

  getRemainTerms r@(Invoice (ReceivableInfo sd ob oa dd ft) st) = 1

  updateOriginDate r@(Invoice (ReceivableInfo sd ob oa dd ft) st) newDate 
    = Invoice (ReceivableInfo newDate ob oa dd ft) st

  projCashflow r@(Invoice (ReceivableInfo sd ob oa dd ft) Current) 
               asOfDay
               massump@(A.ReceivableAssump (Just A.DefaultAtEnd) amr ams, _ , _)
               mRates
    = (CF.CashFlowFrame (initTxn++futureTxns), historyM)
    where
      payDate = dd
      feeDue = calcDueFactorFee r payDate
      initTxn = [CF.ReceivableFlow sd ob 0 0 0 0 0 0 Nothing]
      
      txns = [CF.ReceivableFlow payDate 0 0 0 0 ob 0 ob Nothing]
      (futureTxns,historyM)= CF.cutoffTrs asOfDay (patchLossRecovery txns amr)

  projCashflow r@(Invoice (ReceivableInfo sd ob oa dd ft) Current) 
               asOfDay
               massump@(A.ReceivableAssump amd amr ams, _ , _)
               mRates
    = (CF.CashFlowFrame (initTxn++futureTxns), historyM)
    where
      payDate = dd
      feeDue = calcDueFactorFee r payDate
      initTxn = [CF.ReceivableFlow sd ob 0 0 0 0 0 0 Nothing]
      
      defaultRates = A.buildDefaultRates (sd:[dd]) amd
      defaultAmt = mulBR ob (head defaultRates)
      afterDefaultBal =  ob - defaultAmt
      afterDefaultFee =  mulBR feeDue (1 - (head defaultRates))

      feePaid = min afterDefaultBal afterDefaultFee
      principal = max 0 $ afterDefaultBal - feePaid
      
      txns = [CF.ReceivableFlow payDate 0 0 principal feePaid defaultAmt 0 defaultAmt Nothing]
      (futureTxns,historyM)= CF.cutoffTrs asOfDay (patchLossRecovery txns amr)