{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module AssetClass.AssetCashflow
  (applyHaircut,patchPrepayPentalyFlow,getRecoveryLag,decreaseBorrowerNum)
  where

import qualified Data.Time as T
import qualified Cashflow as CF 
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

import Debug.Trace
import qualified Assumptions as A 
import GHC.Float.RealFracMethods (truncateFloatInteger)
debug = flip trace

applyHaircut :: Maybe A.ExtraStress -> CF.CashFlowFrame -> CF.CashFlowFrame
applyHaircut Nothing cf = cf 
applyHaircut (Just A.ExtraStress{A.poolHairCut = Nothing}) cf = cf
applyHaircut (Just A.ExtraStress{A.poolHairCut = Just haircuts}) (CF.CashFlowFrame txns)
  = CF.CashFlowFrame $ 
      (\txn -> foldr 
                 (\fn acc -> fn acc ) 
                 txn 
                 (applyHaircutTxn <$> haircuts) ) <$> txns
    where
      applyHaircutTxn (CollectedInterest,r) 
                      (CF.MortgageDelinqFlow d bal prin interest ppy delinq def recovery loss irate mbn mppn mst) 
        = CF.MortgageDelinqFlow d bal prin (mulBR interest (1-r)) ppy delinq def recovery loss irate mbn mppn mst
      applyHaircutTxn (CollectedPrincipal,r)
                      (CF.MortgageDelinqFlow d bal prin interest ppy delinq def recovery loss irate mbn mppn mst) 
        = CF.MortgageDelinqFlow d bal (mulBR prin (1-r)) interest ppy delinq def recovery loss irate mbn mppn mst
      applyHaircutTxn (CollectedRecoveries,r)
                      (CF.MortgageDelinqFlow d bal prin interest ppy delinq def recovery loss irate mbn mppn mst) 
        = CF.MortgageDelinqFlow d bal prin interest ppy delinq def (mulBR recovery (1-r)) loss irate mbn mppn mst
      applyHaircutTxn (CollectedPrepayment,r)
                      (CF.MortgageDelinqFlow d bal prin interest ppy delinq def recovery loss irate mbn mppn mst) 
        = CF.MortgageDelinqFlow d bal prin interest (mulBR ppy (1-r)) delinq def recovery loss irate mbn mppn mst
      applyHaircutTxn (CollectedPrepaymentPenalty,r)
                      (CF.MortgageDelinqFlow d bal prin interest ppy delinq def recovery loss irate mbn mppn mst) 
        = CF.MortgageDelinqFlow d bal prin interest ppy delinq def recovery loss irate mbn ((\x -> mulBR x (1-r) ) <$> mppn) mst
      
      applyHaircutTxn (CollectedInterest,r) 
                      (CF.MortgageFlow d bal prin interest ppy def recovery loss irate mbn mppn mst) 
        = CF.MortgageFlow d bal prin (mulBR interest (1-r)) ppy def recovery loss irate mbn mppn mst
      applyHaircutTxn (CollectedPrincipal,r)
                      (CF.MortgageFlow d bal prin interest ppy def recovery loss irate mbn mppn mst) 
        = CF.MortgageFlow d bal (mulBR prin (1-r)) interest ppy def recovery loss irate mbn mppn mst
      applyHaircutTxn (CollectedRecoveries,r)
                      (CF.MortgageFlow d bal prin interest ppy def recovery loss irate mbn mppn mst) 
        = CF.MortgageFlow d bal prin interest ppy def (mulBR recovery (1-r)) loss irate mbn mppn mst
      applyHaircutTxn (CollectedPrepayment,r)
                      (CF.MortgageFlow d bal prin interest ppy def recovery loss irate mbn mppn mst) 
        = CF.MortgageFlow d bal prin interest (mulBR ppy (1-r)) def recovery loss irate mbn mppn mst
      applyHaircutTxn (CollectedPrepaymentPenalty,r)
                      (CF.MortgageFlow d bal prin interest ppy def recovery loss irate mbn mppn mst)
        = CF.MortgageFlow d bal prin interest ppy def recovery loss irate mbn ((\x -> mulBR x (1-r) ) <$> mppn) mst
      
      applyHaircutTxn _ _ = error "Not implemented"
   

patchPrepayPentalyFlow :: (Int,Maybe PrepayPenaltyType) -> CF.CashFlowFrame -> CF.CashFlowFrame
patchPrepayPentalyFlow (ot,mPpyPen) mflow@(CF.CashFlowFrame trs) 
  = let 
      -- (MortgageOriginalInfo _ _ ot _ _ _ mPpyPen) = getOriginInfo m 
      (startDate,endDate) = CF.getDateRangeCashFlowFrame mflow
      prepaymentFlow = CF.mflowPrepayment <$> trs
      flowSize = CF.sizeCashFlowFrame mflow
    in 
      case mPpyPen of 
        Nothing -> mflow
        Just (ByTerm cutoff rate0 rate1) -> 
          let 
            rs = lastN flowSize $ replicate cutoff rate0 ++ replicate (ot-cutoff) rate1
          in 
            CF.CashFlowFrame $ CF.setPrepaymentPenaltyFlow (zipWith mulBR prepaymentFlow rs) trs
        Just (FixAmount amt mCutoff) -> 
          let 
            projFlow = case mCutoff of 
                         Nothing -> replicate flowSize amt
                         Just cutoff -> lastN flowSize $ replicate cutoff amt ++ replicate (ot-cutoff) 0 
            actFlow = [ if ppy > 0 then 
                          f
                        else
                          0
                        | (f,ppy) <- zip projFlow prepaymentFlow]
          in 
            CF.CashFlowFrame $ CF.setPrepaymentPenaltyFlow actFlow trs
        Just (FixPct r mCutoff) ->
          let 
            rs = case mCutoff of 
                   Nothing -> replicate flowSize r
                   Just cutoff -> lastN flowSize $ replicate cutoff r ++ replicate (ot-cutoff) 0
          in
            CF.CashFlowFrame $ CF.setPrepaymentPenaltyFlow (zipWith mulBR prepaymentFlow rs) trs
        Just (Sliding sr changeRate) -> 
          let 
            rs = lastN flowSize $ paddingDefault 0 [sr,(sr-changeRate)..0] ot
          in
            CF.CashFlowFrame $ CF.setPrepaymentPenaltyFlow (zipWith mulBR prepaymentFlow rs) trs
        Just (StepDown ps) ->
          let 
            rs = lastN flowSize $ paddingDefault 0 (concat [ replicate n r | (n,r) <- ps]) ot
          in 
            CF.CashFlowFrame $ CF.setPrepaymentPenaltyFlow (zipWith mulBR prepaymentFlow rs) trs

getRecoveryLag :: A.RecoveryAssumption -> Int
getRecoveryLag (A.Recovery (_,lag)) = lag 
getRecoveryLag (A.RecoveryTiming (_,rs)) = length rs


decreaseBorrowerNum :: Balance -> Balance -> Maybe BorrowerNum -> Maybe Int
decreaseBorrowerNum bb eb mBn 
  = case mBn of
      Nothing -> Nothing::(Maybe BorrowerNum)
      Just 0  -> Nothing::(Maybe BorrowerNum)
      Just bn -> Just $ round $ fromRational $ mulIR bn downRate::(Maybe BorrowerNum)
    where 
      downRate = if eb == 0 then 
                   0.0
                 else
                   divideBB eb bb