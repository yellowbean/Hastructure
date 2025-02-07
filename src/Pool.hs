{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Pool (Pool(..),aggPool
       ,getIssuanceField
       ,poolFutureCf,poolFutureTxn,poolIssuanceStat
       ,poolFutureScheduleCf
       ,poolBegStats,poolFutureCf2,calcLiquidationAmount,pricingPoolFlow
) where


import Lib (Period(..)
           ,Ts(..),periodRateFromAnnualRate,toDate
           ,getIntervalDays,zipWith9,mkTs,periodsBetween
           ,mkRateTs,daysBetween)

import qualified Cashflow as CF -- (Cashflow,Amount,Interests,Principals)
import qualified Assumptions as A
import qualified Analytics as AN
import qualified AssetClass.AssetBase as ACM 
import AssetClass.AssetCashflow
import Asset (Asset(..))
import qualified Data.Map as Map

import Data.Ratio
import Data.Aeson hiding (json)
import Language.Haskell.TH
import GHC.Generics
import Data.Aeson.TH
import Data.Aeson.Types
import Types hiding (Current)

import Data.Maybe
import Control.Lens hiding (element)
import Control.Lens.TH
import Assumptions (ApplyAssumptionType)

import Debug.Trace
import Util
import Cashflow (CashFlowFrame)
import qualified Stmt as CF
debug = flip trace


data Pool a = Pool {assets :: [a]                                           -- ^ a list of assets in the pool
                   ,futureCf :: Maybe CF.CashFlowFrame                      -- ^ projected cashflow from the assets in the pool
                   ,futureScheduleCf :: Maybe CF.CashFlowFrame              -- ^ projected un-stressed cashflow
                   ,asOfDate :: Date                                        -- ^ include cashflow after this date 
                   ,issuanceStat :: Maybe (Map.Map CutoffFields Balance)    -- ^ cutoff balance of pool
                   ,extendPeriods :: Maybe DatePattern                      -- ^ dates for extend pool collection
                   } deriving (Show, Generic, Ord, Eq)


poolFutureCf :: Asset a => Lens' (Pool a) (Maybe CF.CashFlowFrame)
poolFutureCf = lens getter setter 
  where 
    getter p = futureCf p
    setter p mNewCf = p {futureCf = mNewCf}

poolFutureCf2 :: Asset a => Lens' (Pool a) CF.CashFlowFrame
poolFutureCf2 = lens getter setter 
  where 
    getter p = fromMaybe (CF.CashFlowFrame (0,toDate "19000101",Nothing) []) $ futureCf p
    setter p newCf = p {futureCf = Just newCf}

poolFutureScheduleCf :: Asset a => Lens' (Pool a) (Maybe CF.CashFlowFrame)
poolFutureScheduleCf = lens getter setter
  where 
    getter p =  futureScheduleCf p
    setter p mNewCf = p {futureScheduleCf = mNewCf}

poolFutureTxn :: Asset a => Lens' (Pool a) [CF.TsRow]
poolFutureTxn = lens getter setter
  where 
    getter p = case futureCf p of
                Nothing -> []::[CF.TsRow]
                Just (CF.CashFlowFrame _ txns) -> txns
    setter p trs = case futureCf p of
                    Nothing -> p {futureCf = Just (CF.CashFlowFrame (0,toDate "19000101",Nothing) trs)}  --TODO fix this
                    Just (CF.CashFlowFrame st _) -> p {futureCf = Just (CF.CashFlowFrame st trs)}

poolIssuanceStat :: Asset a => Lens' (Pool a) (Map.Map CutoffFields Balance)
poolIssuanceStat = lens getter setter
  where 
    getter p =  fromMaybe Map.empty $ issuanceStat p
    setter p m = case issuanceStat p of
                    Nothing -> p {issuanceStat = Just m}
                    Just _ -> p {issuanceStat = Just m}


-- | get stats of pool 
getIssuanceField :: Pool a -> CutoffFields -> Balance
getIssuanceField p@Pool{issuanceStat = Just m} s
  = case Map.lookup s m of
      Just r -> r
      Nothing -> 0.0
getIssuanceField Pool{issuanceStat = Nothing} _ 
  = error "There is no pool stats"

poolBegStats :: Pool a -> (Balance,Balance,Balance,Balance,Balance,Balance)
poolBegStats p = 
  let 
    m = issuanceStat p
    stats = case m of
              Nothing -> (0,0,0,0,0,0)
              Just m -> (Map.findWithDefault 0 HistoryPrincipal m
                        ,Map.findWithDefault 0 HistoryPrepayment m
                        ,Map.findWithDefault 0 HistoryDelinquency m
                        ,Map.findWithDefault 0 HistoryDefaults m
                        ,Map.findWithDefault 0 HistoryRecoveries m
                        ,Map.findWithDefault 0 HistoryLoss m)
  in
    stats


-- | Aggregate all cashflow into a single cashflow frame
-- patch with pool level cumulative defaults/loss etc
aggPool :: Maybe (Map.Map CutoffFields Balance) -> [(CF.CashFlowFrame, Map.Map CutoffFields Balance)] -> (CF.CashFlowFrame, Map.Map CutoffFields Balance)
aggPool Nothing [] = (CF.CashFlowFrame (0,toDate "19000101",Nothing) [],Map.empty)
aggPool (Just m) [] = (CF.CashFlowFrame (0,toDate "19000101",Nothing) [], m)
aggPool mStat xs 
  = let
      cfs = fst <$> xs
      CF.CashFlowFrame st _txns = foldr1 CF.combine cfs 
      -- total stats with begin stats + stats from each cfs
      stats = foldr1 (Map.unionWith (+)) $  fromMaybe Map.empty mStat:(snd <$> xs)
      -- patch cumulative statistics
      cumulativeStatAtCutoff = case mStat of
                                 Nothing -> (0,0,0,0,0,0)
                                 Just m -> (Map.findWithDefault 0 HistoryPrincipal m
                                           ,Map.findWithDefault 0 HistoryPrepayment m
                                           ,Map.findWithDefault 0 HistoryDelinquency m
                                           ,Map.findWithDefault 0 HistoryDefaults m
                                           ,Map.findWithDefault 0 HistoryRecoveries m
                                           ,Map.findWithDefault 0 HistoryLoss m)
      -- (CumPrincipal,CumPrepay,CumDelinq,CumDefault,CumRecovery,CumLoss)
      txns = CF.patchCumulative cumulativeStatAtCutoff [] _txns 
      -- txns = CF.patchCumulativeAtInit (Just cumulativeStatAtCutoff) _txns 
    in
      case Map.lookup AccruedInterest =<< mStat of
        Nothing -> (CF.CashFlowFrame st txns, stats) 
        Just accruedIntAmt -> (CF.CashFlowFrame st (CF.clawbackInt accruedIntAmt txns), stats)


calcLiquidationAmount :: Asset a => PricingMethod -> Pool a -> Date -> Amount
calcLiquidationAmount (BalanceFactor currentFactor defaultFactor ) pool d 
  = case futureCf pool of 
      Nothing -> 0  -- `debug` ("No futureCF")
      Just _futureCf@(CF.CashFlowFrame _ trs) ->
        let 
          earlierTxns = cutBy Inc Past d trs
          currentCumulativeDefaultBal = sum $ map (\x -> CF.mflowDefault x - CF.mflowRecovery x - CF.mflowLoss x) earlierTxns
        in 
          case earlierTxns of 
            [] -> 0  -- `debug` ("No pool Inflow")
            _ -> (mulBR (view CF.tsRowBalance (last earlierTxns)) currentFactor) + (mulBR currentCumulativeDefaultBal defaultFactor)
            -- TODO need to check if missing last row


-- TODO: check futureCf is future CF or not, seems it is collected CF
-- | pricing via future scheduled cashflow( zero risk adjust)
-- | pricing via user define risk adjust cashflow( own assumption)
-- TODO: in revolving buy future schedule cashflow should be updated as well
calcLiquidationAmount (PV discountRate recoveryPct) pool d 
  = case futureCf pool of
      Nothing -> 0 
      Just (CF.CashFlowFrame _ trs) ->
          let 
            futureTxns = cutBy Inc Future d trs -- `debug` (" pv date"++show d++ " with rate"++show discountRate)
            earlierTxns = cutBy Exc Past d trs -- `debug` ("Total txn"++show trs)
            pvCf = sum $ map (\x -> AN.pv2  discountRate  d (CF.getDate x) (CF.tsTotalCash x)) futureTxns -- `debug` ("FutureTxns: "++show futureTxns)
            
            currentDefaulBal = sum $ map (\x -> CF.mflowDefault x - CF.mflowRecovery x - CF.mflowLoss x) earlierTxns
          in 
            
            pvCf + mulBR currentDefaulBal recoveryPct

-- ^ price a pool with collected cashflow and future cashflow
pricingPoolFlow :: Asset a =>  Date -> Pool a -> CashFlowFrame -> PricingMethod -> Amount
pricingPoolFlow d pool@Pool{ futureCf = mCollectedCf, issuanceStat = mStat } futureCfUncollected pm 
  = let 
      currentCumulativeDefaultBal = case mCollectedCf of 
                                      Nothing -> 0 
                                      Just collectedCf -> 
                                        let 
                                          collectedTxns = view CF.cashflowTxn collectedCf
                                        in
                                          if null collectedTxns then 
                                            0 
                                          else 
                                            let 
                                              lastTxn = last collectedTxns
                                            in 
                                              fromMaybe 0 (CF.tsCumDefaultBal lastTxn) - fromMaybe 0 (CF.tsCumRecoveriesBal lastTxn) - fromMaybe 0 (CF.tsCumLossBal lastTxn)
      currentPerformingBal = case mStat of
              Nothing -> 0
              Just stat -> Map.findWithDefault 0 RuntimeCurrentPoolBalance stat

    in 
      case pm of
        BalanceFactor currentFactor defaultFactor -> 
          mulBR currentPerformingBal currentFactor + mulBR currentCumulativeDefaultBal defaultFactor

        PvRate discountRate ->
          let 
            futureTxn = view CF.cashflowTxn futureCfUncollected -- `debug` ("PV with cf"++ show d ++ ">>"++show futureCfUncollected)
            futureCfCash = CF.tsTotalCash <$> futureTxn
            futureDates = getDate <$> futureTxn
          in 
            AN.pv21 discountRate d futureDates futureCfCash

        


$(deriveJSON defaultOptions ''Pool)
