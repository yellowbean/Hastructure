{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Pool (Pool(..),aggPool
       ,getIssuanceField
       ,poolFutureCf,poolFutureTxn,poolIssuanceStat
       ,poolFutureScheduleCf,futureCf,futureScheduleCf
) where


import Lib (Period(..)
           ,Ts(..),periodRateFromAnnualRate,toDate
           ,getIntervalDays,zipWith9,mkTs,periodsBetween
           ,mkRateTs,daysBetween)

import qualified Cashflow as CF -- (Cashflow,Amount,Interests,Principals)
import qualified Assumptions as A
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
                 Just (CF.CashFlowFrame txns) -> txns
    setter p trs = case futureCf p of
                     Nothing -> p {futureCf = Just (CF.CashFlowFrame trs)}
                     Just (CF.CashFlowFrame _) -> p {futureCf = Just (CF.CashFlowFrame trs)}

poolIssuanceStat :: Asset a => Lens' (Pool a) (Map.Map CutoffFields Balance)
poolIssuanceStat = lens getter setter
  where 
    getter p =  fromMaybe Map.empty $ issuanceStat p
    setter p m = case issuanceStat p of
                    Nothing -> p {issuanceStat = Just m}
                    Just m -> p {issuanceStat = Just m}

-- | get stats of pool 
getIssuanceField :: Pool a -> CutoffFields -> Balance
getIssuanceField p@Pool{issuanceStat = Just m} s
  = case Map.lookup s m of
      Just r -> r
      Nothing -> error ("Failed to lookup "++show s++" in stats "++show m)
getIssuanceField Pool{issuanceStat = Nothing} _ 
  = error "There is no pool stats"

-- | Aggregate all cashflow into a single cashflow frame
-- patch with pool level cumulative defaults/loss etc
aggPool :: Maybe (Map.Map CutoffFields Balance) -> [(CF.CashFlowFrame, Map.Map CutoffFields Balance)] -> (CF.CashFlowFrame, Map.Map CutoffFields Balance)
aggPool Nothing [] = (CF.CashFlowFrame [],Map.empty)
aggPool (Just m) [] = (CF.CashFlowFrame [], m)
aggPool mStat xs 
  = let
      cfs = fst <$> xs
      CF.CashFlowFrame _txns = foldr1 CF.combine cfs 
      stats = foldr1 (Map.unionWith (+)) $ snd <$> xs
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
      -- txns = CF.patchCumulative cumulativeStatAtCutoff [] _txns 
      txns = CF.patchCumulativeAtInit (Just cumulativeStatAtCutoff) _txns 
    in
      case Map.lookup AccruedInterest =<< mStat of
        Nothing -> (CF.CashFlowFrame txns, stats) 
        Just accruedIntAmt -> (CF.CashFlowFrame (CF.clawbackInt accruedIntAmt txns), stats)

 

$(deriveJSON defaultOptions ''Pool)
