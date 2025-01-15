{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module AssetClass.ProjectedCashFlow  
  (ProjectedCashflow(..))
  where

import qualified Data.Time as T
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

import qualified Cashflow as CF

import AssetClass.AssetBase
import AssetClass.AssetCashflow

import Cashflow (extendTxns,TsRow(..))

import Debug.Trace
import Control.Lens hiding (element,Index)
import Control.Lens.TH
debug = flip trace


projectScheduleFlow :: [CF.TsRow] -> Rate -> Balance -> [CF.TsRow] -> [DefaultRate] -> [PrepaymentRate] -> [Amount] -> [Amount] -> (Int, Rate) -> [CF.TsRow]
projectScheduleFlow trs _ last_bal [] _ _ [] [] (_,_) = trs 
projectScheduleFlow trs bal_factor last_bal (flow:flows) (defRate:defRates) (ppyRate:ppyRates) recV lossV (recoveryLag,recoveryRate)
  = projectScheduleFlow (trs++[tr]) surviveRate endBal flows defRates ppyRates (tail recVector) (tail lossVector) (recoveryLag,recoveryRate) -- `debug` ("===>C")
     where
       startBal = last_bal
       defAmt = mulBR startBal defRate
       ppyAmt = mulBR (startBal - defAmt) ppyRate 
       afterBal = startBal - defAmt - ppyAmt   
       
       surviveRate = (1 - defRate) * (1 - ppyRate) * bal_factor 
       schedulePrin = mulBR (CF.mflowPrincipal flow) surviveRate --TODO round trip  -- `debug` ("Schedule Principal"++(printf "%.2f" (CF.mflowPrincipal flow))++" Rate"++show(_schedule_rate))
       scheduleInt = mulBR (CF.mflowInterest flow) surviveRate

       newRec = mulBR defAmt recoveryRate
       newLoss = mulBR defAmt (1 - recoveryRate)

       recVector = replace recV recoveryLag newRec
       lossVector = replace lossV recoveryLag newLoss

       endBal = max 0 $ afterBal - schedulePrin -- `debug` ("start bal"++ show startBal ++"sch prin"++ show schedulePrin)

       tr = CF.MortgageFlow (CF.getDate flow) endBal schedulePrin scheduleInt ppyAmt defAmt (head recVector) (head lossVector) 0.0 Nothing Nothing Nothing--TODO missing ppy-penalty here

projectScheduleFlow trs b_factor lastBal [] _ _ (r:rs) (l:ls) (recovery_lag,recovery_rate)
  = projectScheduleFlow (trs++[tr]) b_factor lastBal [] [] [] rs ls (recovery_lag - 1,recovery_rate) 
   where
      remain_length = length rs
      lastDate = CF.getDate (last trs)
      flowDate = nextDate lastDate Lib.Monthly
      tr = CF.MortgageFlow flowDate lastBal 0 0 0 0 r l 0.0 Nothing Nothing Nothing



projFixCfwithAssumption :: (CF.CashFlowFrame, DatePattern) -> Maybe A.AssetPerfAssumption -> Date -> Either String CF.CashFlowFrame
projFixCfwithAssumption (cf@(CF.CashFlowFrame (begBal, begDate, accInt) flows), dp)
                        mPassump 
                        asOfDay
  = let
        curveDatesLength = recoveryLag + length flows
        endDate = CF.getDate (last flows)
        extraDates = genSerialDates dp Exc endDate recoveryLag
        (recoveryRate,recoveryLag) = case mPassump of 
                                       Nothing -> (0,0)
                                       (Just (A.MortgageAssump _ _ x _) )-> Ast.getRecoveryLagAndRate x
                                       (Just (A.MortgageDeqAssump _ _ x _) )-> Ast.getRecoveryLagAndRate x
        cfDates = (CF.getDate <$> flows) ++ extraDates
    in 
      do
        (ppyRates,defRates,recoveryRate,recoveryLag) <- case mPassump of 
                                                          Just pAssump -> buildAssumptionPpyDefRecRate (begDate:cfDates) pAssump 
                                                          Nothing -> Right (replicate curveDatesLength 0.0, replicate curveDatesLength 0.0, 0.0, 0)
        
        let txns = projectScheduleFlow [] 1.0 begBal flows defRates ppyRates
                                   (replicate curveDatesLength 0.0)
                                   (replicate curveDatesLength 0.0)
                                   (recoveryLag,recoveryRate) --  `debug` (" begin bal"++ show begBal)
        
        let (futureTxns,historyM) = CF.cutoffTrs asOfDay txns 
        
        let cb = (CF.mflowBegBalance . head) futureTxns
        return $ CF.CashFlowFrame (cb,asOfDay,Nothing) futureTxns

projIndexCashflows :: ([Date],[Balance],[Principal],Index,Spread) -> DatePattern -> Maybe A.AssetPerfAssumption -> Maybe [RateAssumption] -> Either String CF.CashFlowFrame
projIndexCashflows (ds,bals,principals,index,spd) dp mPassump (Just ras) = 
  do
    -- mIndexToApply = A.getRateAssumption ras index
    indexRates <- sequenceA $ A.lookupRate0 ras index <$> ds 

    let rates = (spd +) <$> indexRates 
    let interestFlow = zipWith (flip mulBIR) rates bals
    let flowSize = length bals
    let scheduleCf = CF.CashFlowFrame (head bals, head ds, Nothing) $ 
                                        zipWith12 MortgageFlow 
                                                  ds
                                                  bals
                                                  principals
                                                  interestFlow
                                                  (replicate flowSize 0 )
                                                  (replicate flowSize 0 )
                                                  (replicate flowSize 0 )
                                                  (replicate flowSize 0 )
                                                  rates
                                                  (replicate flowSize Nothing)
                                                  (replicate flowSize Nothing)
                                                  (replicate flowSize Nothing) 
    projFixCfwithAssumption (scheduleCf, dp) mPassump (head ds) 
    
-- ^ project cashflow with fix rate portion and floater rate portion
seperateCashflows :: ProjectedCashflow -> Maybe A.AssetPerfAssumption -> Maybe [RateAssumption] -> Either String (CF.CashFlowFrame, [CF.CashFlowFrame])
seperateCashflows (ProjectedFlowMixFloater pflow@(CF.CashFlowFrame (begBal, begDate, accuredInt) flows) dp (fixPct,fixRate) floaterList)
                  mPassump
                  mRates
  = let
        begBal = CF.mflowBegBalance $ head flows
        totalBals = begBal: ((view CF.tsRowBalance) <$> flows)
        ds = (view CF.tsDate) <$> flows
        flowSize = length ds
        -- fix rate cashflow
        fixedBals = flip mulBR fixPct <$> totalBals
        fixedPrincipalFlow = flip mulBR fixPct <$> CF.mflowPrincipal <$> flows
        fixedInterestFlow = flip mulBIR fixRate <$> fixedBals
        fixFlow = zipWith12 MortgageFlow ds fixedBals fixedPrincipalFlow fixedInterestFlow (replicate flowSize 0) (replicate flowSize 0) (replicate flowSize 0) (replicate flowSize 0) (replicate flowSize fixRate) (replicate flowSize Nothing) (replicate flowSize Nothing) (replicate flowSize Nothing)
       -- float rate cashflow
        totalFloatBalFlow = zipWith (-) totalBals fixedBals
        floatPrincipalFlow = zipWith (-) (CF.mflowPrincipal <$> flows) fixedPrincipalFlow
        
        floaterSize = length rs
        rs = (\(a,b,c) -> a) <$> floaterList      -- portion of each floater
        spds = (\(a,b,c) -> b) <$> floaterList    -- spreads
        indexes = (\(a,b,c) -> c) <$> floaterList -- indexes

        floatBalsBreakDown = (\r -> flip mulBR r <$> totalFloatBalFlow ) <$> rs
        floatPrincipalFlowBreakDown = (\r -> flip mulBR r <$> floatPrincipalFlow)  <$> rs -- `debug` ("float bal breakdown"++ show floatBalsBreakDown)
      in
        do 
          fixedCashFlow <- projFixCfwithAssumption ((CF.CashFlowFrame ( ((flip mulBR) fixPct) begBal
                                                                    , begDate
                                                                    , (flip mulBR) fixPct <$> accuredInt)
                                                                   fixFlow)
                                                , dp) mPassump begDate 
          floatedCashFlow <- sequenceA $ (\x -> projIndexCashflows x dp mPassump mRates) <$> zip5 
                                                                                              (replicate floaterSize ds) 
                                                                                              floatBalsBreakDown 
                                                                                              floatPrincipalFlowBreakDown 
                                                                                              indexes
                                                                                              spds
          return (fixedCashFlow, floatedCashFlow) -- `debug` ("float cf"++ show floatedCashFlow)



instance Ast.Asset ProjectedCashflow where

    getCurrentBal (ProjectedFlowFixed cf@(CF.CashFlowFrame (begBal,_,_) _) _) = begBal
    getCurrentBal (ProjectedFlowMixFloater cf@(CF.CashFlowFrame (begBal,_,_) _) _ _ _) = begBal

    getOriginBal x = getCurrentBal x
    getOriginRate x = 0.0

    isDefaulted f = error ""
    getOriginDate f = error ""
    getOriginInfo f = error ""

    getCurrentRate f = 0.0

    calcCashflow f@(ProjectedFlowFixed cf _) d _ = Right $ cf

    calcCashflow f@(ProjectedFlowMixFloater cf _ fxPortion floatPortion) d mRate
      = do
          (fixedCashFlow, floatedCashFlow) <- seperateCashflows f Nothing mRate   -- `debug` ("running fixed cashflow"++show fixedCashFlow)
          return $ foldl CF.combine fixedCashFlow floatedCashFlow
-- projFixCfwithAssumption :: (CF.CashFlowFrame, DatePattern) -> A.AssetPerfAssumption -> Date -> CF.CashFlowFrame
    projCashflow f@(ProjectedFlowFixed cf dp) asOfDay (pAssump,_,_) mRates 
      =  
        do 
          p <- projFixCfwithAssumption (cf, dp) (Just pAssump) asOfDay
          return (p, Map.empty)

    projCashflow f asOfDay (pAssump, _, _) mRates
      = do
          (fixedCashFlow, floatedCashFlow) <- seperateCashflows f (Just pAssump) mRates
          return $ (foldl CF.combine fixedCashFlow floatedCashFlow, Map.empty)
          --(fixedCashFlow, Map.empty)

    projCashflow a b c d = Left $ "Failed to match when proj projected flow with assumption >>" ++ show a ++ show b ++ show c ++ show d
    
    getBorrowerNum f = 0

    splitWith f rs = [f]

-- instance IR.UseRate ProjectedCashflow where 
--       isAdjustbleRate _ = False
--       getIndex _ = Nothing
--       getIndexes _ = Nothing
