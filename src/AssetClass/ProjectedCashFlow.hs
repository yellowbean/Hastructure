{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module AssetClass.ProjectedCashFlow  
  (ProjectedCashFlow(..))
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
import qualified Data.DList as DL
import qualified Cashflow as CF

import AssetClass.AssetBase
import AssetClass.AssetCashflow

import Cashflow (extendTxns,TsRow(..))

import Debug.Trace
import Control.Lens hiding (element,Index)
import Control.Lens.TH
debug = flip trace


projectScheduleFlow ::  DL.DList CF.TsRow -> Rate -> Balance -> [CF.TsRow] -> [DefaultRate] -> [PrepaymentRate] -> [Amount] -> [Amount] -> (Int, Rate) -> (DL.DList CF.TsRow)
projectScheduleFlow trs _ last_bal [] _ _ [] [] (_,_) = trs
projectScheduleFlow trs bal_factor startBal (flow:flows) (defRate:defRates) (ppyRate:ppyRates) recV lossV (recoveryLag,recoveryRate)
  = let
      defAmt = mulBR startBal defRate
      ppyAmt = mulBR (startBal - defAmt) ppyRate 
      afterBal = startBal - defAmt - ppyAmt   
      curWac = CF.mflowRate flow
      
      surviveRate = (1 - defRate) * (1 - ppyRate) * bal_factor
      -- surviveRate
      --   | startBal == 0 = 1
      --   | otherwise = (1 - (divideBB (defAmt + ppyAmt) startBal)) * bal_factor
      -- schedulePrin = mulBR (CF.mflowPrincipal flow) surviveRate 
      (schedulePrin', lossOfPrin) = splitBal surviveRate (CF.mflowPrincipal flow)
      schedulePrin = schedulePrin' 
      scheduleInt = mulBR (CF.mflowInterest flow) surviveRate

      newRec = mulBR defAmt recoveryRate
      newLoss = mulBR defAmt (1 - recoveryRate)
      recVector = replace recV recoveryLag newRec
      lossVector = replace lossV recoveryLag newLoss

      endBal = max 0 $ afterBal - schedulePrin -- `debug` ("start bal"++ show startBal ++"sch prin"++ show schedulePrin)
      tr = CF.MortgageFlow (CF.getDate flow) endBal schedulePrin scheduleInt ppyAmt defAmt (head recVector) (head lossVector) curWac Nothing Nothing Nothing
    in 
      projectScheduleFlow (DL.snoc trs tr) surviveRate endBal flows defRates ppyRates (tail recVector) (tail lossVector) (recoveryLag,recoveryRate) -- `debug` ("===>C")

projectScheduleFlow trs b_factor lastBal [] _ _ (r:rs) (l:ls) (recovery_lag,recovery_rate)
  = let 
      remain_length = length rs
      lastDate = CF.getDate (last (DL.toList trs))
      flowDate = nextDate lastDate Lib.Monthly
      tr = CF.MortgageFlow flowDate lastBal 0 0 0 0 r l 0.0 Nothing Nothing Nothing
    in 
      projectScheduleFlow (DL.snoc trs tr) b_factor lastBal [] [] [] rs ls (recovery_lag - 1,recovery_rate) 


-- ^ project cashflow with fix rate portion
projFixCfwithAssumption :: (CF.CashFlowFrame, DatePattern) -> ([Rate],[Rate],Rate,Int) -> Date -> Either String CF.CashFlowFrame
projFixCfwithAssumption (cf@(CF.CashFlowFrame (begBal, begDate, accInt) flows), dp)
                        (ppyRates,defRates,recoveryRate,recoveryLag)
                        asOfDay
  = let
        curveDatesLength = recoveryLag + length flows
        endDate = CF.getDate (last flows)
        extraDates = genSerialDates dp Exc endDate recoveryLag
        cfDates = (CF.getDate <$> flows) ++ extraDates
    in 
      do
        let txns = projectScheduleFlow DL.empty 1.0 begBal flows defRates ppyRates
                    (replicate curveDatesLength 0.0)
                    (replicate curveDatesLength 0.0)
                    (recoveryLag,recoveryRate) --  `debug` (" begin bal"++ show begBal)
        
        let (futureTxns,historyM) = CF.cutoffTrs asOfDay (DL.toList txns)
        
        let cb = (CF.mflowBegBalance . head) futureTxns
        return $ CF.CashFlowFrame (cb,asOfDay,Nothing) futureTxns

-- ^ project cashflow with fix rate portion
projIndexCashflows :: ([Date],[Balance],[Principal],Index,Spread) -> DatePattern -> ([Rate],[Rate],Rate,Int) -> Maybe [RateAssumption] -> Either String CF.CashFlowFrame
projIndexCashflows (ds,bals,principals,index,spd) dp pAssump Nothing = Left "No rate assumption provided for index cashflow projection" 
projIndexCashflows (ds,bals,principals,index,spd) dp pAssump (Just ras) = 
  do
    -- mIndexToApply = A.getRateAssumption ras index
    indexRates <- traverse (A.lookupRate0 ras index) ds 

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
    projFixCfwithAssumption (scheduleCf, dp) pAssump (head ds) 
    
-- ^ project cashflow with fix rate portion and floater rate portion
seperateCashflows :: ProjectedCashFlow -> Maybe A.AssetPerfAssumption -> Maybe [RateAssumption] -> Either String (CF.CashFlowFrame, [CF.CashFlowFrame])
seperateCashflows a@(ProjectedCashflow pflow@(CF.CashFlowFrame (begBal, begDate, accuredInt) flows) dp (fixPct,fixRate) floaterList)
                  mPassump
                  mRates
  = let
        totalBals = begBal: ((view CF.tsRowBalance) <$> flows)
        ds = (view CF.tsDate) <$> flows
        flowSize = length ds
        -- fix rate cashflow
        -- fix balance = total balance * fix percent
        fixedBals = flip mulBR fixPct <$> totalBals
        -- fix principal flow = total principal flow * fix percent
        fixedPrincipalFlow = flip mulBR fixPct <$> CF.mflowPrincipal <$> flows
        -- fix principal interest = total principal flow * fix rate
        fs = getIntervalFactors (begDate:ds)
        -- fixedInterestFlow = flip mulBIR fixRate <$>  fixedBals
        fixedInterestFlow = [ mulBR b (fromRational (toRational fixRate * r)) | (b,r) <- zip fixedBals fs]
        fixFlow = zipWith12 MortgageFlow ds fixedBals fixedPrincipalFlow fixedInterestFlow (replicate flowSize 0) (replicate flowSize 0) (replicate flowSize 0) (replicate flowSize 0) (replicate flowSize fixRate) (replicate flowSize Nothing) (replicate flowSize Nothing) (replicate flowSize Nothing)
        -- float rate cashflow
        -- float balance = total balance - fixed balance
        totalFloatBalFlow = zipWith (-) totalBals fixedBals
        -- float principal flow = total principal flow - fixed principal flow
        floatPrincipalFlow = zipWith (-) (CF.mflowPrincipal <$> flows) fixedPrincipalFlow
        
        rs = (\(a,iRate,b,c) -> a) <$> floaterList      -- portion of each floater
        spds = (\(a,iRate,b,c) -> b) <$> floaterList    -- spreads
        indexes = (\(a,iRate,b,c) -> c) <$> floaterList -- indexes
        floaterSize = length rs
        -- float bal brekdown by index
        floatBalsBreakDown = (\r -> flip mulBR r <$> totalFloatBalFlow ) <$> rs
        -- float principal flow breakdown by index
        floatPrincipalFlowBreakDown = (\r -> flip mulBR r <$> floatPrincipalFlow) <$> rs -- `debug` ("float bal breakdown"++ show floatBalsBreakDown)
        recoveryLag = case mPassump of 
                        Nothing -> 0 
                        Just passump -> fromMaybe 0 $ getRecoveryLagFromAssumption passump
        curveDatesLength = length flows + recoveryLag
      in
        do
          assumptionInput <- case mPassump of 
                              Just pAssump -> buildAssumptionPpyDefRecRate a (begDate:ds) pAssump 
                              Nothing -> Right (replicate curveDatesLength 0.0, replicate curveDatesLength 0.0, 0.0, 0)
          fixedCashFlow <- projFixCfwithAssumption ((CF.CashFlowFrame ( ((flip mulBR) fixPct) begBal
                                                                    , begDate
                                                                    , (flip mulBR) fixPct <$> accuredInt)
                                                                   fixFlow)
                                                , dp) assumptionInput begDate 
          floatedCashFlow <- traverse 
                           (\x -> projIndexCashflows x dp assumptionInput mRates) 
                   $ zip5 (replicate floaterSize ds) floatBalsBreakDown floatPrincipalFlowBreakDown indexes spds
          return (fixedCashFlow, floatedCashFlow) -- `debug` ("float cf"++ show floatedCashFlow)



instance Ast.Asset ProjectedCashFlow where

    getCurrentBal (ProjectedCashflow cf@(CF.CashFlowFrame (begBal,_,_) _) _ _ _) = begBal

    getOriginBal x = getCurrentBal x
    getOriginRate (ProjectedCashflow cf@(CF.CashFlowFrame (begBal,_,_) _) _ (fixRatePortion,fixRate) floatRatePortions)
      = let
          avgFloatRate = weightedBy ((view _1) <$> floatRatePortions) (toRational . (view _2) <$> floatRatePortions)
          floatRatePortion = sum $ (view _1) <$> floatRatePortions
        in 
          fromRational $ weightedBy [fixRatePortion,(1-fixRatePortion)] [toRational fixRate,avgFloatRate]


    isDefaulted f = False
    getOriginDate (ProjectedCashflow cf@(CF.CashFlowFrame (_,startDate,_) _) _ _ _) = startDate

    getCurrentRate f = getOriginRate f

    calcCashflow f@(ProjectedCashflow cf _ fxPortion floatPortion) d mRate
      = do
          (fixedCashFlow, floatedCashFlow) <- seperateCashflows f Nothing mRate
          return $ foldl CF.combine fixedCashFlow floatedCashFlow

    projCashflow f asOfDay (pAssump, _, _) mRates
      = do
          (fixedCashFlow, floatedCashFlow) <- seperateCashflows f (Just pAssump) mRates
          return (foldl CF.combine fixedCashFlow floatedCashFlow, Map.empty)

    projCashflow a b c d = Left $ "Failed to match when proj projected flow with assumption >>" ++ show a ++ show b ++ show c ++ show d
    
    getBorrowerNum f = 0

    splitWith f rs = [f]
