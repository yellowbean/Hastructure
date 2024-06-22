{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module AssetClass.ProjectedCashFlow  
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

import qualified Cashflow as CF

import AssetClass.AssetBase
import AssetClass.AssetCashflow

import qualified Assumptions as A

import Cashflow (extendTxns,TsRow(..),mflowBalance)

import Debug.Trace

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

       endBal = max 0 $ afterBal - schedulePrin

       tr = CF.MortgageFlow (CF.getDate flow) endBal schedulePrin scheduleInt ppyAmt defAmt (head recVector) (head lossVector) 0.0 Nothing Nothing Nothing--TODO missing ppy-penalty here

projectScheduleFlow trs b_factor lastBal [] _ _ (r:rs) (l:ls) (recovery_lag,recovery_rate)
  = projectScheduleFlow (trs++[tr]) b_factor lastBal [] [] [] rs ls (recovery_lag - 1,recovery_rate) 
   where
      remain_length = length rs
      lastDate = CF.getDate (last trs)
      flowDate = nextDate lastDate Lib.Monthly
      tr = CF.MortgageFlow flowDate lastBal 0 0 0 0 r l 0.0 Nothing Nothing Nothing



projCfwithAssumption :: (CF.CashFlowFrame, DatePattern) -> A.AssetPerfAssumption -> Date -> Maybe [RateAssumption] -> CF.CashFlowFrame
projCfwithAssumption (cf@(CF.CashFlowFrame (begBal, begDate, accInt) flows), dp)
                     pAssump@(A.MortgageAssump mDefault mPrepay mRecovery mEs) 
                     asOfDay
                     mRates 
  = CF.CashFlowFrame (cb,asOfDay,Nothing) futureTxns
      where
        curveDatesLength =  recoveryLag + length flows
        endDate = CF.getDate (last flows)
        (ppyRates,defRates,recoveryRate,recoveryLag) = buildAssumptionPpyDefRecRate (begDate:cfDates) pAssump 
        extraDates = genSerialDates dp Exc endDate recoveryLag
        cfDates = (CF.getDate <$> flows) ++ extraDates
        
        txns = projectScheduleFlow [] 1.0 begBal flows defRates ppyRates
                                   (replicate curveDatesLength 0.0)
                                   (replicate curveDatesLength 0.0)
                                   (recoveryLag,recoveryRate) 
        
        (futureTxns,historyM) = CF.cutoffTrs asOfDay txns 
        
        cb = (CF.mflowBegBalance . head) futureTxns

projIndexCashflows :: ([Date],[Balance],[Principal],Index,Spread) -> Maybe [RateAssumption] -> CF.CashFlowFrame
projIndexCashflows (ds,bals,principals,index,spd) (Just ras) = 
  let
    mIndexToApply = A.getRateAssumption ras index
    indexRates = A.lookupRate0 ras index <$> ds 

    rates = (spd +) <$> indexRates 
    interestFlow = zipWith (flip mulBIR) rates bals
    flowSize = length bals
  in
    CF.CashFlowFrame (head bals, head ds, Nothing) $ 
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
                                                    



-- projMortgageFlow :: (Date,[Balance]) -> A.AssetPerfAssumption -> Maybe [RateAssumption] -> ([Balance],[Balance],[Balance])
-- projMortgageFlow (begDate,bals) pAssump mRates = 
--   let
--     curveDatesLength =  recoveryLag + length bals
--     extraPeriods = recoveryLag
--     endDate = last ds
--     extraDates = genSerialDates dp Exc endDate recoveryLag
--     cfDates = ds ++ extraDates
--     begBal = head bals
--     (ppyRates,defRates,recoveryRate,recoveryLag) = buildAssumptionPpyDefRecRate (begDate:cfDates) pAssump 
--     
--     txns = projectScheduleFlow [] 1.0 begBal flows defRates ppyRates
--                                (replicate curveDatesLength 0.0)
--                                (replicate curveDatesLength 0.0)
--                                (recoveryLag,recoveryRate) 
--     
--     (futureTxns,historyM) = CF.cutoffTrs asOfDay txns 
--     
--     cb = (CF.mflowBegBalance . head) futureTxns
--   in
--     (cb, futureTxns, historyM)


seperateCashflows :: ProjectedCashflow -> Maybe A.AssetPerfAssumption -> Maybe [RateAssumption] -> (CF.CashFlowFrame, [CF.CashFlowFrame])
seperateCashflows (ProjectedFlowMixFloater begDate flows _ (fixPct,fixRate) floaterList)
                   (Just pAssump)
                   mRates
  = let
        begBal = CF.mflowBegBalance $ head flows
        totalBals = begBal: (CF.mflowBalance <$> flows)
        ds = CF.mflowDate <$> flows
        
        (ppyRates,defRates,recoveryRate,recoveryLag) = buildAssumptionPpyDefRecRate (begDate:ds) pAssump 
        

        flowSize = length ds
        fixedBals = (flip mulBR) fixPct <$> totalBals
        fixedPrincipalFlow =  replicate flowSize 0 -- flip mulBR fixPct <$> CF.mflowPrincipal <$> flows
        fixedInterestFlow =  replicate flowSize 0 -- flip mulBIR fixRate <$> fixedBals
        fixedCashFlow = CF.CashFlowFrame (head fixedBals, begDate, Nothing) []
                                        --  zipWith12 CF.MortgageFlow $
                                        --             ds $
                                        --             init fixedBals $
                                        --             fixedPrincipalFlow $
                                        --             fixedInterestFlow $
                                        --             replicate flowSize 0 $
                                        --             replicate flowSize 0 $
                                        --             replicate flowSize 0 $
                                        --             replicate flowSize 0 $
                                        --             replicate flowSize fixRate $
                                        --             replicate flowSize Nothing $
                                        --             replicate flowSize Nothing $
                                        --             replicate flowSize Nothing 
                                                 
        floatBals = zipWith (-) totalBals fixedBals
        floatPrincipalFlow = zipWith (-) (CF.mflowPrincipal <$> flows) fixedPrincipalFlow
        
        -- rs = (head <$> floaterList)
        -- floaterSize = length rs
        -- indexes = last <$> floaterList
        -- spds = (\(a,b,c) -> b) <$> floaterList
        -- floatBalsBreakDown = zipWith mulBR floatBals rs
        -- floatPrincipalFlowBreakDown = zipWith mulBR floatPrincipalFlow rs
        -- floatedCashFlow = \x -> projIndexCashflows x mRates <$> zip5 
        --                                                             (repeat ds) 
        --                                                             floatBalsBreakDown 
        --                                                             floatPrincipalFlowBreakDown 
        --                                                             indexes
        --                                                             spds
  in
    -- (fixedCashFlow, floatedCashFlow)
    (fixedCashFlow, [CF.CashFlowFrame (0, begDate, Nothing) []])



instance Ast.Asset ProjectedCashflow where

    getCurrentBal (ProjectedFlowFixed _ cf _ ) = CF.mflowBalance (head cf)
    getCurrentBal (ProjectedFlowMixFloater _ cf _ _ _ ) = CF.mflowBegBalance (head cf)

    getOriginBal (ProjectedFlowFixed _ cf _ ) = CF.mflowBegBalance (head cf)
    getOriginBal (ProjectedFlowMixFloater _ cf _ _ _ ) = CF.mflowBegBalance (head cf)

    isDefaulted f = error ""
    getOriginDate f = error ""
    getOriginInfo f = error ""

    calcCashflow f@(ProjectedFlowFixed begDate flows _) d _ 
      = CF.CashFlowFrame ( (CF.mflowBalance . head) flows, begDate, Nothing ) flows

    calcCashflow f@(ProjectedFlowMixFloater {}) d mRate
      = let 
          (fixedCashFlow, floatedCashFlow) = seperateCashflows f Nothing mRate
        in
          -- Map.foldl CF.mergePoolCf fixedCashFlow floatedCashFlow
          fixedCashFlow

    projCashflow f asOfDay _ mRates = (calcCashflow f asOfDay mRates, Map.empty)


    projCashflow f asOfDay (_, _, _) mRates
      = let 
          (fixedCashFlow, floatedCashFlow) = seperateCashflows f Nothing mRates
        in
          -- Map.foldl CF.mergePoolCf fixedCashFlow floatedCashFlow
          (fixedCashFlow, Map.empty)

    getBorrowerNum f = 0

    splitWith f rs = [f]

instance IR.UseRate ProjectedCashflow where 
      isAdjustbleRate _ = False

      getIndex _ = Nothing
      getIndexes _ = Nothing
      getResetDates _ = []
