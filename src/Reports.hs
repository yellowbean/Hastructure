{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}

module Reports (patchFinancialReports,getItemBalance,buildBalanceSheet,buildCashReport
            ) where

import Data.List ( find, sort )
import qualified Asset as P
import qualified Data.Map as Map
import qualified Cashflow as CF
import qualified Accounts as A
import qualified CreditEnhancement as CE
import qualified Hedge as HE
import qualified Expense as F
import qualified Liability as L
import Types
    ( ResultComponent(FinancialReport),
      CashflowReport(..),
      BalanceSheetReport(..),
      BookItem(..),
      RangeType(EI),
      DealStats(CurrentPoolBalance, CurrentPoolDefaultedBalance),
      CutoffFields(IssuanceBalance),
      Date,sliceBy,
      Balance, PoolId (PoolConsol) )
import Deal.DealBase
    ( TestDeal(TestDeal, pool, fees, bonds, accounts,liqProvider,rateSwap), getIssuanceStatsConsol, getAllCollectedFrame )
import Deal.DealQuery ( queryDeal )
import Deal.DealAction ( calcDueFee, calcDueInt )
import Data.Maybe (fromMaybe)

import Stmt
    ( aggByTxnComment,
      getFlow,
      getTxnComment,
      getTxns,
      FlowDirection(Outflow, Inflow) )


patchFinancialReports :: P.Asset a => TestDeal a -> Date -> [ResultComponent] -> [ResultComponent]
patchFinancialReports t d [] = []
patchFinancialReports t d logs 
  = case (find pickReportLog (reverse logs)) of 
      Nothing -> logs
      Just (FinancialReport sd ed bs cash) 
        -> let
             bsReport = buildBalanceSheet t d
             cashReport = buildCashReport t ed d
             newlog = FinancialReport ed d bsReport cashReport
           in
             logs++[newlog] 
      where 
        pickReportLog FinancialReport {} = True
        pickReportLog _ = False

getItemBalance :: BookItem -> Balance
getItemBalance (Item _ bal) = bal
getItemBalance (ParentItem _ items) = sum $ getItemBalance <$> items


-- TODO fix pool bablance
buildBalanceSheet :: P.Asset a => TestDeal a -> Date -> BalanceSheetReport
buildBalanceSheet t@TestDeal{ pool = pool, bonds = bndMap , fees = feeMap , liqProvider = liqMap, rateSwap = rsMap } d 
    = BalanceSheetReport {asset=ast,liability=liab,equity=eqty,reportDate=d}
    where 
        ---accured interest
        accM = [ Item accName accBal | (accName,accBal) <- Map.toList $ Map.map A.accBalance (accounts t) ]
        -- TODO Fix all pool
        consoleCF = getAllCollectedFrame t (Just [PoolConsol]) Map.! PoolConsol
        (performingBal,dBal,rBal) = case consoleCF of
                                      Nothing -> let 
                                                   _dbal = queryDeal t CurrentPoolDefaultedBalance
                                                   _pbal = queryDeal t (CurrentPoolBalance Nothing) - _dbal
                                                   consolStat = getIssuanceStatsConsol t Nothing
                                                   _issuancePbal = Map.findWithDefault 0 IssuanceBalance consolStat
                                                 in 
                                                   (max _pbal _issuancePbal, _dbal, 0)
                                      Just cf@(CF.CashFlowFrame _ txns) 
                                        -> (CF.mflowBalance (last txns) ,CF.totalDefault cf ,negate (CF.totalRecovery cf))
        
        poolAst = [ Item "Pool Performing" performingBal
                  , Item "Pool Defaulted" dBal
                  , Item "Pool Recovery" rBal]
        
        swapToCollect = [ Item ("Swap:"++rsName) rsNet | (rsName,rsNet) <- Map.toList (Map.map (HE.rsNetCash . (HE.accrueIRS d)) (fromMaybe Map.empty rsMap))
                                                       , rsNet > 0 ]
        ast = accM ++ poolAst ++ swapToCollect
        --tranches
        
        bndM = [ Item bndName bndBal | (bndName,bndBal) <- Map.toList $ Map.map L.getCurBalance (bonds t) ]
        bndAccPayable = [ Item ("Accured Int:"++bndName) bndAccBal | (bndName,bndAccBal) <- Map.toList (Map.map (L.totalDueInt . (calcDueInt t d Nothing Nothing)) bndMap)]
        feeToPay = [ Item ("Fee Due:"++feeName) feeDueBal | (feeName,feeDueBal) <- Map.toList (Map.map (F.feeDue . (calcDueFee t d)) feeMap)]
        liqProviderToPay = [ Item ("Liquidity Provider:"++liqName) liqBal | (liqName,liqBal) <- Map.toList (Map.map (CE.liqBalance . (CE.accrueLiqProvider d)) (fromMaybe Map.empty liqMap))] 
        swapToPay = [ Item ("Swap:"++rsName) (negate rsNet) | (rsName,rsNet) <- Map.toList (Map.map (HE.rsNetCash . (HE.accrueIRS d)) (fromMaybe Map.empty rsMap))
                                                   , rsNet < 0 ]
        liab = bndM ++ bndAccPayable ++ feeToPay ++ liqProviderToPay ++ swapToPay -- `debug` ("ACC BOND"++show bndAccPayable)

        totalAssetBal = sum $ getItemBalance <$> ast  
        totalDebtBal = sum $ getItemBalance <$> liab
        eqty = [ Item "Net Asset" (totalAssetBal - totalDebtBal) ]

buildCashReport :: P.Asset a => TestDeal a -> Date -> Date -> CashflowReport
buildCashReport t@TestDeal{accounts = accs } sd ed 
  = CashflowReport { inflow = inflowItems
                   , outflow = outflowItems
                   , net = cashChange
                   , startDate = sd
                   , endDate = ed }
      where 
        _txns = concat $ Map.elems $ Map.map getTxns $ Map.map A.accStmt accs
        txns = sliceBy EI sd ed _txns
   
        inflowTxn = sort $ filter (\x -> (getFlow . getTxnComment) x == Inflow)  txns
        outflowTxn = sort $ filter (\x -> (getFlow . getTxnComment) x == Outflow) txns
        
        inflowM = Map.mapKeys show $ aggByTxnComment inflowTxn Map.empty
        outflowM = Map.mapKeys show $ aggByTxnComment outflowTxn Map.empty 
        
        inflowItems = [ Item k v | (k,v) <- Map.toList inflowM ]
        outflowItems = [ Item k v | (k,v) <- Map.toList outflowM ]
        
        cashChange = sum (Map.elems inflowM) + sum (Map.elems outflowM)

