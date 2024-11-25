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
      DealStats(..),
      CutoffFields(IssuanceBalance),
      Date,sliceBy,
      Balance, PoolId (..) ,PoolSource(..))
import Deal.DealBase
    ( TestDeal(TestDeal, pool, fees, bonds, accounts,liqProvider,rateSwap), getIssuanceStatsConsol, getAllCollectedFrame ,poolTypePool, dealPool)
import Deal.DealQuery ( queryDeal ,queryDealInt)
import Deal.DealAction ( calcDueFee, calcDueInt )
import Data.Maybe (fromMaybe)

import Control.Lens hiding (element)
import Control.Lens.TH
import Control.Lens
import Stmt
    ( aggByTxnComment,
      getFlow,
      getTxnComment,
      getTxns,
      FlowDirection(Outflow, Inflow) )

-- ^ add financial report to the logs
patchFinancialReports :: P.Asset a => TestDeal a -> Date -> [ResultComponent] -> Either String [ResultComponent]
patchFinancialReports t d [] = Right []
patchFinancialReports t d logs 
  = case (find pickReportLog (reverse logs)) of 
      Nothing -> Right logs
      Just (FinancialReport sd ed bs cash) 
        -> let
             cashReport = buildCashReport t ed d
           in
             do 
               bsReport <- buildBalanceSheet t d
               let newlog = FinancialReport ed d bsReport cashReport
               return (logs++[newlog])
      where 
        pickReportLog FinancialReport {} = True
        pickReportLog _ = False

getItemBalance :: BookItem -> Balance
getItemBalance (Item _ bal) = bal
getItemBalance (ParentItem _ items) = sum $ getItemBalance <$> items


getPoolBalanceStats :: P.Asset a => TestDeal a -> Maybe PoolId -> (Balance,Balance,Balance)
getPoolBalanceStats t Nothing = (queryDeal t (FutureCurrentPoolBalance Nothing)
                                 ,(queryDeal t (PoolCumCollection [NewDefaults] Nothing))
                                 ,negate (queryDeal t (PoolCumCollection [CollectedRecoveries] Nothing)))

getPoolBalanceStats t (Just pid) = (queryDeal t (FutureCurrentPoolBalance (Just [pid]))
                                   ,(queryDeal t (PoolCumCollection [NewDefaults] (Just [pid])))
                                   ,negate (queryDeal t (PoolCumCollection [CollectedRecoveries] (Just [pid]))))

type PoolBalanceSnapshot = (Balance, Balance, Balance)

buildBalanceSheet :: P.Asset a => TestDeal a -> Date -> Either String BalanceSheetReport
buildBalanceSheet t@TestDeal{ pool = pool, bonds = bndMap , fees = feeMap , liqProvider = liqMap, rateSwap = rsMap ,accounts = accMap} 
                  d 
    = let  
        --- accounts
        accM = [ ParentItem accName [Item "Balance" accBal,Item "Accrue Int" accDue] | (accName, [accBal,accDue]) <- Map.toList $ Map.map (\acc -> [A.accBalance,(A.accrueInt d)] <*> [acc]) accMap ]
        -- accsDueMap = [ Item accName accAccrueBal | (accName, accAccrueBal) <- Map.toList $ Map.map (A.accrueInt d) accMap ]
        
        ---- pools
        mapPoolKey PoolConsol = Nothing 
        mapPoolKey (PoolName x) = Just (PoolName x)
        poolMap = Map.mapKeys mapPoolKey $ view (dealPool . poolTypePool) t
        poolAstBalMap = Map.mapWithKey 
                          (\k _ -> getPoolBalanceStats t k)
                          poolMap
        
        poolAstMap = Map.mapWithKey 
                       (\k (a,b,c) -> ParentItem (show (fromMaybe PoolConsol k))
                                        [ Item "Performing" a
                                        , Item "Defaulted"  b
                                        , Item "Recovery"   c ])
                       poolAstBalMap
        poolAst = ParentItem "Pool" $ Map.elems poolAstMap
        ---- swaps
        swapToCollect = ParentItem "Swap" [ ParentItem rsName [ Item "To Receive" rsNet ] | (rsName,rsNet) <- Map.toList (Map.map (HE.rsNetCash . (HE.accrueIRS d)) (fromMaybe Map.empty rsMap))
                                            , rsNet > 0 ]
        ast = ParentItem "Asset" [ParentItem "Account" accM , poolAst , swapToCollect]


        -- tranches
        bndM = [ ParentItem bndName [Item "Balance" bndBal,Item "Due Int" bndDueAmt ] 
                                      | (bndName,[bndBal,bndDueAmt]) <- Map.toList $ Map.map (\bnd -> [L.getCurBalance, (L.totalDueInt . (calcDueInt t d Nothing Nothing))] <*> [bnd]) bndMap ]

        -- expenses
        -- liquidity provider 
        liqProviderAccrued = Map.map (CE.accrueLiqProvider d) (fromMaybe Map.empty liqMap)
        liqProviderOs = [ ParentItem liqName [Item "Balance" liqBal,Item "Accrue Int" liqDueInt, Item "Due Fee" liqDueFee ]  | (liqName,[liqBal,liqDueInt,liqDueFee]) <- Map.toList (Map.map (\liq -> [CE.liqBalance,CE.liqDueInt,CE.liqDuePremium]<*> [liq]) liqProviderAccrued)] 
        -- rate swap
        swapToPay = ParentItem "Swap" [ ParentItem rsName [Item "To Pay" (negate rsNet)] | (rsName,rsNet) <- Map.toList (Map.map (HE.rsNetCash . (HE.accrueIRS d)) (fromMaybe Map.empty rsMap))
                                                   , rsNet < 0 ]

      in
        do
          feeWithDueAmount <- (F.feeDue <$>) <$>  mapM ((calcDueFee t d)) feeMap
          let feeToPay = ParentItem "Fee" [ ParentItem feeName [Item "Due" feeDueBal] 
                                           | (feeName,feeDueBal) <- Map.toList feeWithDueAmount ]
          let liab = ParentItem "Liability" [ ParentItem "Bond" bndM , feeToPay, ParentItem "Liquidity" liqProviderOs, swapToPay] -- `debug` ("ACC BOND"++show bndAccPayable)
          let totalDebtBal = getItemBalance liab
          let totalAssetBal = getItemBalance ast  
          let eqty = Item "Net Asset" (totalAssetBal - totalDebtBal)
          return $ BalanceSheetReport {asset=ast,liability=liab,equity=eqty,reportDate=d}

buildCashReport :: P.Asset a => TestDeal a -> Date -> Date -> CashflowReport
buildCashReport t@TestDeal{accounts = accs} sd ed 
  = CashflowReport { inflow = inflowItems
                   , outflow = outflowItems
                   , net = cashChange
                   , startDate = sd
                   , endDate = ed }
      where 
        -- TODO  performance improve here, need to filter txn first
        _txns = concat $ Map.elems $ Map.map getTxns $ Map.map A.accStmt accs
        txns = sliceBy EI sd ed _txns
   
        inflowTxn = sort $ filter (\x -> (getFlow . getTxnComment) x == Inflow)  txns
        outflowTxn = sort $ filter (\x -> (getFlow . getTxnComment) x == Outflow) txns
        
        inflowM = Map.mapKeys show $ aggByTxnComment inflowTxn Map.empty
        outflowM = Map.mapKeys show $ aggByTxnComment outflowTxn Map.empty 
        
        inflowItems = ParentItem "Inflow" [ Item k v | (k,v) <- Map.toList inflowM ]
        outflowItems = ParentItem "Outflow" [ Item k v | (k,v) <- Map.toList outflowM ]
        
        cashChange = Item "Net Cash" $ sum (Map.elems inflowM) + sum (Map.elems outflowM)

