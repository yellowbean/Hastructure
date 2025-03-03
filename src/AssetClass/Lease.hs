{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AssetClass.Lease
  (Lease(..),projCashflow,updateOriginDate)
  where

import qualified Data.Time as T
import qualified Cashflow as CF -- (Cashflow,Amount,Interests,Principals)
import qualified Assumptions as AP
import Asset
import Types
import Lib
import Util
import DateUtil

import qualified Data.Map as Map
import Data.List
import Data.Aeson hiding (json)
import Language.Haskell.TH
import Data.Aeson.TH
import Data.Aeson.Types
import GHC.Generics
import Data.Maybe
import AssetClass.AssetBase

import Control.Lens hiding (element)
import Control.Lens.TH

import Debug.Trace
import qualified Assumptions as A
debug = flip trace

type PeriodAmount = Balance
type CapRate = Rate
type RentChangeRate = Rate
type RentChangeCurve = Ts
type TermChangeRate = Rate
type DayGap = Int
type LastAccuredDate = Date

calcChangeRateOnRental :: AP.LeaseAssetRentAssump -> Date -> Date -> Rate
calcChangeRateOnRental (AP.BaseAnnualRate r) sd ed = 1 + yearCountFraction DC_ACT_365F sd ed * r 
calcChangeRateOnRental (AP.BaseCurve rc) sd ed = 1 + yearCountFraction DC_ACT_365F sd ed * getValByDate rc Exc ed

calcGapDays :: AP.LeaseAssetGapAssump -> Date -> Int
calcGapDays (AP.GapDays days) _ = days
calcGapDays (AP.GapDaysByAmount tbl defaultDays) _ = defaultDays
calcGapDays (AP.GapDaysByCurve ts) d = round $ fromRational $ getValByDate ts Exc d 


-- ^ Generate next lease with new rental / term changes/ day gap
nextLease :: Lease -> (AP.LeaseAssetRentAssump, TermChangeRate, DayGap) -> (Lease, Date)
nextLease l@(RegularLease (LeaseInfo sd ot dp dr ob) bal rt _) (rAssump,tc,gd) 
  = let
        leaseEndDate = last $ projectCfDates dp sd ot 
        nextStartDate = T.addDays (succ (toInteger gd)) leaseEndDate -- `debug` ("Gap Day ->"++ show gd)
        nextOriginTerm = round $ mulIR ot (1+tc) 
        nextEndDate = last $ genSerialDates dp Inc nextStartDate (fromIntegral nextOriginTerm)
        nextDailyRate = mulBR dr $ calcChangeRateOnRental rAssump sd nextStartDate
        newBal =  fromRational $ mulBInteger nextDailyRate $ daysBetween nextStartDate nextEndDate
    in 
      (RegularLease (LeaseInfo nextStartDate nextOriginTerm dp nextDailyRate ob) newBal rt Current,nextEndDate) -- `debug` ("1+tc"++show (1+tc) ++">>"++ show (mulIR ot (1+tc)))

nextLease l@(StepUpLease (LeaseInfo sd ot dp dr ob) lsteupInfo bal rt _) (rAssump,tc,gd) 
  = let 
        leaseEndDate = last $ projectCfDates dp sd ot 
        nextStartDate = T.addDays (succ (toInteger gd)) leaseEndDate -- `debug` ("Gap Day ->"++ show gd)
        nextOriginTerm = round $ mulIR ot (1+tc) 
        nextEndDate = last $ genSerialDates dp Inc nextStartDate (fromIntegral nextOriginTerm)
        nextDailyRate =  mulBR dr $ calcChangeRateOnRental rAssump sd nextStartDate
        newBal = -1
    in
      (StepUpLease (LeaseInfo nextStartDate nextOriginTerm dp nextDailyRate ob) lsteupInfo newBal rt Current,nextEndDate) --  `debug` ("leaseEndDate>>"++show leaseEndDate++">>>"++show (succ (toInteger gd)))

-- | create a new lease base on the lease in 1st argument, with new rental/term, a gap days, till the end date
nextLeaseTill :: Lease -> (AP.LeaseAssetRentAssump, TermChangeRate, DayGap) -> Date -> AP.LeaseEndType -> [Lease] -> [Lease]
nextLeaseTill l (rsc,tc,mg) lastDate (AP.CutByDate ed) accum 
  | lastDate >= ed = accum 
  | otherwise = nextLeaseTill new_lease (rsc,tc,mg) new_lastDate (AP.CutByDate ed) (accum++[new_lease])
                where 
                 (new_lease,new_lastDate) = nextLease l (rsc,tc,mg)

nextLeaseTill l (rsc,tc,mg) lastDate (AP.StopByExtTimes n) accum 
  | n == 0 = accum 
  | otherwise = nextLeaseTill new_lease (rsc,tc,mg) new_lastDate (AP.StopByExtTimes (pred n)) (accum++[new_lease])
                where 
                 (new_lease,new_lastDate) = nextLease l (rsc,tc,mg) 


getGapDaysByBalance :: Lease -> ([(Amount,Int)],Int) -> Int 
getGapDaysByBalance l tbl@(rows,defaultVal)
  = let 
      tbl = ThresholdTable rows 
      pmt = case l of 
              (RegularLease (LeaseInfo _ _ _ dr _) _ _ _) -> dr
              (StepUpLease (LeaseInfo _ _ _ dr _) _ _ _ _) -> dr
    in 
      fromMaybe  defaultVal $ lookupTable tbl Down (>= pmt)

projectCfDates :: DatePattern -> Date -> Int -> [Date]
projectCfDates dp sd ot
  = let  
        cf_dates_proj = genSerialDates dp Inc sd ot
    in 
        if head cf_dates_proj == sd then 
            genSerialDates dp Inc sd (succ ot)
        else
            sd:cf_dates_proj

-- ^ calculate the daily rate for a step up lease
-- TODO: factor rates to model the defaulted factors
calcPmts :: LeaseStepUp -> [Rate] -> Amount -> Either String [Amount] 
calcPmts (FlatRate _r) fs amt = Right $ amt:(scanl mulBR amt (replicate (length fs) _r))
calcPmts (ByFlatAmount _amt) fs amt = Right $ amt:(scanl (+) amt (replicate (length fs) _amt))
calcPmts (ByRateCurve rs) fs amt 
  | length rs /= length fs = Left "The length of the factors and the rate curve should be the same"
  | otherwise = Right $ amt:(scanl mulBR amt rs)
calcPmts (ByAmountCurve amts) fs amt 
  | length amts /= length fs = Left "The length of the factors and the amount curve should be the same"
  | otherwise = Right $ amt:(scanl (+) amt amts)


-- ^ return a lease contract with opening balance and a payment cashflow on each payment date
patchBalance :: Lease -> Either String (Lease,[Amount]) 
patchBalance (RegularLease (LeaseInfo sd ot dp dr ob) bal rt st)
  = let 
      cf_dates = lastN (succ rt) $ projectCfDates dp sd ot
      pmts = [ fromRational (mulBInt dr ds) | ds <- getIntervalDays cf_dates ]
      new_bal = sum pmts -- `debug` ("cf_date" ++ show cf_dates)
    in
      Right (RegularLease (LeaseInfo sd ot dp dr ob) new_bal rt st, pmts)

patchBalance l@(StepUpLease (LeaseInfo sd ot dp dr ob) lsu bal rt st)
  = let 
      cfDates = sd:getPaymentDates l 0
      intervals = daysInterval cfDates
      factors = replicate ((length cfDates) - 2) 1.0
    in 
      do 
        dailyRentals <- calcPmts lsu factors dr
        let pmts = [ fromRational (mulBInteger r d) | (d,r) <- zip intervals dailyRentals ]
        let new_bal = sum pmts 
        return (StepUpLease (LeaseInfo sd ot dp dr ob) lsu new_bal rt st,pmts)


instance Asset Lease where 
    calcCashflow l@(RegularLease (LeaseInfo sd ot dp dr ob) _ rt st) d _ =
      do 
        (l',pmts) <- patchBalance l
        let bal = getCurrentBal l'
        let cf_dates = lastN (succ rt) $ projectCfDates dp sd ot
        let daysBetween = getIntervalDays cf_dates -- `debug` (">>>>>> genSerialDates"++ show cf_dates)
        let bals = tail $ scanl (-) bal pmts -- `debug` ("PMTS for regular"++show pmts)
        return $ CF.CashFlowFrame (0,d,Nothing) $ cutBy Inc Future d (zipWith3 CF.LeaseFlow (tail cf_dates) bals pmts)

    calcCashflow l@(StepUpLease (LeaseInfo sd ot dp dr ob) lsu bal rt st) d _ =
      do 
        (l' ,pmts) <- patchBalance l -- `debug` ("1")
        let bal = getCurrentBal l'
        let p_dates = projectCfDates dp sd ot -- `debug` ("2")
        let cf_dates = lastN (succ rt) p_dates -- `debug` ("3")   -- `debug` ("P dates"++ show p_dates)
        let bals = tail $ scanl (-) bal pmts -- `debug` ("4"++show pmts)     -- `debug` ("PMTS->"++ show pmts) 
        return $ CF.CashFlowFrame (0,d,Nothing) $ cutBy Inc Future d (zipWith3 CF.LeaseFlow (tail cf_dates) bals pmts)

    getPaymentDates l@(RegularLease (LeaseInfo sd ot dp _ _) _ rt _) _
        = genSerialDates dp Inc sd ot 

    getPaymentDates l@(StepUpLease (LeaseInfo sd ot dp _ _) _ _ rt _) _
        = genSerialDates dp Inc sd ot 

    getOriginDate (StepUpLease (LeaseInfo sd ot dp _ _) _ _ rt _) = sd
    getOriginDate (RegularLease (LeaseInfo sd ot dp _ _) _ rt _)  = sd
    
    getRemainTerms (StepUpLease (LeaseInfo sd ot dp _ _) _ _ rt _) = rt
    getRemainTerms (RegularLease (LeaseInfo sd ot dp _ _) _ rt _)  = rt
    
    updateOriginDate (StepUpLease (LeaseInfo sd ot dp dr ob) lsu bal rt st) nd 
      = StepUpLease (LeaseInfo nd ot dp dr ob) lsu bal rt st
    updateOriginDate (RegularLease (LeaseInfo sd ot dp dr ob) bal rt st) nd 
      = RegularLease (LeaseInfo nd ot dp dr ob) bal rt st
      
    -- resetToOrig (StepUpLease (LeaseInfo sd ot dp dr ob) lsu bal rt st) 
    --   = fst . patchBalance $ StepUpLease (LeaseInfo sd ot dp dr ob) lsu bal ot st
    -- resetToOrig (RegularLease (LeaseInfo sd ot dp dr ob) bal rt st) 
    --   = fst . patchBalance $ RegularLease (LeaseInfo sd ot dp dr ob) bal ot st

    projCashflow l asOfDay (AP.LeaseAssump mDefault gapAssump rentAssump endType,_,_) mRates
      = let 
          -- (rc,rcCurve,mgTbl,gapDays,ed) = extractAssump (A.LeaseAssump gapAssump rentAssump) -- (0.0,mkTs [],([(0.0,0)],0),0,epocDate)-- `debug` ("7")
          pdates = getPaymentDates l 0  -- `debug` ("8")-- `debug` ("RCURVE"++show rcCurve)
        
          -- get the gap days between leases
          pickGapDays (AP.GapDays days) = days
          pickGapDays (AP.GapDaysByAmount tbl defaultDays) = getGapDaysByBalance l (tbl,defaultDays)
        
          newLeases = nextLeaseTill 
                        l
                        (rentAssump ,0.0,pickGapDays gapAssump) 
                        (last pdates) 
                        endType
                        []
        in
          do
            currentCf <- calcCashflow l asOfDay mRates
            newCfs <- sequenceA [ calcCashflow l asOfDay mRates | l <- newLeases ]  `debug` ("Current CF\n "++ show currentCf)
            let allTxns = view CF.cashflowTxn currentCf ++ (concat $ (view CF.cashflowTxn) <$> newCfs)
            let begBal = CF.buildBegBal allTxns
            return $ (CF.CashFlowFrame (begBal,asOfDay,Nothing) allTxns, Map.empty)  
        

    projCashflow a b c d = Left $ "Failed to match when proj lease with assumption >>" ++ show a ++ show b ++ show c ++ show d
    
    getCurrentBal l = case l of 
                        StepUpLease _ _ bal _ _ -> bal
                        RegularLease _ bal _ _-> bal

    getOriginRate (StepUpLease (LeaseInfo _ _ _ dr _) _ _ _ _) = fromRational $ toRational dr
    getOriginRate (RegularLease (LeaseInfo _ _ _ dr _) _ _ _) = fromRational $ toRational dr

    isDefaulted (StepUpLease _ _ _ rt Current) = False
    isDefaulted (StepUpLease _ _ _ rt _) = True
    isDefaulted (RegularLease _ _  rt Current) = False
    isDefaulted (RegularLease _ _  rt _) = True

    getOriginBal l = 
      let 
            _sd = case l of 
                RegularLease (LeaseInfo sd ot dp dr _) bal _ _ -> sd 
                StepUpLease (LeaseInfo sd ot dp dr _) _ bal _ _  -> sd 
      in  
        case calcCashflow l _sd Nothing of
            Right (CF.CashFlowFrame _ txns) -> CF.mflowBegBalance $ head txns
            Left _ -> 0

    splitWith (RegularLease (LeaseInfo sd ot dp dr ob) bal rt st ) rs
      = [ RegularLease (LeaseInfo sd ot dp dr ob) (mulBR bal ratio) rt st | ratio <- rs ] 
    splitWith (StepUpLease (LeaseInfo sd ot dp dr ob) stup bal rt st ) rs
      = [ StepUpLease (LeaseInfo sd ot dp dr ob) stup (mulBR bal ratio) rt st | ratio <- rs]

