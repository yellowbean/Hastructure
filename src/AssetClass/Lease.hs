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
import Types hiding (getOriginDate)
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

getNewRental :: AP.LeaseAssetRentAssump -> Date -> Date -> LeaseRateCalc -> LeaseRateCalc
getNewRental (AP.BaseAnnualRate r) sd ed (ByDayRate dr dp) 
  = ByDayRate (mulBR dr (1 + yearCountFraction DC_ACT_365F sd ed * fromRational r)) dp
getNewRental (AP.BaseCurve rc) sd ed (ByDayRate dr dp) 
  = ByDayRate (mulBR dr (1 + yearCountFraction DC_ACT_365F sd ed * getValByDate rc Exc ed)) dp

getNewRental (AP.BaseAnnualRate r) sd ed (ByPeriodRental rental per) 
  = ByPeriodRental (mulBR rental (1 + yearCountFraction DC_ACT_365F sd ed * fromRational r)) per
getNewRental (AP.BaseCurve rc) sd ed (ByPeriodRental rental per) 
  = ByPeriodRental (mulBR rental (1 + yearCountFraction DC_ACT_365F sd ed * (fromRational (getValByDate rc Exc ed)))) per

calcEndDate :: Date -> Int -> LeaseRateCalc -> Date 
calcEndDate sd periods (ByDayRate _ dp) = last $ genSerialDates dp Exc sd periods
calcEndDate sd periods (ByPeriodRental _ per) = last $ genDates sd per periods

--  ByDayRate _ dp -> genSerialDates dp Exc (getOriginDate l) (ot + getTotalTerms l)
--  ByPeriodRental _ per -> genDates (getOriginDate l) per (ot + getTotalTerms l)



calcGapDays :: AP.LeaseAssetGapAssump -> Date -> Int
calcGapDays (AP.GapDays days) _ = days
calcGapDays (AP.GapDaysByCurve ts) d = round $ fromRational $ getValByDate ts Exc d 



-- ^ Generate next lease with new rental / term changes/ day gap
nextLease :: Lease -> (AP.LeaseAssetRentAssump, TermChangeRate, DayGap) -> (Lease, Date)
nextLease l@(RegularLease (LeaseInfo sd ot rental ob) bal rt _) (rAssump,tc,gd) 
  = let
        leaseEndDate = last $ getPaymentDates l 0
        nextStartDate = T.addDays (succ (toInteger gd)) leaseEndDate -- `debug` ("Gap Day ->"++ show gd)

        nextOriginTerm = round $ mulIR ot (1+tc) 
        nextEndDate = calcEndDate nextStartDate ot rental
        nextRental = getNewRental rAssump sd nextStartDate rental
        newBal =  -1
    in 
      (RegularLease (LeaseInfo nextStartDate nextOriginTerm nextRental ob) 
                    newBal nextOriginTerm Current,nextEndDate) -- `debug` ("1+tc"++show (1+tc) ++">>"++ show (mulIR ot (1+tc)))

nextLease l@(StepUpLease (LeaseInfo sd ot rental ob) lsteupInfo bal rt _) (rAssump,tc,gd) 
  = let 
        leaseEndDate = last $ getPaymentDates l 0
        nextStartDate = T.addDays (succ (toInteger gd)) leaseEndDate -- `debug` ("Gap Day ->"++ show gd)
        nextOriginTerm = round $ mulIR ot (1+tc) 
        nextEndDate = calcEndDate sd ot rental
        nextRental = getNewRental rAssump sd nextStartDate rental
        newBal = -1
    in
      (StepUpLease (LeaseInfo nextStartDate nextOriginTerm nextRental ob) 
                    lsteupInfo newBal nextOriginTerm Current,nextEndDate) --  `debug` ("leaseEndDate>>"++show leaseEndDate++">>>"++show (succ (toInteger gd)))

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


-- getGapDaysByBalance :: Lease -> ([(Amount,Int)],Int) -> Int 
-- getGapDaysByBalance l tbl@(rows,defaultVal)
--   = let 
--       tbl = ThresholdTable rows 
--       pmt = case l of 
--               (RegularLease (LeaseInfo _ _ _ dr _) _ _ _) -> dr
--               (StepUpLease (LeaseInfo _ _ _ dr _) _ _ _ _) -> dr
--     in 
--       fromMaybe  defaultVal $ lookupTable tbl Down (>= pmt)


-- ^ calculate the daily rate for a step up lease
-- TODO: factor rates to model the defaulted factors
calcPmts :: LeaseStepUp -> [Rate] -> Amount -> Either String [Amount] 
calcPmts (FlatRate _r) fs amt = Right (scanl mulBR amt (replicate (length fs) _r))
calcPmts (ByFlatAmount _amt) fs amt = Right (scanl (+) amt (replicate (length fs) _amt))
calcPmts (ByRateCurve rs) fs amt 
  | length rs /= length fs = Left "ByRateCurve: the rate curve should be the same length as remain pay dates"
  | otherwise = Right $ scanl mulBR amt rs
calcPmts (ByAmountCurve amts) fs amt 
  | length amts /= length fs = Left "ByAmountCurve: the rate curve should be the same length as remain pay dates"
  | otherwise = Right $ scanl (+) amt amts



-- ^ return a lease contract with opening balance and a payment cashflow on each payment date
patchBalance :: Lease -> Either String (Lease,[Amount]) 
patchBalance l@(RegularLease (LeaseInfo sd ot (ByDayRate dr dp) ob) bal rt st)
  = let 
      cf_dates = sd:getPaymentDates l 0
      pmts = [ fromRational (mulBInt dr ds) | ds <- getIntervalDays cf_dates ]
      new_bal = sum pmts -- `debug` ("cf_date" ++ show cf_dates)
    in
      Right (RegularLease (LeaseInfo sd ot (ByDayRate dr dp) ob) new_bal rt st, pmts)

patchBalance l@(RegularLease (LeaseInfo sd ot (ByPeriodRental rental per) ob) bal rt st)
  = let 
      cf_dates = lastN (succ rt) $ getPaymentDates l 0
      intervals = daysInterval cf_dates
      factors = replicate (pred ot) 1.0
      pmts = replicate rt rental
      new_bal = sum pmts 
    in 
      do 
        return (RegularLease (LeaseInfo sd ot (ByPeriodRental rental per) ob) new_bal rt st,pmts) -- `debug` ("daily payments" ++ show pmts)


patchBalance l@(StepUpLease (LeaseInfo sd ot (ByDayRate dr p) ob) lsu bal rt st)
  = let 
      cfDates = sd:getPaymentDates l 0
      intervals = daysInterval cfDates
      factors = replicate (pred ot) 1.0
    in 
      do 
        dailyRentals <- calcPmts lsu factors dr
        let pmts = [ fromRational (mulBInteger r d) | (d,r) <- zip intervals dailyRentals ] 
        let new_bal = sum pmts 
        return (StepUpLease (LeaseInfo sd ot (ByDayRate dr p) ob) lsu new_bal rt st,pmts) -- `debug` ("daily payments" ++ show pmts)


instance Asset Lease where 
    calcCashflow l@(RegularLease (LeaseInfo sd ot or ob) _ rt st) d _ =
      do 
        (l',pmts) <- patchBalance l
        let bal = getCurrentBal l' -- `debug` ("payments"++ show pmts)
        let pDates = lastN rt $ getPaymentDates l 0 
        let bals = tail $ scanl (-) bal pmts -- `debug` ("pDates "++ show pDates)
        return $ CF.CashFlowFrame (0,d,Nothing) $ cutBy Inc Future d (zipWith3 CF.LeaseFlow pDates bals pmts)

    calcCashflow l@(StepUpLease (LeaseInfo sd ot or ob) lsu bal rt st) d _ =
      do 
        (l' ,pmts) <- patchBalance l
        let bal = getCurrentBal l'
        let pDates = (lastN rt) $ getPaymentDates l 0
        let bals = tail $ scanl (-) bal pmts
        return $ CF.CashFlowFrame (0,d,Nothing) $ cutBy Inc Future d (zipWith3 CF.LeaseFlow  pDates bals ((lastN rt) pmts))


    getOriginInfo (StepUpLease lInfo lsteupInfo bal rt st) =  lInfo
    getOriginInfo (RegularLease lInfo bal rt st) = lInfo
      
    getOriginDate (StepUpLease (LeaseInfo sd _ _ _) _ _ _ _) = sd
    getOriginDate (RegularLease (LeaseInfo sd _ _ _) _ _ _)  = sd

    getPaymentDates l ot
      = case originRental (getOriginInfo l) of
          ByDayRate _ dp -> genSerialDates dp Exc (getOriginDate l) (ot + getTotalTerms l)
          ByPeriodRental _ per -> genDates (getOriginDate l) per (ot + getTotalTerms l)
    
    getRemainTerms (StepUpLease _ _ _ rt _) = rt
    getRemainTerms (RegularLease _ _ rt _)  = rt

    getTotalTerms (RegularLease (LeaseInfo _ ot _ _) _ _ _) = ot
    getTotalTerms (StepUpLease (LeaseInfo _ ot _ _) _ _ _ _) = ot
    
    updateOriginDate (StepUpLease (LeaseInfo sd ot rental ob) lsu bal rt st) nd 
      = StepUpLease (LeaseInfo nd ot rental ob) lsu bal rt st
    updateOriginDate (RegularLease (LeaseInfo sd ot rental ob) bal rt st) nd 
      = RegularLease (LeaseInfo nd ot rental ob) bal rt st
      
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
          -- TODO
          pickGapDays (AP.GapDaysByAmount tbl defaultDays) = 0
        
          newLeases = nextLeaseTill 
                        l
                        (rentAssump ,0.0,pickGapDays gapAssump) 
                        (last pdates) 
                        endType
                        []
        in
          do
            currentCf <- calcCashflow l asOfDay mRates
            newCfs <- sequenceA [ calcCashflow l asOfDay mRates | l <- newLeases ] --  `debug` ("Current CF\n "++ show currentCf)
            let allTxns = view CF.cashflowTxn currentCf ++ (concat $ (view CF.cashflowTxn) <$> newCfs)
            let begBal = CF.buildBegBal allTxns
            return $ (CF.CashFlowFrame (begBal,asOfDay,Nothing) allTxns, Map.empty)  
        

    projCashflow a b c d = Left $ "Failed to match when proj lease with assumption >>" ++ show a ++ show b ++ show c ++ show d
    
    getCurrentBal l = case l of 
                        StepUpLease _ _ bal _ _ -> bal
                        RegularLease _ bal _ _-> bal

    -- getOriginRate (StepUpLease (LeaseInfo _ _ _ dr _) _ _ _ _) = fromRational $ toRational dr
    -- getOriginRate (RegularLease (LeaseInfo _ _ _ dr _) _ _ _) = fromRational $ toRational dr
    getOriginRate _ = 0.0

    isDefaulted (StepUpLease _ _ _ rt Current) = False
    isDefaulted (StepUpLease _ _ _ rt _) = True
    isDefaulted (RegularLease _ _  rt Current) = False
    isDefaulted (RegularLease _ _  rt _) = True

    getOriginBal l = 
      let 
            _sd = case l of 
                RegularLease (LeaseInfo sd _ _ _) bal _ _ -> sd 
                StepUpLease (LeaseInfo sd _ _ _) _ bal _ _  -> sd 
      in  
        case calcCashflow l _sd Nothing of
            Right (CF.CashFlowFrame _ txns) -> CF.mflowBegBalance $ head txns
            Left _ -> 0

    splitWith (RegularLease (LeaseInfo sd ot dr ob) bal rt st ) rs
      = [ RegularLease (LeaseInfo sd ot dr ob) (mulBR bal ratio) rt st | ratio <- rs ] 
    splitWith (StepUpLease (LeaseInfo sd ot dr ob) stup bal rt st ) rs
      = [ StepUpLease (LeaseInfo sd ot dr ob) stup (mulBR bal ratio) rt st | ratio <- rs]

