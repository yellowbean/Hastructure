{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AssetClass.Lease
  (Lease(..),accrueRentals,projCashflow,updateOriginDate)
  where

import qualified Data.Time as T
import qualified Cashflow as CF -- (Cashflow,Amount,Interests,Principals)
import qualified Assumptions as AP
import Asset
import Types
import Lib
import Util

import qualified Data.Map as Map
import Data.List
import Data.Aeson hiding (json)
import Language.Haskell.TH
import Data.Aeson.TH
import Data.Aeson.Types
import GHC.Generics

import AssetClass.AssetBase

import Debug.Trace
debug = flip trace

type PeriodAmount = Balance
type CapRate = Rate
type RentChangeRate = Rate
type RentChangeCurve = Ts
type TermChangeRate = Rate
type DayGap = Int
type LastAccuredDate = Date
type AccuralAmount = Balance


accrueRentalBetween :: Date -> Date -> Ts -> Amount
accrueRentalBetween sd ed rc@(LeftBalanceCurve tps)
  = fromRational $ sum $ zipWith (*) ints vs -- `debug` ("ds from range"++show sd++show ed++">>ints"++show ints++">>"++show vs)
   where 
     ds = [sd]++(getDates $ sliceBy EE sd ed tps)++[ed]  -- `debug` ("in ds with rc->"++show rc++">>"++show tps++">>"++show sd++">>"++show ed)
     vs = init $ getValByDates rc Inc ds  
     ints = map fromIntegral $ daysInterval ds -- `debug` ("ds value ->"++show ds++"->>"++show vs)

accrueRentals :: Ts -> [Date] -> LastAccuredDate -> [Amount] -> [Amount]
accrueRentals _ [] _ payAmts = payAmts
accrueRentals rc@(LeftBalanceCurve tps) pd@(payD:payDs) accrueCutoff payAmts
  = accrueRentals
      rc
      payDs
      payD
      (payAmts ++ [accrueRentalBetween accrueCutoff payD rc]) -- `debug` ("ACCRENTALS"++ show accrueCutoff++">>"++show payD++"rc+"++ show rc)

nextLease :: Lease -> (RentChangeCurve, TermChangeRate, DayGap) -> (Lease, Date)
nextLease l@(RegularLease (LeaseInfo sd ot dp dr) bal rt _) (rcCurve,tc,gd) 
  = (RegularLease (LeaseInfo nextStartDate nextOriginTerm dp nextDailyRate) newBal rt Current,nextEndDate) -- `debug` ("1+tc"++show (1+tc) ++">>"++ show (mulIR ot (1+tc)))
    where 
        leaseEndDate = last $ projectCfDates dp sd ot 
        nextStartDate = T.addDays (succ (toInteger gd)) leaseEndDate -- `debug` ("Gap Day ->"++ show gd)
        nextOriginTerm = round $ mulIR ot (1+tc) 
        nextEndDate = last $ genSerialDates dp nextStartDate (fromIntegral nextOriginTerm)
        yearsBetween = yearCountFraction DC_ACT_365F sd nextStartDate
        currentRateOnCurve = getValByDate rcCurve Exc nextStartDate
        nextDailyRate = dr + mulBR dr currentRateOnCurve*(fromRational yearsBetween)
        newBal =  fromRational $ mulBInteger nextDailyRate $ daysBetween nextStartDate nextEndDate

nextLease l@(StepUpLease (LeaseInfo sd ot dp dr) lsteupInfo bal rt _) (rcCurve,tc,gd) 
  = (StepUpLease (LeaseInfo nextStartDate nextOriginTerm dp nextDailyRate) lsteupInfo newBal rt Current,nextEndDate) --  `debug` ("leaseEndDate>>"++show leaseEndDate++">>>"++show (succ (toInteger gd)))
    where 
        leaseEndDate = last $ projectCfDates dp sd ot 
        nextStartDate = T.addDays (succ (toInteger gd)) leaseEndDate -- `debug` ("Gap Day ->"++ show gd)
        nextOriginTerm = round $ mulIR ot (1+tc) 
        nextEndDate = last $ genSerialDates dp nextStartDate (fromIntegral nextOriginTerm)
        yearsBetween = yearCountFraction DC_ACT_365F sd nextStartDate
        currentRateOnCurve = getValByDate rcCurve Exc nextStartDate
        nextDailyRate = dr + mulBR dr currentRateOnCurve*(fromRational yearsBetween)
        newBal = -1


nextLeaseTill :: Lease -> (RentChangeCurve, TermChangeRate, DayGap) -> Date -> Date -> [Lease] -> [Lease]
nextLeaseTill l (rsc,tc,mg) lastDate ed accum 
  | lastDate >= ed = accum 
  | otherwise = nextLeaseTill new_lease (rsc,tc,mg) new_lastDate ed (accum++[new_lease])
                where 
                 (new_lease,new_lastDate) = nextLease l (rsc,tc,mg) 

extractAssump :: [AP.AssumptionBuilder] -> (Rate,Ts,([(Amount,Int)],Int),DayGap,Date)-> (Rate,Ts,([(Amount,Int)],Int),DayGap,Date)
extractAssump [] r = r
extractAssump (ap:aps) (a,b,c,d,e) 
  = case ap of 
      (AP.LeaseProjectionEnd ed) -> extractAssump aps (a,b,c,d,ed)
      (AP.LeaseGapDays mg) -> extractAssump aps (a,b,c,mg,e)
      (AP.LeaseBaseAnnualRate r) -> extractAssump aps (r,b,c,d,e)
      (AP.LeaseBaseCurve ts) -> extractAssump aps (a,ts,c,d,e)
      (AP.LeaseGapDaysByAmount tbl rest) -> extractAssump aps (a,b,(tbl,rest),d,e)
      _ -> extractAssump aps (a,b,c,d,e)

getGapDaysByBalance :: Lease -> ([(Amount,Int)],Int) -> Int 
getGapDaysByBalance l tbl@(rows,defaultVal) = 
    let 
        tbl = ThresholdTable rows 
        pmt = case l of 
                (RegularLease (LeaseInfo _ _ _ dr) _ _ _) -> dr
                (StepUpLease (LeaseInfo _ _ _ dr) _ _ _ _) -> dr
    in 
        lookupTable tbl DownwardInclude pmt defaultVal

projectCfDates :: DatePattern -> Date -> Int -> [Date]
projectCfDates dp sd ot
  = let  
        cf_dates_proj = genSerialDates dp sd ot
    in 
        if head cf_dates_proj == sd then 
            genSerialDates dp sd (succ ot)
        else
            [sd]++cf_dates_proj


patchBalance :: Lease -> (Lease,[Amount]) 
patchBalance (RegularLease (LeaseInfo sd ot dp dr) bal rt st)
  = let 
      cf_dates = lastN (succ rt) $ projectCfDates dp sd ot
      pmts = [ fromRational (mulBInt dr ds) | ds <- getIntervalDays cf_dates ]
      new_bal = sum pmts -- `debug` ("cf_date" ++ show cf_dates)
    in
      (RegularLease (LeaseInfo sd ot dp dr) new_bal rt st, pmts)

patchBalance (StepUpLease (LeaseInfo sd ot dp dr) lsu bal rt st)
  = let 
      p_dates = projectCfDates dp sd ot 
      cf_dates = lastN (succ rt) $ projectCfDates dp sd ot
      last_pay_date = head cf_dates
      next_pay_date = head $ tail cf_dates
      accrueEndsAt = last cf_dates -- `debug` ("0-0")
      pmts = case lsu of 
               (FlatRate _dp _r) ->
                 let 
                   a_dates = genSerialDatesTill2 II sd _dp accrueEndsAt -- `debug` ("0-1")
                   accrueDates = sliceDates (SliceOnAfterKeepPrevious next_pay_date) a_dates
                   lengthFutureAccD = length accrueDates
                   lengthAccD = length a_dates
                   dailyRates = [ (mulBR dr ((toRational (1+_r))^^x)) | x <- [(lengthAccD - lengthFutureAccD)..lengthAccD]] -- `debug` ("accd length"++show lengthAccD++"futureAccd"++show lengthFutureAccD)
                   rate_curve = LeftBalanceCurve [ TsPoint d v | (d,v) <- zip accrueDates dailyRates ]
                 in  
                   accrueRentals rate_curve (tail cf_dates) last_pay_date []  -- `debug` (">>>> rentals")-- `debug` ("Using curve"++show rate_curve)
               (ByRateCurve _dp _rs) -> 
                 let 
                   a_dates = genSerialDatesTill2 II sd _dp accrueEndsAt
                   accrueDates = sliceDates (SliceOnAfterKeepPrevious next_pay_date) a_dates
                   factors = scanl (*) 1.0 $ [ _r + 1  | _r <- _rs] -- `debug` ("Slice acc dates"++ show a_dates++">>"++show next_pay_date) 
                   dailyRatesCurve = [ mulBR dr f | f <- factors ]
                   dailyRates =  paddingDefault (last dailyRatesCurve) dailyRatesCurve (length accrueDates)-- `debug` (">>>>ACCRUEDATES"++show accrueDates)
                   rate_curve = LeftBalanceCurve [ TsPoint d v | (d,v) <- zip accrueDates dailyRates ]
                 in 
                   accrueRentals rate_curve (tail cf_dates) last_pay_date [] -- `debug` ("Using curve->"++show rate_curve++">>"++ show last_pay_date)
      new_bal = sum pmts  -- `debug` ("Patch balance pmts"++ show pmts)
    in 
      (StepUpLease (LeaseInfo sd ot dp dr) lsu new_bal rt st,pmts)


instance Asset Lease where 
    calcCashflow l@(RegularLease _ _ _ _) d =
        CF.CashFlowFrame flows  -- `debug` ("FLows for regular"++show flows)
      where 
        (RegularLease (LeaseInfo sd ot dp dr) bal rt st,pmts) = patchBalance l
        cf_dates = lastN (succ rt) $ projectCfDates dp sd ot
        daysBetween = getIntervalDays cf_dates -- `debug` (">>>>>> genSerialDates"++ show cf_dates)
        bals = tail $ scanl (-) bal pmts -- `debug` ("PMTS for regular"++show pmts)
        (_,flows) = splitByDate 
                      (zipWith3 CF.LeaseFlow (tail cf_dates) bals pmts)
                      d
                      EqToRight

    calcCashflow l@(StepUpLease _ _ _ _ _) d =
        CF.CashFlowFrame flows -- `debug` ("5_01"++show flows)
      where 
        (StepUpLease (LeaseInfo sd ot dp dr) lsu bal rt st,pmts) = patchBalance l `debug` ("1")
        p_dates = projectCfDates dp sd ot -- `debug` ("2")
        cf_dates = lastN (succ rt) p_dates -- `debug` ("3")   -- `debug` ("P dates"++ show p_dates)
        bals = tail $ scanl (-) bal pmts -- `debug` ("4"++show pmts)     -- `debug` ("PMTS->"++ show pmts) 
        (_,flows) = splitByDate 
                      (zipWith3 CF.LeaseFlow (tail cf_dates) bals pmts)
                      d
                      EqToRight -- `debug` ("5"++show bals++">>"++show pmts)

    getPaymentDates l@(RegularLease (LeaseInfo sd ot dp _) _ rt _) _
        = genSerialDates dp sd ot 

    getPaymentDates l@(StepUpLease (LeaseInfo sd ot dp _) _ _ rt _) _
        = genSerialDates dp sd ot 

    getOriginDate (StepUpLease (LeaseInfo sd ot dp _) _ _ rt _) = sd
    getOriginDate (RegularLease (LeaseInfo sd ot dp _) _ rt _)  = sd
    
    getRemainTerms (StepUpLease (LeaseInfo sd ot dp _) _ _ rt _) = rt
    getRemainTerms (RegularLease (LeaseInfo sd ot dp _) _ rt _)  = rt
    
    updateOriginDate (StepUpLease (LeaseInfo sd ot dp dr) lsu bal rt st) nd 
      = (StepUpLease (LeaseInfo nd ot dp dr) lsu bal rt st)
    updateOriginDate (RegularLease (LeaseInfo sd ot dp dr) bal rt st) nd 
      = (RegularLease (LeaseInfo nd ot dp dr) bal rt st)

    projCashflow l asOfDay assumps = 
        foldl CF.combineCashFlow currentCf newCfs  -- `debug` ("current cf->"++ show currentCf ++ "newCf>>"++show newCfs)
      where 
        currentCf = calcCashflow l asOfDay-- `debug` ("6")
        (rc,rcCurve,mgTbl,gapDays,ed) = extractAssump assumps (0.0,mkTs [],([(0.0,0)],0),0,epocDate)-- `debug` ("7")
        pdates = getPaymentDates l 0  -- `debug` ("8")-- `debug` ("RCURVE"++show rcCurve)
        rcCurveToUse = if isTsEmpty rcCurve then 
                         mkTs [(epocDate,rc),(ed,rc)]
                       else 
                         rcCurve
        gapDaysFromTbl = getGapDaysByBalance l mgTbl--  `debug` ("9")
        newLeases = nextLeaseTill l (rcCurveToUse,0.0,max gapDays gapDaysFromTbl) (last pdates) ed [] -- `debug` ("10")
        newCfs = [ calcCashflow l asOfDay | l <- newLeases ]--  `debug` ("11")-- `debug` ("new leases"++ show newLeases++ "MGap"++ show (gapDays,gapDaysFromTbl))

    getCurrentBal l = case l of 
                        StepUpLease _ _ bal _ _ -> bal
                        RegularLease _ bal _ _-> bal

    getOriginRate l = case l of 
                        StepUpLease (LeaseInfo sd ot dp dr) _ bal _ _ -> fromRational $ toRational dr
                        RegularLease (LeaseInfo sd ot dp dr) bal _ _ ->  fromRational $ toRational dr

    isDefaulted (StepUpLease _ _ _ rt Current) = False
    isDefaulted (StepUpLease _ _ _ rt _) = True
    isDefaulted (RegularLease _ _  rt Current) = False
    isDefaulted (RegularLease _ _  rt _) = True

    getOriginBal l = 
      let 
            _sd = case l of 
                RegularLease (LeaseInfo sd ot dp dr) bal _ _ -> sd 
                StepUpLease (LeaseInfo sd ot dp dr) _ bal _ _  -> sd 
            CF.CashFlowFrame txns = calcCashflow l _sd
        in  
            CF.mflowBegBalance $ head txns

    splitWith (RegularLease (LeaseInfo sd ot dp dr) bal rt st ) rs
      = [ RegularLease (LeaseInfo sd ot dp dr) (mulBR bal ratio) rt st | ratio <- rs ] 
    splitWith (StepUpLease (LeaseInfo sd ot dp dr) stup bal rt st ) rs
      = [ StepUpLease (LeaseInfo sd ot dp dr) stup (mulBR bal ratio) rt st | ratio <- rs]

