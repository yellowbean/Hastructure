{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module AssetClass.Lease
  (Lease(..),LeaseInfo(..),accrueRentals,LeaseStepUp(..))
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

import Debug.Trace
debug = flip trace

type PeriodAmount = Balance

data LeaseInfo = LeaseInfo {
    startDate :: Date
    ,originTerm :: Int 
    ,paymentDates :: DatePattern
    ,originRental :: Amount
    }
    deriving (Show)

data LeaseStepUp = FlatRate DatePattern Rate
                 | ByRateCurve DatePattern [Rate]
    deriving (Show)


data Lease = RegularLease LeaseInfo Int 
           | StepUpLease LeaseInfo LeaseStepUp Int
    deriving (Show)

type LastAccuredDate = Date
type DailyRate = Balance
type AccuralPeriod = (Date,DailyRate)


accrueRentals :: [AccuralPeriod] -> [Date] -> LastAccuredDate -> [Amount] -> [Amount] -> [Amount]
accrueRentals _ [] _ _ payAmts = payAmts
accrueRentals ad@((accrueD,dr):accrueDs) pd@(payD:payDs) lastAccrueD accAmts payAmts
  |accrueD < payD = accrueRentals 
                      accrueDs 
                      pd
                      accrueD 
                      (accAmts++ [((fromRational (toRational (daysBetween lastAccrueD accrueD))) * dr)]) 
                      payAmts -- `debug` (">> acc amts->"++show accAmts++">>adding"++ show (mulBR (fromRational (toRational (daysBetween lastAccrueD accrueD))) dr ))
  |accrueD == payD = let 
                      _accAmt = ((fromRational (toRational (daysBetween lastAccrueD accrueD))) * dr)
                     in  
                      accrueRentals 
                      accrueDs 
                      payDs
                      accrueD 
                      [] 
                      (payAmts++[(sum (_accAmt:accAmts))])
  |otherwise = accrueRentals ad payDs lastAccrueD [] (payAmts++[(sum accAmts)])

type RentChangeRate = Rate
type RentChangeCurve = Ts
type TermChangeRate = Rate
type DayGap = Int

nextLease :: Lease -> (RentChangeCurve, TermChangeRate, DayGap) -> (Lease, Date)
nextLease l@(RegularLease (LeaseInfo sd ot dp dr) rt) (rcCurve,tc,gd) 
  = (RegularLease (LeaseInfo nextStartDate nextOriginTerm dp nextPmt) rt,nextEndDate) -- `debug` ("1+tc"++show (1+tc) ++">>"++ show (mulIR ot (1+tc)))
    where 
        leaseEndDate = last $ genSerialDates dp sd ot 
        nextStartDate = T.addDays (toInteger gd) leaseEndDate -- `debug` ("Gap Day ->"++ show gd)
        nextOriginTerm = round $ mulIR ot (1+tc) 
        nextEndDate = last $ genSerialDates dp nextStartDate (fromIntegral nextOriginTerm)
        yearsBetween = yearCountFraction DC_ACT_365F sd nextStartDate
        currentRateOnCurve = getValByDate rcCurve nextStartDate
        nextPmt = dr + mulBR dr currentRateOnCurve*(fromRational yearsBetween)

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

getGapDaysByBalance :: Lease -> ([(Amount,Int)],Int) -> Int 
getGapDaysByBalance l tbl@(rows,defaultVal) = 
    let 
        tbl = ThresholdTable rows 
        pmt = case l of 
                (RegularLease (LeaseInfo _ _ _ dr) _) -> dr
                (StepUpLease (LeaseInfo _ _ _ dr) _ _) -> dr
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

instance Asset Lease where 
    calcCashflow l@(RegularLease (LeaseInfo sd ot dp dr) rt) d =
        CF.CashFlowFrame $ zipWith CF.LeaseFlow (tail cf_dates) pmts -- `debug` ("CF Dates"++ show cf_dates++"PMTS->"++ show pmts)
      where 
        -- cf_dates = sliceDates (SliceAfterKeepPrevious d) $ [sd]++genSerialDates dp sd ot 
        cf_dates = sliceDates (SliceAfterKeepPrevious d) $ projectCfDates dp sd ot
        daysBetween = getIntervalDays cf_dates -- `debug` (">>>>>> genSerialDates"++ show (genSerialDates dp sd ot))
        pmts = [ (fromRational ((toRational dr) * (toRational ds))) | ds <- daysBetween] -- `debug` (">>>> Days between"++ show daysBetween)  --TODO to be simplified 

    calcCashflow l@(StepUpLease (LeaseInfo sd ot dp dr) lsu rt) d =
        CF.CashFlowFrame $ zipWith CF.LeaseFlow cf_dates pmts
      where 
        -- p_dates = genSerialDates dp sd ot
        p_dates = projectCfDates dp sd ot 
        -- cf_dates =  filter (> d) p_dates `debug` ("P dates"++ show p_dates)
        last_pay_date:cf_dates =  sliceDates (SliceAfterKeepPrevious d) p_dates `debug` ("P dates"++ show p_dates)
        accrueEndsAt = last cf_dates
        pmts = case lsu of 
                 (FlatRate _dp _r) ->
                   let 
                     a_dates = genSerialDatesTill2 II sd _dp accrueEndsAt
                     lastAccD:accrueDates = sliceDates (SliceAfterKeepPrevious d) a_dates
                     lengthFutureAccD = length accrueDates
                     lengthAccD= length a_dates
                     dailyRates = [ mulBR dr ((toRational (1+_r))^^x) | x <- [(lengthAccD - lengthFutureAccD - 1)..lengthAccD]] -- `debug` (">>LAD"++show lastAccD++">>"++show accrueDates)]
                     accruePeriods = zip accrueDates dailyRates 
                   in  
                     accrueRentals accruePeriods cf_dates lastAccD [] [] -- `debug` ("Acc P>>"++show accruePeriods++">> pay dates"++show cf_dates)
                 (ByRateCurve _dp _rs) -> 
                   let 
                     a_dates = genSerialDatesTill2 II sd _dp accrueEndsAt
                     lastAccD:accrueDates = sliceDates (SliceAfterKeepPrevious d) a_dates
                     factors = scanl (*) 1.0 $ [ (_r + 1)  | _r <- _rs] 
                     dailyRates =  drop ((length a_dates) - (length accrueDates) - 1) $ [ mulBR dr f | f <- factors ]
                     accruePeriods = zip accrueDates $ paddingDefault dr dailyRates (length accrueDates)
                   in 
                     accrueRentals accruePeriods cf_dates lastAccD [] [] -- `debug` ("AP->"++show accruePeriods)

    getPaymentDates l@(RegularLease (LeaseInfo sd ot dp pmt) rt) never_use 
        = genSerialDates dp sd ot 

    projCashflow l@(RegularLease (LeaseInfo sd ot dp pmt) rt) asOfDay assumps = 
        foldl CF.combineCashFlow currentCf newCfs  -- `debug` ("current cf->"++ show currentCf ++ "newCf>>"++show newCfs)
      where 
        currentCf = calcCashflow l asOfDay
        (rc,rcCurve,mgTbl,gapDays,ed) = extractAssump assumps (0.0,mkTs [],([(0.0,0)],0),0,epocDate)
        pdates = getPaymentDates l 0
        rcCurveToUse = if isTsEmpty rcCurve then 
                         mkTs [(epocDate,rc),(ed,rc)]
                       else 
                         rcCurve
        gapDaysFromTbl = getGapDaysByBalance l mgTbl 
        newLeases = nextLeaseTill l (rcCurveToUse,0.0,max gapDays gapDaysFromTbl) (last pdates) ed [] 
        newCfs = [ calcCashflow l asOfDay | l <- newLeases ]  -- `debug` ("new leases"++ show newLeases++ "MGap"++ show (gapDays,gapDaysFromTbl))
        -- projected contract 


$(deriveJSON defaultOptions ''LeaseInfo)
$(deriveJSON defaultOptions ''LeaseStepUp)
$(deriveJSON defaultOptions ''Lease)
