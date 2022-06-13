{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Asset (Mortgage(..),Pool(..),OriginalInfo(..),calc_p_i_flow
       ,aggPool,calcCashflow,getCurrentBal,getOriginBal,runPool
       ,RateType(..)
) where

import Data.Time (Day)
import qualified Data.Time as T
import Lib (Period(..), Balance,calcInt,Dates,DayCount(..),calcIntRate,genDates
           ,Balance,Rate,Ts(..),Spread,Index(..),periodRateFromAnnualRate)
import qualified Cashflow as CF -- (Cashflow,Amount,Interests,Principals)
import qualified Assumptions as A
import qualified Data.Map as Map
import Data.List
import Data.Aeson hiding (json)
import Language.Haskell.TH
import Data.Aeson.TH
import Data.Aeson.Types

type PrepaymentRate = Float
type DefaultRate = Float
type RecoveryRate = Float


class Asset a where
  calcCashflow :: a -> CF.CashFlowFrame
  getCurrentBal :: a -> Float
  getOriginBal :: a -> Float
  getOriginRate :: a -> Float
  getPaymentDates :: a -> [T.Day]
  projCashflow :: a -> [A.AssumptionBuilder] -> CF.CashFlowFrame


data Pool a = Pool {assets :: [a]}
                    deriving (Show)

calcPmt :: Float -> Float -> Int -> Float
calcPmt bal periodRate periods = 
    bal * (periodRate * (1+periodRate)^periods)/((1+periodRate)^periods-1)

replace :: [a] -> Int -> a -> [a]
replace xs i e = case splitAt i xs of
   (before, _:after) -> before ++ e: after
   _ -> xs

type Floor = Float
data RateType = Fix Rate
              | Floater Index Spread Rate Period (Maybe Floor) 
              -- index, spread, initial-rate reset interval,floor
              deriving (Show)

data OriginalInfo = OriginalInfo {
    originBalance::Float
    ,originRate:: RateType
    ,originTerm:: Int
    ,period:: Period
    ,startDate :: Day} deriving (Show)

data Mortgage = Mortgage OriginalInfo Balance Rate Int
                deriving (Show)

instance Asset Mortgage  where
  calcCashflow m@(Mortgage (OriginalInfo ob or ot p sd )  _bal _rate _term) =
      CF.CashFlowFrame $ zipWith7
                            CF.MortgageFlow
                              cf_dates
                              b_flow
                              prin_flow
                              int_flow
                              (replicate l 0.0)
                              (replicate l 0.0)
                              (replicate l 0.0)
    where
      orate = getOriginRate m
      pmt = calcPmt ob (periodRateFromAnnualRate p orate) ot
      cf_dates = getPaymentDates m
      l = length cf_dates
      (b_flow,prin_flow,int_flow) = calc_p_i_flow _bal pmt cf_dates _rate

  getCurrentBal (Mortgage x _bal _ _) = _bal
  getOriginBal (Mortgage (OriginalInfo _bal _ _ _ _  ) _ _ _ ) = _bal
  getOriginRate (Mortgage (OriginalInfo _ or _ _ _  ) _ _ _ )
   = case or of
       Fix _r -> _r
       Floater _ _ _r _ Nothing -> _r
       Floater _ _ _r _ (Just floor) -> (max _r floor)
  getPaymentDates (Mortgage (OriginalInfo _ _ ot p sd) _ _ _ ) = genDates sd p ot
  projCashflow m@(Mortgage (OriginalInfo ob or ot p sd) _ _ _ ) assumps = 
    CF.CashFlowFrame $ _projCashflow [] ob sd cf_dates def_rates ppy_rates (replicate cf_recovery_length 0.0)
    where
      cf_dates = getPaymentDates m
      cf_dates_length = length cf_dates
      cf_recovery_length = cf_dates_length + recovery_lag
      (def_rates,ppy_rates,recovery_rate,recovery_lag) = buildAssumpCurves assumps 
                               (replicate cf_dates_length 0.0) 
                               (replicate cf_dates_length 0.0) 
                               0
                               0
      orate = getOriginRate m
      initPmt = calcPmt ob (periodRateFromAnnualRate p orate) ot

      _projCashflow 
           trs _bal _last_date (_pdate:_pdates) (_def_rate:_def_rates) (_ppy_rate:_ppy_rates) (_rec_amt:_rec_amts)
           | _bal > 0.01 = _projCashflow (trs++[tr]) _end_bal _pdate _pdates _def_rates _ppy_rates (replace _rec_amts recovery_lag _new_rec)
           | otherwise = trs      
         where
            _remain_terms = length _pdates
            _new_default = _bal * _def_rate
            _new_bal_after_default = _bal - _new_default 
            _new_prepay = _new_bal_after_default * _ppy_rate
            _new_bal_after_ppy = _new_bal_after_default - _new_prepay
            _new_int = _new_bal_after_ppy * calcIntRate _last_date _pdate orate ACT_360
            _pmt = calcPmt _new_bal_after_ppy (periodRateFromAnnualRate p orate) _remain_terms
            _new_prin = _pmt - _new_int
            _new_rec = _new_default * recovery_rate
            _end_bal = _new_bal_after_ppy - _new_prin
            tr = CF.MortgageFlow _pdate _end_bal _new_prin _new_int _new_prepay _new_default _rec_amt
      
      _projCashflow trs _bal _last_date [] _ _ _ = trs
      
        
      buildAssumpCurves (assump:assumps) _def_rates _ppy_rates _recovery_rate _recovery_lag = 
         case assump of 
             A.DefaultConstant r -> 
                 buildAssumpCurves assumps (replicate cf_dates_length r) _ppy_rates _recovery_rate _recovery_lag
             A.PrepaymentConstant r -> 
                 buildAssumpCurves assumps _def_rates (replicate cf_dates_length r) _recovery_rate _recovery_lag
             A.Recovery (rr,rl) -> buildAssumpCurves assumps _def_rates _ppy_rates rr rl
             -- Recovery (recoveryRate,recoveryLag) -> 
             _ -> buildAssumpCurves assumps _def_rates _ppy_rates _recovery_rate _recovery_lag
      buildAssumpCurves [] _def_rates _ppy_rates _recovery_rate _recovery_lag = (_def_rates,_ppy_rates,_recovery_rate,_recovery_lag)


tm = Mortgage (OriginalInfo 10000 (Fix 0.08) 5 Monthly (T.fromGregorian 2022 1 1))
     10000 0.08 5

tmcf = calcCashflow tm 
tmcf2 = projCashflow tm [A.DefaultConstant 0.015]

tmF = Mortgage (OriginalInfo 10000 (Floater LIBOR1M 0.02 0.075 Monthly Nothing) 5 Monthly (T.fromGregorian 2022 1 1))
     10000 0.08 5
tmFf = projCashflow tm [A.InterestRateConstant (LIBOR1M,0.07)]

_calc_p_i_flow :: Float -> [Balance] -> [Float] -> [Float] -> [Rate] -> (CF.Balances,CF.Principals,CF.Interests)
_calc_p_i_flow pmt bals ps is [] = (bals,ps,is)
_calc_p_i_flow pmt bals ps is (r:rs)
  | (last bals) < 0.01  =  (bals,ps,is)
  | otherwise
    = _calc_p_i_flow pmt (bals++[new_bal]) (ps++[new_prin]) (is++[new_int]) rs
      where
        new_int = (last bals) * r
        new_prin = pmt - new_int
        new_bal = (last bals) - new_prin

calc_p_i_flow :: Balance -> Float -> Dates -> Rate -> (CF.Balances,CF.Principals,CF.Interests)
calc_p_i_flow bal pmt dates r =
  _calc_p_i_flow pmt [bal] [] [] period_r
    where
      size = length dates
      period_r = [ calcIntRate (dates!!d) (dates!!(d+1)) r ACT_360 | d <- [0..size-2]]

runPool :: Asset a => [a] -> (Maybe [A.AssumptionBuilder])-> [CF.CashFlowFrame]
runPool as Nothing = map calcCashflow as
runPool assets (Just assumps) 
  = map (\x -> (projCashflow x assumps)) assets

aggPool :: [CF.CashFlowFrame]  -> CF.CashFlowFrame
aggPool asflows = foldr1 CF.combine asflows


$(deriveJSON defaultOptions ''Mortgage)
$(deriveJSON defaultOptions ''OriginalInfo)
$(deriveJSON defaultOptions ''RateType)
$(deriveJSON defaultOptions ''Pool)


