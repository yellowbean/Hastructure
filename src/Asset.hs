{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Asset (Mortgage(..),Pool(..),OriginalInfo(..),calc_p_i_flow
       ,aggPool,calcCashflow,getCurrentBal,getOriginBal,runPool
       ,AssumptionBuilder(..)
) where

import Control.Lens
import Data.Time (Day)
import qualified Data.Time as T
import Lib (Period(..),Rate, Balance,calcInt,Dates,DayCount(..),calcIntRate,genDates
           ,Balance,Rate,Ts(..))
import qualified Cashflow as CF -- (Cashflow,Amount,Interests,Principals)

import qualified Data.Map as Map
import Data.List
import Data.Aeson hiding (json)
import Language.Haskell.TH
import Data.Aeson.TH
import Data.Aeson.Types

type PrepaymentRate = Float
type DefaultRate = Float
type RecoveryRate = Float

data AssumptionBuilder =  MortgageByAge ([Int],[Float])
                | MortgageByRate ([Float],[Float])
                | PrepaymentConstant Float
                | DefaultConstant Float
                | Recovery (Rate,Int)
                | LinearTo Int Float
                deriving (Show)

data AssumptionEffect = PrepaymentCurve [(Int,Float)]
                      | DefaultCurve [(Int,Float)]
  deriving (Show)

class Asset a where
  calcCashflow :: a -> CF.CashFlowFrame
  getCurrentBal :: a -> Float
  getOriginBal :: a -> Float
  getPaymentDates :: a -> [T.Day]
  projCashflow :: a -> [AssumptionBuilder] -> CF.CashFlowFrame


data Pool a = Pool {assets :: [a]}
                    deriving (Show)

calcPmt :: Float -> Float -> Int -> Float
calcPmt bal periodRate periods = 
    bal * (periodRate * (1+periodRate)^periods)/((1+periodRate)^periods-1)

data OriginalInfo = OriginalInfo {
    originBalance::Float
    ,originRate::Float
    ,originTerm:: Int
    ,period:: Period
    ,startDate :: Day} deriving (Show)

data Mortgage = Mortgage OriginalInfo Balance Rate Int
                deriving (Show)

instance Asset Mortgage  where
  calcCashflow m@(Mortgage (OriginalInfo ob or ot p sd )  _bal _rate _term) =
      CF.CashFlowFrame $ zipWith6
                            CF.MortgageFlow
                              cf_dates
                              b_flow
                              prin_flow
                              int_flow
                              (replicate l 0.0)
                              (replicate l 0.0)
    where
      pmt = calcPmt ob or ot
      cf_dates = getPaymentDates m
      l = length cf_dates
      (b_flow,prin_flow,int_flow) = calc_p_i_flow _bal pmt cf_dates _rate

  getCurrentBal (Mortgage x _bal _ _) = _bal
  getOriginBal (Mortgage (OriginalInfo _bal _ _ _ _  ) _ _ _ ) = _bal
  getPaymentDates (Mortgage (OriginalInfo _ _ ot p sd) _ _ _ ) = genDates sd p ot
  projCashflow m@(Mortgage (OriginalInfo ob or ot p sd) _ _ _ ) assumps = 
    CF.CashFlowFrame $ _projCashflow [] ob sd cf_dates 1.0 def_rates ppy_rates
    where
      cf_dates = getPaymentDates m
      cf_dates_length = length cf_dates
      (def_rates,ppy_rates) = buildAssumpCurves assumps 
                               (replicate cf_dates_length 0.0) 
                               (replicate cf_dates_length 0.0) 
      initPmt = calcPmt ob or ot 

      _projCashflow trs _bal _last_date (_pdate:_pdates) pmt_factor 
         (_def_rate:_def_rates) (_ppy_rate:_ppy_rates) = 
         _projCashflow (trs++[tr]) _new_bal _pdate _pdates _new_pmt_factor _def_rates _ppy_rates
         where
            _pmt = pmt_factor * initPmt
            _new_int = calcIntRate _last_date _pdate  or ACT_360  
            _new_prin = _pmt - _new_int
            _new_prepay = _bal * _ppy_rate
            _new_default = ( _bal - _new_prepay ) * _def_rate
            _new_bal = _bal - _new_prin - _new_prepay - _new_default
            _new_pmt_factor = pmt_factor * (1 - _ppy_rate) * (1 - _def_rate)
            _new_rec = 0.0
            tr = CF.MortgageFlow _pdate _new_bal _new_prin _new_int _new_prepay _new_rec
      
      _projCashflow trs _bal _last_date [] pmt_factor _ _ = trs
      
        
      buildAssumpCurves (assump:assumps) _def_rates _ppy_rates = 
         case assump of 
             DefaultConstant r -> buildAssumpCurves assumps (replicate cf_dates_length r) _ppy_rates
             PrepaymentConstant r -> buildAssumpCurves assumps _def_rates (replicate cf_dates_length r)
             -- Recovery (recoveryRate,recoveryLag) -> 
             _ -> buildAssumpCurves assumps _def_rates _ppy_rates
      buildAssumpCurves [] _def_rates _ppy_rates = (_def_rates,_ppy_rates)


tm = Mortgage (OriginalInfo 10000 0.08 5 Monthly (T.fromGregorian 2022 1 1))
     10000 0.08 5

tmcf = calcCashflow tm 
tmcf2 = projCashflow tm [DefaultConstant 0.015]

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

-- applyAssumption :: Asset a => a -> [Assumption] -> [Ts]
-- applyAssumption a assumps = []

runPool :: Asset a => [a] -> (Maybe [AssumptionBuilder])-> [CF.CashFlowFrame]
runPool as Nothing = map calcCashflow as
runPool assets (Just assumps) 
  = map (\x -> (projCashflow x assumps)) assets

aggPool :: [CF.CashFlowFrame]  -> CF.CashFlowFrame
aggPool asflows = foldr1 CF.combine asflows


$(deriveJSON defaultOptions ''Mortgage)
$(deriveJSON defaultOptions ''OriginalInfo)
$(deriveJSON defaultOptions ''Pool)
$(deriveJSON defaultOptions ''AssumptionBuilder)
