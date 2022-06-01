{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Asset (Mortgage(..),Pool(..),OriginalInfo(..),calc_p_i_flow
       ,aggPool,calcCashflow,getCurrentBal,getOriginBal,runPool
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

data Assumption =  MortgageByAge ([Int],[Float])
                 | MortgageByRate ([Float],[Float])
                 | PrepaymentConstant Float
                 | DefaultConstant Float
                 | RecoveryConstant Float


class Asset a where
  calcCashflow :: a -> CF.CashFlowFrame
  getCurrentBal :: a -> Float
  getOriginBal :: a -> Float
  projCashflow :: a -> [Assumption] -> CF.CashFlowFrame

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
  calcCashflow (Mortgage (OriginalInfo ob or ot p sd )  _bal _rate _term) =
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
      cf_dates = genDates sd p ot
      l = length cf_dates
      (b_flow,prin_flow,int_flow) = calc_p_i_flow _bal pmt cf_dates _rate

  getCurrentBal (Mortgage x _bal _ _) = _bal
  getOriginBal (Mortgage (OriginalInfo _bal _ _ _ _  ) _ _ _) = _bal

  projCashflow m assumps = calcCashflow m

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

applyAssumption :: Asset a => a -> [Assumption] -> [Ts]
applyAssumption a assumps = []

runPool :: Asset a => [a] -> (Maybe [Assumption])-> [CF.CashFlowFrame]
runPool as Nothing = map calcCashflow as
runPool assets (Just [assumps])  = map calcCashflow assets

aggPool :: [CF.CashFlowFrame]  -> CF.CashFlowFrame
aggPool asflows = foldr1 CF.combine asflows


$(deriveJSON defaultOptions ''Mortgage)
$(deriveJSON defaultOptions ''OriginalInfo)
$(deriveJSON defaultOptions ''Pool)
