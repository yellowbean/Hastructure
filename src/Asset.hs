{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Asset (Mortgage(..),Pool(..),OriginalInfo(..),calc_p_i_flow
       ,aggPool,runPool,calcCashflow
) where

import Control.Lens
import Data.Time (Day)
import qualified Data.Time as T
import Lib (Period(..),Rate, Balance,calcInt,Dates,DayCount(..),calcIntRate,genDates)
import qualified Cashflow as CF -- (Cashflow,Amount,Interests,Principals)

import qualified Data.Map as Map
import Data.List
import Data.Aeson hiding (json)
import Language.Haskell.TH
import Data.Aeson.TH
import Data.Aeson.Types

class Asset a where
  calcCashflow :: a -> CF.CashFlowFrame
  getCurrentBal :: a -> Float
  -- projCashflow :: a ->  -> CF.CashFLow

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

data Mortgage = Mortgage OriginalInfo Float Float Int
                deriving (Show)

instance Asset Mortgage  where
  calcCashflow (Mortgage x _bal _rate _term) =
      CF.CashFlowFrame $ zipWith6
                            CF.MortgageFlow
                              cf_dates
                              b_flow
                              prin_flow
                              int_flow
                              (replicate l 0.0)
                              (replicate l 0.0)
    where
      d = (startDate x)
      pmt = calcPmt (originBalance x) (originRate x) (originTerm x)
      cf_dates = (genDates d (period x) (originTerm x))
      l = length cf_dates
      (b_flow,prin_flow,int_flow) = calc_p_i_flow _bal pmt cf_dates _rate

  getCurrentBal (Mortgage x _bal _ _) = _bal

_calc_p_i_flow :: Float -> [Balance] -> [Float] -> [Float] -> [Rate] -> ([Balance],CF.Principals,CF.Interests)
_calc_p_i_flow pmt bals ps is rs =
    if (last bals) < 0.01 || (length rs==0) then
      (bals,ps,is)
    else
      _calc_p_i_flow pmt (bals++[new_bal]) (ps++[new_prin]) (is++[new_int]) (tail rs)
      where
        new_int = (last bals) * (head rs)
        new_prin = pmt - new_int
        new_bal = (last bals) - new_prin

calc_p_i_flow :: Balance -> Float -> Dates -> Rate -> ([Balance],CF.Principals,CF.Interests)
calc_p_i_flow bal pmt dates r =
    _calc_p_i_flow pmt [bal] [] [] period_r
    where size = length dates
          period_r = [ calcIntRate (dates!!d) (dates!!(d+1)) r ACT_360 | d <- [0..size-2]]

runPool :: Asset a => [a] -> [CF.CashFlowFrame]
runPool as = map calcCashflow as

aggPool :: [CF.CashFlowFrame]  -> CF.CashFlowFrame
aggPool asflows =
  foldr CF.combine first_flow tail_flows
  where
    first_flow = head asflows
    tail_flows = tail asflows


$(deriveJSON defaultOptions ''Mortgage)
$(deriveJSON defaultOptions ''OriginalInfo)
$(deriveJSON defaultOptions ''Pool)
