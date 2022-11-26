{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module AssetClass.Installment 
  (Installment(..))
  where

import qualified Data.Time as T
import Data.Ratio

import Data.Aeson hiding (json)
import Language.Haskell.TH
import Data.Maybe
import Data.Aeson.TH
import Data.Aeson.Types

import Asset
import Types 
import Lib
import Util
import qualified Cashflow as CF

import Debug.Trace
debug = flip trace

data Installment = Installment OriginalInfo Balance RemainTerms Status
                 | Dummy
     deriving (Show)

calc_p_i_flow_f_p :: Balance -> Balance -> Balance -> Amount -> Dates -> Period -> IRate -> ([Balance],CF.Principals,CF.Interests)
calc_p_i_flow_f_p ob cb sb amt ds p r 
  = (_bals, _prins,_fees)
    where 
      size = length ds
      factor = toRational $ cb / sb
      _prins = replicate size $ (mulBR amt factor)
      _period_fee =  mulBR (mulBI ob r) factor
      _bals = tail $ scanl (-) cb _prins
      _fees = replicate size _period_fee


instance Asset Installment where
  calcCashflow inst@(Installment (LoanOriginalInfo ob or ot p sd ptype) cb rt st) asOfDay
    = CF.CashFlowFrame $ zipWith9
                          CF.LoanFlow
                            (tail cf_dates)
                            b_flow
                            prin_flow
                            int_flow
                            (replicate l 0.0)
                            (replicate l 0.0)
                            (replicate l 0.0)
                            (replicate l 0.0)
                            (replicate l orate)
      where 
        orate = getOriginRate inst
        cf_dates = take (succ ot) $ sliceDates (SliceAfterKeepPrevious asOfDay) $ getPaymentDates inst 0
        l = pred (length cf_dates)
        pmt = divideBI ob ot
        schedule_bal = fromRational $ (toRational ob) * (toRational (rt % ot))
        (b_flow,prin_flow,int_flow) = case ptype of 
                                        F_P -> calc_p_i_flow_f_p ob cb schedule_bal pmt cf_dates p orate

  getCurrentBal (Installment _ b _ _) = b
  
  getOriginBal (Installment (LoanOriginalInfo ob _ _ _ _ _) _ _ _) = ob

  getOriginRate (Installment (LoanOriginalInfo _ or _ _ _ _) _ _ _) 
    = case or of
       Fix _r -> _r
       Floater _ _ _r _ Nothing -> _r
       Floater _ _ _r _ (Just floor) -> max _r floor


  isDefaulted (Installment _ _ _ (Defaulted _)) = True
  isDefaulted (Installment _ _ _ _) = False

  getPaymentDates (Installment (LoanOriginalInfo _ _ ot p sd _) _ _ _) extra 
    = genDates sd p (ot+extra)

  projCashflow inst = undefined

$(deriveJSON defaultOptions ''Installment)
