{-# LANGUAGE DeriveGeneric #-}
module Interface(Liable(..),Accruable(..),Payable(..),RateResettable(..),Drawable(..),Collectable(..)
        ,DueType(..)
        )

where

import GHC.Generics
import Types

class Liable lb where 
    -- bond / expense / liquidity facility / hedges 

    -- must implement
    isPaidOff :: lb -> Bool
    getCurBalance :: lb -> Balance
    getCurRate :: lb -> IRate
    getOriginBalance :: lb -> Balance
    getOriginDate :: lb -> Date
    getAccrueBegDate :: lb -> Date
    getDueInt :: lb -> Balance
    getDueIntAt :: lb -> Int -> Balance
    getDueIntOverInt :: lb -> Balance
    getDueIntOverIntAt :: lb -> Int -> Balance
    getTotalDueInt :: lb -> Balance
    getTotalDueIntAt :: lb -> Int -> Balance
    getOutstandingAmount :: lb -> Balance



class Accruable ac where 
    -- 
    bookAccrual :: Date -> Balance -> ac -> ac
    getAccrualDates :: Date -> ac -> [Date]
    accrueTo :: Date -> ac -> ac
    -- accrueWithDeal :: Date -> deal -> ac -> ac

class Payable pa where
    pay :: Date -> DueType -> Balance -> pa -> Either ErrorRep pa
    getDueBal :: Date -> Maybe DueType -> pa -> Balance
    writeOff :: Date -> DueType -> Amount -> pa -> Either ErrorRep pa

class RateResettable rs where
    getResetDates :: Date -> rs -> [Dates]
    reset :: Date -> rs -> rs

class Drawable dr where
    draw :: Date -> Balance -> TxnComment -> dr -> Either ErrorRep dr
    availForDraw :: Date -> dr -> SupportAvailType

class Collectable cl where 
    collect :: Date -> cl -> Either ErrorRep cl
    availForCollect :: Date -> cl -> Either ErrorRep Balance

