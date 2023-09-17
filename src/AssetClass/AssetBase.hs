{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module AssetClass.AssetBase 
  (Installment(..),Lease(..),OriginalInfo(..),Status(..)
  ,LeaseStepUp(..),AccrualPeriod(..),PrepayPenaltyType(..)
  ,AmortPlan(..),Loan(..),Mortgage(..),AssetUnion(..)
  )
  where

import Language.Haskell.TH
import GHC.Generics
import Data.Aeson.TH
import Data.Aeson.Types
import Types hiding (Current,startDate,originTerm)


import qualified InterestRate as IR
import qualified Cashflow as CF

type DailyRate = Balance

data AmortPlan = Level                -- ^ for mortgage / french system  -> fixed payment each period which consist of increasing princial and decreasing interest.
               | Even                 -- ^ for linear mortgage   -> evenly distributed principal repayment
               | I_P                  -- ^ interest only and principal due at last payment
               | F_P                  -- ^ fee based 
               | ScheduleRepayment Ts -- ^ custom principal follow
               deriving (Show,Generic)

data Status = Current
            | Defaulted (Maybe Date)
            -- | Delinquency (Maybe Int)
            -- | Extended (Maybe T.Day)
            deriving (Show,Generic)

data PrepayPenaltyType = ByTerm Int Rate Rate           -- ^ using penalty rate 1 if period < Int, use penalty rate 2 if period > Int
                       | FixAmount Balance (Maybe Int)  -- ^ fixed penalty fee if any prepayment, or it only applies if period < Int
                       | FixPct Rate (Maybe Int)        -- ^ fixed percentage penalty fee as percentage of prepayment, or it only applies if period < Int
                       | Sliding Rate Rate              -- ^ starting with Rate1 at period 1 then decrease by step by rate2
                       | StepDown [(Int,Rate)]          -- ^ first tuple (n,r) ,first n periods use penalty rate r , then next n periods use pentaly rate in next tuple
                       -- | NMonthInterest Int
                       deriving (Show,Generic)

data OriginalInfo = MortgageOriginalInfo { originBalance :: Balance
                                          ,originRate :: IR.RateType
                                          ,originTerm :: Int
                                          ,period :: Period
                                          ,startDate :: Date
                                          ,prinType :: AmortPlan 
                                          ,prepaymentPenalty :: Maybe PrepayPenaltyType }
                  | LoanOriginalInfo { originBalance :: Balance
                                      ,originRate :: IR.RateType
                                      ,originTerm :: Int
                                      ,period :: Period
                                      ,startDate :: Date
                                      ,prinType :: AmortPlan }
                  | LeaseInfo { startDate :: Date            -- ^ lease start date
                              ,originTerm :: Int             -- ^ total terms
                              ,paymentDates :: DatePattern   -- ^ payment dates pattern
                              ,originRental :: Amount}       -- ^ rental by day
                  deriving (Show,Generic)


data Installment = Installment OriginalInfo Balance RemainTerms Status
                 | Dummy
                 deriving (Show,Generic)

data LeaseStepUp = FlatRate DatePattern Rate
                 | ByRateCurve DatePattern [Rate]
                 deriving (Show,Generic)

data Lease = RegularLease OriginalInfo Balance RemainTerms Status
           | StepUpLease OriginalInfo LeaseStepUp Balance RemainTerms Status
           deriving (Show,Generic)

data AccrualPeriod = AccrualPeriod Date DailyRate
                    deriving (Show,Generic)

instance TimeSeries AccrualPeriod where 
    getDate (AccrualPeriod d _) = d

data Loan = PersonalLoan OriginalInfo Balance IRate RemainTerms Status
          | DUMMY
          deriving (Show,Generic)

-- Mortgage
data MortgageInsurance = MortgageInsurance Rate

data Mortgage = Mortgage OriginalInfo Balance IRate RemainTerms (Maybe BorrowerNum) Status
              | AdjustRateMortgage OriginalInfo IR.ARM Balance IRate RemainTerms (Maybe BorrowerNum) Status
              | ScheduleMortgageFlow Date [CF.TsRow] DatePattern
              deriving (Show,Generic)

-- Base 
data AssetUnion = MO Mortgage
                | LO Loan
                | IL Installment
                | LS Lease
                deriving (Show, Generic)


$(deriveJSON defaultOptions ''Status)
$(deriveJSON defaultOptions ''AmortPlan)
$(deriveJSON defaultOptions ''OriginalInfo)
$(deriveJSON defaultOptions ''Installment)
$(deriveJSON defaultOptions ''LeaseStepUp)
$(deriveJSON defaultOptions ''Mortgage)
$(deriveJSON defaultOptions ''Loan)
$(deriveJSON defaultOptions ''Lease)
$(deriveJSON defaultOptions ''AssetUnion)
$(deriveJSON defaultOptions ''PrepayPenaltyType)
