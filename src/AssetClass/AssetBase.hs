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

data AmortPlan = Level   -- for mortgage / french system
               | Even    -- for mortgage
               | I_P     -- interest only and principal due at last payment
               | F_P     -- fee based 
               | ScheduleRepayment Ts-- custom principal follow
               deriving (Show,Generic)

data Status = Current
            | Defaulted (Maybe Date)
            -- | Delinquency (Maybe Int)
            -- | Extended (Maybe T.Day)
            deriving (Show,Generic)

data PrepayPenaltyType = ByTerm Int Float Float
                       | FixAmount Balance (Maybe Int)
                       | FixPct Float (Maybe Int)
                       | Sliding Float Float
                       | StepDown [(Int,Float)]
                       -- | NMonthInterest Int
                       deriving (Show,Generic)

data OriginalInfo = MortgageOriginalInfo { originBalance :: Balance
                                          ,originRate :: IR.RateType
                                          ,originTerm :: Int
                                          ,period :: Period
                                          ,startDate :: Date
                                          ,prinType :: AmortPlan 
                                          ,pepaymentPenalty :: Maybe PrepayPenaltyType }
                  | LoanOriginalInfo { originBalance :: Balance
                                      ,originRate :: IR.RateType
                                      ,originTerm :: Int
                                      ,period :: Period
                                      ,startDate :: Date
                                      ,prinType :: AmortPlan }
                  | LeaseInfo { startDate :: Date
                              ,originTerm :: Int 
                              ,paymentDates :: DatePattern
                              ,originRental :: Amount}
                  deriving (Show,Generic)


data Installment = Installment OriginalInfo Balance RemainTerms Status
                 | Dummy
                 deriving (Show,Generic)

data LeaseStepUp = FlatRate DatePattern Rate
                 | ByRateCurve DatePattern [Rate]
                 deriving (Show,Generic)

data Lease = RegularLease OriginalInfo Balance Int Status
           | StepUpLease OriginalInfo LeaseStepUp Balance Int Status
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
              | ScheduleMortgageFlow Date [CF.TsRow]
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
