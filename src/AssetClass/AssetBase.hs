{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}

module AssetClass.AssetBase 
  (Installment(..),Lease(..),OriginalInfo(..),Status(..)
  ,LeaseStepUp(..),AccrualPeriod(..),PrepayPenaltyType(..)
  ,AmortPlan(..),Loan(..),Mortgage(..),AssetUnion(..),MixedAsset(..),FixedAsset(..)
  ,AmortRule(..),Capacity(..),AssociateExp(..),AssociateIncome(..),ReceivableFeeType(..),Receivable(..)
  ,ProjectedCashflow(..)
  ,calcAssetPrinInt, calcPmt
  )
  where

import Language.Haskell.TH
import GHC.Generics
import Data.Aeson.TH
import Data.Aeson.Types

import Data.OpenApi hiding (Server,contentType)

import Types hiding (Current,startDate,originTerm)
import Data.Ratio
import Data.Proxy
import Util
import qualified Data.Map as Map
import qualified InterestRate as IR
import qualified Cashflow as CF
-- import Assumptions (RevolvingAssumption(Dummy4))

type DailyRate = Balance

data AmortPlan = Level                    -- ^ for mortgage / french system  -> fixed payment each period which consist of increasing princial and decreasing interest.
                | Even                    -- ^ for linear mortgage   -> evenly distributed principal repayment
                | I_P                     -- ^ interest only and principal due at last payment
                | F_P                     -- ^ fee based 
                | PO_FirstN Int       -- ^ 0 fee for first N period
                | IO_FirstN Int AmortPlan -- ^ interest only for first N period
                | NO_FirstN Int AmortPlan -- ^ non payment during first N period
                | ScheduleRepayment Ts (Maybe DatePattern)   -- ^ custom principal follow
                deriving (Show, Generic, Ord, Eq)

-- | calculate period payment (Annuity/Level mortgage)
calcPmt :: Balance -> IRate -> Int -> Amount
calcPmt bal 0.0 periods = divideBI bal periods
calcPmt bal periodRate periods =
  let
    periodRate1 = toRational periodRate
    r1 =  ((1+periodRate1)^^periods) / ((1+periodRate1)^^periods-1) -- `debug` ("PR>>"++show periodRate)
    pmtFactor = periodRate1 * r1 -- `debug` ("R1>>"++ show r1)
  in
    mulBR bal pmtFactor -- `debug` ("Factor"++ show pmtFactor)


type InterestAmount = Amount
type PrincipalAmount = Amount

calcAssetPrinInt :: AmortPlan -> Balance -> IRate -> Int -> Int -> (InterestAmount, PrincipalAmount)
calcAssetPrinInt pt bal rate ot rt = 
  let 
    interestAccrued = mulBIR bal rate
    pmt = calcPmt bal rate rt
    periodPassed = ot - rt
  in 
    case pt of 
      Level -> (interestAccrued, pmt - interestAccrued)
      Even -> (interestAccrued, bal / fromIntegral rt)
      I_P -> if rt == 1 then
               (interestAccrued, bal)
             else
               (interestAccrued, 0)
      NO_FirstN n _pt -> if periodPassed >= n then 
                          calcAssetPrinInt _pt bal rate ot rt
                         else
                          (0, negate interestAccrued)
      IO_FirstN n _pt -> if periodPassed >= n then 
                          calcAssetPrinInt _pt bal rate ot rt
                         else
                          (interestAccrued, 0)
                         
      _ -> error $ "unsupported pt "++ show pt


data Status = Current
            | Defaulted (Maybe Date)
            -- | Delinquency (Maybe Int)
            -- | Extended (Maybe T.Day)
            deriving (Show,Generic,Ord,Eq)

data PrepayPenaltyType = ByTerm Int Rate Rate           -- ^ using penalty rate 1 if period < Int, use penalty rate 2 if period > Int
                       | FixAmount Balance (Maybe Int)  -- ^ fixed penalty fee if any prepayment, or it only applies if period < Int
                       | FixPct Rate (Maybe Int)        -- ^ fixed percentage penalty fee as percentage of prepayment, or it only applies if period < Int
                       | Sliding Rate Rate              -- ^ starting with Rate1 at period 1 then decrease by step by rate2
                       | StepDown [(Int,Rate)]          -- ^ first tuple (n,r) ,first n periods use penalty rate r , then next n periods use pentaly rate in next tuple
                       -- | NMonthInterest Int
                       deriving (Show,Generic,Eq,Ord)

data AmortRule = DecliningBalance        -- ^ DecliningBalance Method
               | DoubleDecliningBalance  -- ^ Not implemented
               | StraightLine            -- ^ Straight Line Method
               -- | UnitBased Int
               -- | MACRS
               | SumYearsDigit           -- ^ Not implemented
               deriving (Show,Generic,Eq,Ord)

data ReceivableFeeType = FixedFee Balance                    -- ^ a flat fee amount
                       | FixedRateFee Rate                   -- ^ a percentage fee against balance for once
                       | FactorFee Rate Int Direction        -- ^ a percentage fee against balance for each period (N days)
                       | AdvanceFee Rate                     -- ^ annualized rate for discount fee based on advance amount
                       | CompoundFee [ReceivableFeeType]     -- ^ compound fee
                       deriving (Show,Generic,Eq,Ord)


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
                  | FixedAssetInfo { startDate :: Date 
                                     ,originBalance :: Balance 
                                     ,residualBalance :: Balance
                                     ,originTerm :: Int
                                     ,period :: Period
                                     ,accRule :: AmortRule
                                     ,capacity :: Capacity }
                  | ReceivableInfo { startDate :: Date
                                   ,originBalance :: Balance
                                   ,originAdvance :: Balance
                                   ,dueDate :: Date
                                   ,feeType :: Maybe ReceivableFeeType }
                  deriving (Show,Generic,Ord,Eq)


data Installment = Installment OriginalInfo Balance RemainTerms Status
                 | Dummy
                 deriving (Show,Generic,Ord,Eq)

data LeaseStepUp = FlatRate DatePattern Rate
                 | ByRateCurve DatePattern [Rate]
                 deriving (Show,Generic,Ord,Eq)

data Lease = RegularLease OriginalInfo Balance RemainTerms Status
           | StepUpLease OriginalInfo LeaseStepUp Balance RemainTerms Status
           deriving (Show,Generic,Eq,Ord)

data AccrualPeriod = AccrualPeriod Date DailyRate
                    deriving (Show,Generic,Eq,Ord)

instance TimeSeries AccrualPeriod where 
    getDate (AccrualPeriod d _) = d

data Loan = PersonalLoan OriginalInfo Balance IRate RemainTerms Status
          | DUMMY
          deriving (Show,Generic,Ord,Eq)

data Mortgage = Mortgage OriginalInfo Balance IRate RemainTerms (Maybe BorrowerNum) Status
              | AdjustRateMortgage OriginalInfo IR.ARM Balance IRate RemainTerms (Maybe BorrowerNum) Status
              | ScheduleMortgageFlow Date [CF.TsRow] DatePattern
              deriving (Show,Generic,Eq,Ord)

data ProjectedCashflow = ProjectedFlowFixed Date [CF.TsRow] DatePattern
                       | ProjectedFlowMixFloater Date [CF.TsRow] DatePattern (Rate, IRate) [(Rate, Spread, Index)]
                       deriving (Show,Generic,Eq,Ord)

data Receivable = Invoice OriginalInfo Status
                | DUMMY4
                deriving (Show,Generic,Eq,Ord)

data MixedAsset = MixedPool (Map.Map String [AssetUnion])
                | DUMMY2
                deriving (Show,Generic,Eq,Ord)

type LineOfCredit = Maybe Balance

data Revolver = Heloc OriginalInfo LineOfCredit Balance IRate RemainTerms (Maybe BorrowerNum) Status
              | DUMMY5
              deriving (Show,Generic,Eq,Ord)



-- FixedAsset 
data Capacity = FixedCapacity Balance
              | CapacityByTerm [(Int,Balance)]
              deriving (Show,Generic,Ord,Eq)

data AssociateExp = ExpPerPeriod Balance 
                  | ExpPerUnit Balance
                  deriving (Show,Generic,Ord,Eq)

data AssociateIncome = IncomePerPeriod Balance 
                      | IncomePerUnit Balance
                      deriving (Show,Generic,Ord,Eq)

data FixedAsset = FixedAsset OriginalInfo RemainTerms
                | Dummy5
                deriving (Show,Generic,Eq,Ord)


-- Base type to hold all asset types
data AssetUnion = MO Mortgage
                | LO Loan
                | IL Installment
                | LS Lease
                | FA FixedAsset
                | RE Receivable
                deriving (Show, Generic,Ord,Eq)

instance IR.UseRate AssetUnion where
  getIndex (MO ma) = IR.getIndex ma
  getIndex (LO ma) = IR.getIndex ma
  getIndex (IL ma) = IR.getIndex ma
  getIndex (LS ma) = IR.getIndex ma
  getIndex (FA ma) = IR.getIndex ma
  getIndex (RE ma) = IR.getIndex ma


instance IR.UseRate Mortgage where 
  getIndex (Mortgage oi@MortgageOriginalInfo{ originRate = IR.Floater _ idx _ _ _ _ _ _ } _ _ _ _ _) = Just idx 
  getIndex Mortgage {} = Nothing
  getIndex (AdjustRateMortgage oi@MortgageOriginalInfo{ originRate = IR.Floater _ idx _ _ _ _ _ _ } _ _ _ _ _ _) = Just idx 
  getIndex AdjustRateMortgage {} = Nothing

instance IR.UseRate Loan where
  getIndex (PersonalLoan oi@LoanOriginalInfo{originRate = IR.Floater _ idx _ _ _ _ _ _ } _ _ _ _) = Just idx 
  getIndex PersonalLoan {} = Nothing

instance IR.UseRate Installment where 
  getIndex (Installment oi@LoanOriginalInfo{originRate = IR.Floater _ idx _ _ _ _ _ _ } _ _ _) = Just idx 
  getIndex Installment {} = Nothing
  
instance IR.UseRate Lease where
  getIndex :: Lease -> Maybe Index
  getIndex _ = Nothing

instance IR.UseRate FixedAsset where
  getIndex _ = Nothing

instance IR.UseRate Receivable where
  getIndex _ = Nothing



$(concat <$> traverse (deriveJSON defaultOptions) [''OriginalInfo, ''FixedAsset, ''AmortPlan, ''PrepayPenaltyType
    , ''Capacity, ''AmortRule, ''ReceivableFeeType])


-- $(deriveJSON defaultOptions ''AmortRule)
-- $(deriveJSON defaultOptions ''Capacity)
$(deriveJSON defaultOptions ''AssociateExp)
$(deriveJSON defaultOptions ''AssociateIncome)
-- $(deriveJSON defaultOptions ''FixedAsset)
$(deriveJSON defaultOptions ''Status)
-- $(deriveJSON defaultOptions ''AmortPlan)
-- $(deriveJSON defaultOptions ''ReceivableFeeType)
-- $(deriveJSON defaultOptions ''OriginalInfo)
$(deriveJSON defaultOptions ''Installment)
$(deriveJSON defaultOptions ''LeaseStepUp)
$(deriveJSON defaultOptions ''Mortgage)
$(deriveJSON defaultOptions ''Loan)
$(deriveJSON defaultOptions ''Lease)
$(deriveJSON defaultOptions ''Receivable)
$(deriveJSON defaultOptions ''AssetUnion)
-- $(deriveJSON defaultOptions ''PrepayPenaltyType)


-- instance ToSchema TsPoint
-- instance ToSchema (Ratio Integer)
instance ToSchema Capacity
instance ToSchema AmortRule
-- instance ToSchema (Ratio Integer) 
instance ToSchema (Ratio Integer) where 
  declareNamedSchema _ = NamedSchema Nothing <$> declareSchema (Proxy :: Proxy Double)


instance ToSchema PrepayPenaltyType
instance ToSchema Ts
instance ToSchema (TsPoint Balance)
instance ToSchema (TsPoint IRate)
instance ToSchema (TsPoint Rational)
instance ToSchema (TsPoint Bool)
instance ToSchema (RoundingBy IRate)

instance ToSchema Index
instance ToSchema DayCount
instance ToSchema Direction
instance ToSchema AmortPlan
instance ToSchema DatePattern
instance ToSchema IR.RateType
-- instance ToSchema (IR.RoundingBy IR.RateType)
instance ToSchema CF.TsRow
instance ToSchema Period
instance ToSchema IR.ARM
instance ToSchema Status
instance ToSchema ReceivableFeeType
instance ToSchema OriginalInfo
instance ToSchema Mortgage 
