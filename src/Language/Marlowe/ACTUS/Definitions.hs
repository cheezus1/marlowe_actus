module Language.Marlowe.ACTUS.Definitions where

import Data.Maybe
import Data.Time

import Language.Marlowe.ACTUS.Util.Schedule
import Language.Marlowe.ACTUS.Util.Cycle as Cycle
import Language.Marlowe.ACTUS.Util.Conventions.BusinessDayShift
import Language.Marlowe.ACTUS.Util.Conventions.ContractDefault
import Language.Marlowe.ACTUS.Util.Conventions.ContractRoleSign
import Language.Marlowe.ACTUS.Util.Conventions.YearFraction
import Language.Marlowe.ACTUS.Util.Conventions.DayCount
import Language.Marlowe.ACTUS.Util.Conventions.EndOfMonthShift

data Event =  IED
            | FP
            | PR
            | PD
            | PRF
            | PY
            | PP
            | IP
            | IPCI
            | CE
            | RRF
            | RR
            | DV
            | PRD
            | MR
            | TD
            | SC
            | IPCB
            | MD
            | XD
            | STD
            | AD deriving (Show, Eq, Ord)

data ScalingEffect =  SE_000
                    | SE_0N0
                    | SE_00M
                    | SE_0NM
                    | SE_I00
                    | SE_IN0
                    | SE_I0M
                    | SE_INM deriving (Show, Eq)

data InterestCalculationBase = ICB_NT | ICB_NTIED | ICB_NTL deriving (Show, Eq)

data FeeBasis = FB_A | FB_N deriving (Show, Eq)

data PenaltyType = PT_O | PT_A | PT_N | PT_I deriving (Show, Eq)

data PrepaymentEffect = PE_N | PE_A | PE_M deriving (Show, Eq)

data BoundTypes = INF | SUP

data ContractState = ContractState
  { t0  :: Day
  , tmd :: Day
  , nt  :: Double
  , ipnr :: Double
  , ipac :: Double
  , fac :: Double
  , nsc :: Double
  , isc :: Double
  , prf :: ContractStatus
  , sd :: Day
  , prnxt :: Double
  , ipcb :: Double
  } deriving (Show)

data ContractConfig = ContractConfig
  {
  -- General
    nominalInterestRate :: Double -- IPNR
  , dayCountConvention :: DCC -- IPDC
  , initialExchangeDate :: Day -- IED
  , notionalPrincipal :: Double -- NT

  -- Calendar
  , calendar :: CalendarType -- CLDR
  , businessDayConvention :: BDC -- BDC
  , endOfMonthConvention :: EOM -- EOMC

  -- Contract Identification
  , statusDate :: Maybe Day -- SD (t0)
  , contractRole :: Maybe ContractRole -- CNTRL

  -- Fees
  , cycleAnchorDateOfFee :: Maybe Day -- FEANX
  , cycleOfFee :: Maybe [Char] -- FECL
  , feeBasis :: Maybe FeeBasis -- FEB
  , feeRate :: Maybe Double -- FER
  , feeAccrued :: Maybe Double -- FEAC

  -- Interest
  , cycleAnchorDateOfInterestPayment :: Maybe Day -- IPANX
  , cycleOfInterestPayment :: Maybe [Char] -- IPCL
  , accruedInterest :: Maybe Double -- IPAC
  , capitalizationEndDate :: Maybe Day -- IPCED
  , cycleAnchorDateOfInterestCalculationBase :: Maybe Day -- IPCBANX
  , cycleOfInterestCalculationBase :: Maybe [Char] -- IPCBCL
  , interestCalculationBase :: InterestCalculationBase -- IPCB
  , interestCalculationBaseAmount :: Maybe Double -- IPCBA

  -- Notional Principal
  , maturityDate :: Maybe Day -- MD
  , contractDealDate :: Maybe Day -- CDD
  , premiumDiscountAtIED :: Double -- PDIED
  , cycleAnchorDateOfPrincipalRedemption :: Maybe Day -- PRANX
  , cycleOfPrincipalRedemption :: Maybe [Char] -- PRCL
  , nextPrincipalRedemptionPayment :: Maybe Double -- PRNXT
  , purchaseDate :: Maybe Day -- PRD
  , priceAtPurchaseDate :: Maybe Double -- PPRD
  , terminationDate :: Maybe Day -- TD
  , priceAtTerminationDate :: Maybe Double -- PTD
  , marketObjectCodeOfScalingIndex :: Maybe String -- RRMO
  , scalingIndexAtStatusDate :: Maybe Double -- SCIXSD
  , cycleAnchorDateOfScalingIndex :: Maybe Day -- SCANX
  , cycleOfScalingIndex :: Maybe [Char] -- SCCL
  , scalingEffect :: ScalingEffect -- SCEF

  -- Optionality
  , cycleAnchorDateOfOptionality :: Maybe Day -- OPANX
  , cycleOfOptionality :: Maybe [Char] -- OPCL
  , penaltyType :: PenaltyType -- PYTP
  , penaltyRate :: Double -- PYRT
  , prepaymentEffect :: PrepaymentEffect -- PPEF

  -- Rate Reset
  , cycleAnchorDateOfRateReset :: Maybe Day -- RRANX
  , cycleOfRateReset :: Maybe [Char] -- RRCL
  , rateSpread :: Double -- RRSP
  , periodFloor :: Maybe Double -- RRPF
  , periodCap :: Maybe Double -- RRPC
  , lifeFloor :: Maybe Double -- RRLF
  , lifeCap :: Maybe Double -- RRLC
  , nextResetRate :: Maybe Double -- RRNXT
  , rateMultiplier :: Double -- RRMLT
  } deriving (Show)
