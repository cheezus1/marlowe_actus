module Language.Marlowe.ACTUS.Definitions where

import Data.Maybe
import Data.Time

data EOMC = EOMC_EOM
          | EOMC_SD deriving (Show, Eq)

data BDC =  BDC_NULL
          | BDC_SCF
          | BDC_SCMF
          | BDC_CSF
          | BDC_CSMF
          | BDC_SCP
          | BDC_SCMP
          | BDC_CSP
          | BDC_CSMP deriving (Show, Eq)

data DCC =  DCC_A_AISDA
          | DCC_A_360
          | DCC_A_365
          | DCC_E30_360ISDA
          | DCC_E30_360
          | DCC_B_252 deriving (Show)

data CalendarType = NoCalendar | MondayToFriday deriving (Show)

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

data ContractRole = CR_RPA -- Real position asset
                  | CR_RPL -- Real position liability
                  | CR_CLO -- Role of a collateral
                  | CR_CNO -- Role of a close-out-netting
                  | CR_COL -- Role of an underlying to a collateral
                  | CR_LG  -- Long position
                  | CR_ST  -- Short position
                  | CR_BUY -- Protection buyer
                  | CR_SEL -- Protection seller
                  | CR_RFL -- Receive first leg
                  | CR_PFL -- Pay first leg
                  | CR_RF  -- Receive fix leg
                  | CR_PF  -- Pay fix leg
                  deriving (Show, Eq)

-- CS – Indicates different states of the contract from performance to default
data ContractStatus = CS_PF -- performant
                    | CS_DL -- delayed
                    | CS_DQ -- delinquent
                    | CS_DF -- default
                    deriving (Show, Eq)

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

data Period = P_D -- Day
            | P_W -- Week
            | P_M -- Month
            | P_Q -- Quarter
            | P_H -- Half Year
            | P_Y -- Year
            deriving (Show, Eq, Ord)

data Stub = ShortStub | LongStub deriving (Show, Eq, Ord)

data Cycle = Cycle
  { n :: Integer
  , p :: Period
  , stub :: Stub
  } deriving (Show, Eq, Ord)

data Schedule = Schedule
  { s :: Maybe Day -- kANX with kANX attribute cycle anchor date of event type k
  , c :: Maybe Cycle -- kCL with kCL event type k’s schedule cycle
  , t :: Maybe Day -- MD with MD the contract’s maturity
  , b :: Bool -- indicating whether the schedule end date T belongs to the schedule or not
  , dateToExclude :: Maybe Day -- additional field
  } deriving (Show)

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
  , endOfMonthConvention :: EOMC -- EOMC

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
