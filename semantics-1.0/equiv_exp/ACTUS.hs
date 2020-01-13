{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module ACTUS where

import Data.Maybe
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Clock.System
import Debug.Trace
import Minimal
{-
    = Data Dictionary

| SD    | StatusDate    |
| CNTRL | ContractRole
| LEICP | LegalEntityID Counterparty
| LEIRC | LegalEntityIDRecord Creator
| DS    | DeliverySettlement                |
| NT    | NotionalPrincipal                 |
| IPANX | CycleAnchorDateOfInterestPayment  |   Date from which the periodic occurring dates are calculated through the cycle length. The first IP event (first interest cash flow will be paid) takes place on this anchor. If IPANX is not set, then there will be exactly one interest cash flow at MaturityDate.
                                            |   Note: Interest accrual calculation starts always at IED (or PRD if set).
| IPCL  | CycleOfInterestPayment            |   For the explanation of the format see RRCL. The interval will be adjusted yet by EOMC and BDC.
                                                The lower boundary of accrual calculation is IED and the upper boundary MD where also a last interest cash flow happens. In case where the Cycle_Of_IP does not coincide with the Maturity_Date, the last interest cash flow will be time adjusted depending on the stub information.

PYTP PenaltyType Defines whether prepayment is linked to a penalty and of which kind.

    = State Variables

    = Contract Events

    A contract event e refers to any contractually scheduled or un-scheduled event at
a certain time t and of a certain type k.

    = State Transition Functions.

State Transition Functions (STF) define how the State Variables are being updated when a certain Contract Event e(k, t) applies from a pre-event (i.e. pre-time t) state indexed t− to a post-event (i.e. post-time t) state indexed t+.
The STF for an IP event and PAM contract is written as STF_IP_PAM() and maps e.g. state variable Nominal Accrued from a pre-event state Nact− to post-event state Nact+.

    = Contract Lifetime.

The lifetime of a contract starts with its SD and ends with
min(MD, AMD, PR∗, STD, TD, tmax).

    = Questions.

Unknowns: OPMO

-}

{-| Contract Events. Order of constructors matters for state transition and payout calculations. -}
data ContractEvent  = AD   -- Analysis Event Retrieves current contract states without alter these
                    | IED  -- Initial Exchange Date Scheduled date of first principal event, start of accrual calculation
                    | PR   -- Principal Redemption Scheduled principal redemption payment
                    | PI   -- Principal Increase Scheduled principal increase payments
                    | PRF  -- Principal Payment Amount Fixing Scheduled re-fixing of principal payment (PR or PI) amount
                    | PP   -- Principal Prepayment Unscheduled (early) repayment of principal outstanding
                    | PY   -- Penalty Payment Payment of a penalty (e.g. due to early repayment of principal outstanding)
                    | FP   -- Fee Payment Scheduled fee payment
                    | PRD  -- Purchase Date Purchase date of a contract bought in the secondary market
                    | TD   -- Termination Date Sell date of a contract sold in the secondary market
                    | IP   -- Interest Payment Scheduled interest payment
                    | IPCI -- Interest Capitalization Scheduled interest payment which is capitalized instead of paid out
                    | IPCB -- Interest Payment Calculation Base Scheduled update to the calculation base for IP accruing
                    | RR   -- Rate Reset Variable Scheduled rate reset event where new rate is fixed at event time
                    | RRF  -- Rate Reset Fixed Scheduled rate reset event where new rate is already fixed
                    | SC   -- Scaling Index Revision Scheduled re-fixing of a scaling index
                    | XD   -- Execution Date Scheduled or unscheduled execution of e.g. an OPTNS or FUTUR contract
                    | DV   -- Dividend Payment Scheduled (e.g. announced) dividend payment
                    | MR   -- Margin Call Date Scheduled margin call event
                    | STD  -- Settlement Date Date when payment for derivatives is settled
                    | MD   -- Maturity Date Scheduled maturity or expiry of a contract
                    | CD   -- Credit Default Credit event of counterparty to a contract
                    deriving (Eq, Ord, Show)


-- CNTRL
data ContractRole   = RPA -- Real position asset
                    | RPL -- Real position liability
                    | CLO -- Role of a collateral
                    | CNO -- Role of a close-out-netting
                    | COL -- Role of an underlying to a collateral
                    | LG  -- Long position
                    | ST  -- Short position
                    | BUY -- Protection buyer
                    | SEL -- Protection seller
                    | RFL -- Receive first leg
                    | PFL -- Pay first leg
                    | RF  -- Receive fix leg
                    | PF  -- Pay fix leg
                    deriving (Show, Eq)

-- R
contractRoleSign :: ContractRole -> Integer
contractRoleSign role = case role of
    RPA ->  1
    RPL -> -1
    CLO ->  1
    CNO ->  1
    COL ->  1
    LG  ->  1
    ST  -> -1
    BUY ->  1
    SEL -> -1
    RFL ->  1
    PFL -> -1
    RF  ->  1
    PF  -> -1

-- | CS – Indicates different states of the contract from performance to default
data ContractStatus = PERF -- PF performant
                    | DL -- delayed
                    | DQ -- delinquent
                    | DF -- default
                    deriving (Show, Eq)

{-| Indicates whether the cash flows of the underlying financial contract of
    a combined contract are effectively exchanged or only used as
    a calculation base for the settlement cash flow(s).
-}
data DeliverySettlement = Delivery | Settlement deriving (Show, Eq)

data FeeBasis = AbsoluteValue {- A -} | NotionalOfUnderlying {- N -} deriving (Show, Eq)

data TimePeriod = Day | Week | Month | Quarter | HalfYear | Year deriving (Show, Eq)
data Stub = LongLastStub | ShortLastStub deriving (Show, Eq)
data Cycle = Cycle Integer TimePeriod Stub deriving (Show, Eq)
data EndOfMonthConvention = EndOfMonth | SameDay deriving (Show, Eq)
data BusinessDayConvention  = NoShift                           -- NULL
                            | ShiftCalculateFollowing           -- SCF
                            | ShiftCalculateModifiedFollowing   -- SCMF
                            | CalculateShiftFollowing           -- CSF
                            | CalculateShiftModifiedFollowing   -- CSMF
                            | ShiftCalculatePreceding           -- SCP
                            | ShiftCalculateModifiedFreceding   -- SCMP
                            | CalculateShiftPreceding           -- CSP
                            | CalculateShiftModifiedPreceding   -- CSMP
                            deriving (Show, Eq)


-- A schedule is a function S mapping times s, T with s < T and cycle c onto a sequence ~t of cyclic times
data Schedule = Schedule
    { scheduleStart :: Maybe Day
    , scheduleCycle :: Maybe Cycle
    , scheduleEnd   :: Maybe Day
    , scheduleEOMC  :: Maybe EndOfMonthConvention
    , scheduleBDC   :: Maybe BusinessDayConvention
    } deriving (Show, Eq)

emptySchedule :: Schedule
emptySchedule = Schedule
    { scheduleStart = Nothing
    , scheduleCycle = Nothing
    , scheduleEnd   = Nothing
    , scheduleEOMC  = Nothing
    , scheduleBDC   = Nothing
    }

singleEvent :: Day -> Schedule
singleEvent t = emptySchedule { scheduleStart = Just t }

addTimePeriod :: Day -> TimePeriod -> Day
addTimePeriod d period = case period of
    Day      -> addDays 1 d
    Week     -> addDays 7 d
    Month    -> addGregorianMonthsClip 1 d -- todo EndOfMonth convention
    Quarter  -> addGregorianMonthsClip 3 d -- todo EndOfMonth convention
    HalfYear -> addGregorianMonthsClip 6 d -- todo EndOfMonth convention
    Year     -> addGregorianYearsClip  1 d -- todo EndOfMonth convention


schedule :: Schedule -> [Day]
schedule s = case s of
    Schedule { scheduleStart=Nothing, scheduleEnd=Nothing }   -> []
    Schedule { scheduleStart=Just t,  scheduleEnd=Nothing }   -> [t]
    Schedule { scheduleStart=Nothing, scheduleEnd=Just tmax } -> error "Not specified scheduleStart"
    Schedule { scheduleStart=Just t,  scheduleCycle=Nothing, scheduleEnd=Just tmax } -> [t, tmax]
    Schedule { scheduleStart=Just t,  scheduleCycle=Just c,  scheduleEnd=Just tmax } ->
        if t < tmax then let
            (Cycle n timePeriod stub) = c
            times = List.unfoldr (\(d, i) ->
                let r = addTimePeriod d timePeriod
                in if i <= n && r <= tmax then Just (r, (r, i + 1)) else Nothing) (t, 1)
            -- times = scanl (\d i -> timePeriodToAsTimeout d timePeriod) t [1..n]
            -- todo stubs
            in times
        else error "scheduleStart must be less than scheduleEnd!"

type Timeout = Integer
type Money = Integer

data PenaltyType    = NoPenalty {- O -}
                    | Absolute  {- A -}
                    | NominalRate   {- N -}
                    | CurrentInterestRateDifferential   {- I -}

{-| Contract Default Convention is a function D that maps the Prf state variable into
    +1 indicating that the contract is performing or 0 which reflext default and,
    from an analytical perspective, means that future cash flows cancel out
-}
contractDefaultConvention :: ContractStatus -> Integer
contractDefaultConvention PERF  = 1
contractDefaultConvention _     = 0

{-| Year Fraction Convention, aka Day count convention.
    Currently only Actual/365 Fixed.

    -- TODO: implement other conventions.
-}
yearFractionConvention :: Day -> Day -> Double
yearFractionConvention s e = fromIntegral (abs $ diffDays e s) / 365

-- type LastEventDate = ContractEvent -> Day
type LastEventDate = Day

-- PAM: State Variables Initialization
data State = State
    { tmd :: Day        -- Maturity Date ?
    , nvl :: Money          -- Nominal Value. The outstanding nominal value
    , nv2 :: Money          -- Secondary Nominal Value. The outstanding nominal value of the second leg
    , nrt :: Double         -- Nominal Rate. The applicable nominal rate
    , nac :: Money          -- Nominal Accrued. The current value of nominal accrued interest at the Nominal Rate
    , fac :: Money          -- Fee Accrued?
    , icb :: Money          -- Interest Calculation Base. The basis at which interest is being accrued if different from Nvl
    , nsc :: Double         -- Notional Scaling Multiplier. The multiplier being applied to Notional/Principal related cashflows
    , isc :: Double         -- Interest Scaling Multiplier. The multiplier being applied to Interest related cash-flows
    , prf :: ContractStatus -- Contract performance
    , led :: LastEventDate        -- Last Event Date. The date of the most recent ContractEvent
    } deriving (Show)

pamStateInit :: Day -> Day -> State
pamStateInit t0 maturityDate = State
    { tmd = maturityDate
    , nvl = 0
    , nv2 = 0
    , nrt = 0
    , nac = 0
    , fac = 0
    , icb = 0
    , nsc = 1.0
    , isc = 1.0
    , prf = PERF
    , led = t0
    }

type FeeRate = Double

data ContractConfig = ContractConfig
             { initialExchangeDate :: Day
             , maturityDate :: Maybe Day
             , notional :: Money
             , nominalInterestRate    :: Double
             , interestPaymentCycle   :: Maybe Cycle
             , cycleAnchorDateOfInterestPayment :: Maybe Day -- IPANX
             , capitalizationEndDate  :: Maybe Day -- IPCED
             , premiumDiscountAtIED   :: Money
             , priceAtPurchaseDate    :: Money
             , priceAtTerminationDate :: Money
             , feeBasis :: FeeBasis
             , feeRate  :: FeeRate
             }

emptyContractConfig d = ContractConfig
    { initialExchangeDate = d
    , maturityDate = Nothing
    , notional = 0
    , nominalInterestRate = 0.0
    , interestPaymentCycle = Nothing
    , cycleAnchorDateOfInterestPayment = Nothing
    , capitalizationEndDate = Nothing
    , premiumDiscountAtIED = 0
    , priceAtPurchaseDate  = 0
    , priceAtTerminationDate = 0
    , feeBasis = AbsoluteValue
    , feeRate  = 0.0
    }

contractSchedule :: ContractConfig -> State -> Map ContractEvent Schedule
contractSchedule ContractConfig{..} State{..} =
    Map.fromList
        [ (IED, singleEvent initialExchangeDate)
        , (PR,  singleEvent tmd)
        , (PP,  emptySchedule)
        , (PY,  emptySchedule)
        , (FP,  emptySchedule)
        , (PRD, singleEvent initialExchangeDate)
        -- , (TD,  singleEvent tmd) -- todo
        , (IP,  interestPaymentSchedule)
        , (MD,  singleEvent tmd)
        ]
  where
    interestPaymentSchedule =
        if nominalInterestRate == 0.0 then emptySchedule
        else let
            start = case (cycleAnchorDateOfInterestPayment, interestPaymentCycle, capitalizationEndDate) of
                    (Nothing, Nothing, Nothing) -> Nothing
                    (_, _, Just ipced) -> Just ipced
                    (Nothing, Just (Cycle _ timePeriod _), Nothing) -> Just $ addTimePeriod initialExchangeDate timePeriod
                    (Just ipanx, _, Nothing) -> Just ipanx
            in maybe emptySchedule (\start -> emptySchedule { scheduleStart = Just start
                , scheduleCycle = interestPaymentCycle
                , scheduleEnd = Just tmd
                }) start

asdf :: Map ContractEvent Schedule -> Map Day [ContractEvent]
asdf m = do
    let eventDays = fmap schedule m
    let pairs = [(v, [k]) | (k, vs) <- Map.toList eventDays, v <- vs]
    Map.fromListWith (++) (reverse pairs)
  where


pamPayoff :: ContractRole
    -> ContractConfig
    -> Day
    -> ContractEvent
    -> State
    -> Money
pamPayoff role ContractConfig{..} currTime event State{..} =
  case event of
    IED     ->  -- D(Prft− )R(CNTRL)(−1)(NT + PDIED)
                dperf * rsign * (-1) * (notional + premiumDiscountAtIED)
    IPCI    -> 0
    IP      -> dperf * round (isc * fromIntegral (nac + yearNrtNvl))
                -- D(Prft− )Isct− (Nact− + Y (Ledt− , t)Nrtt− Nvlt− )
    FP      ->  let c = fromIntegral (dperf * rsign) * feeRate
                in case feeBasis of
                    AbsoluteValue -> round c
                    NotionalOfUnderlying -> round (c * yearNvl) + fac

    PR      ->  -- D(Prft− )Nsct− Nvlt− Prf
                round (fromIntegral (dperf * nvl) * nsc)
    PI      -> undefined
    PRF     -> undefined
    PY      -> 0 -- todo
    PP      -> 0 -- D(Prft− )Orf (OPMO, t) -- todo
    CD      -> 0
    RRF     -> 0
    RR      -> 0
    DV      -> undefined
    PRD     -> dperf * rsign * (-1) * (priceAtPurchaseDate + nac + yearNrtNvl)
                -- D(Prft− )R(CNTRL)(−1)(PPRD + Nact− +Y (Ledt− , t)Nrtt− Nvlt− )
    MR      -> undefined
    TD      -> dperf * rsign * (priceAtTerminationDate + nac + yearNrtNvl)
                -- D(Prft− )R(CNTRL)(PTD + Nact− + Y (Ledt− , t)Nrtt− Nvlt− )
    SC      -> 0
    IPCB    -> undefined
    XD      -> undefined
    STD     -> undefined
    MD      -> undefined
    AD      -> 0
  where dperf = contractDefaultConvention prf
        rsign = contractRoleSign role
        yearFrac a b = yearFractionConvention a b
        yearNvl = yearFrac led currTime * fromIntegral nvl
        yearNrtNvl = round (yearNvl * nrt)



pamStateTransition :: ContractRole
    -> ContractConfig
    -> ContractEvent
    -> Day
    -> State
    -> State
pamStateTransition role ContractConfig{..} event currTime state@State{..} = case event of
    IED     -> state { nvl = rsign * notional
                     , nrt = 0 -- todo
                     , nac = 0 -- todo
                     , led = currTime
                     }
    IPCI    -> state { nvl = nvl + nac + yearNrtNvl
                     , nac = 0
                     , fac = newFac
                     , led = currTime
                     }
    IP      -> state { nac = 0
                     , fac = newFac
                     , led = currTime
                     }
    FP      -> state { nac = nac + yearNrtNvl
                     , fac = 0
                     , led = currTime
                     }
    PR      -> state { nvl = 0
                     , nrt = 0
                     , led = currTime
                     }
    PI      -> undefined
    PRF     -> undefined
    PY      -> state { nac = nac + yearNrtNvl
                     , fac = newFac
                     , led = currTime
                     }
    PP      -> state { nac = nac + yearNrtNvl
                     , fac = newFac
                     , led = currTime
                     }
    CD      -> state { nac = nac + yearNrtNvl
                     , fac = newFac
                     , led = currTime
                     }
    RRF     -> state { nac = nac + yearNrtNvl
                     , fac = newFac
                     , prf = DF
                     , led = currTime
                     }
    RR      -> state { nac = nac + yearNrtNvl
                     , fac = newFac
                     , nrt = nrt -- todo
                     , led = currTime
                     }
    DV      -> undefined
    PRD     -> state { nac = nac + yearNrtNvl
                     , fac = newFac
                     , led = currTime
                     }
    MR      -> undefined
    TD      -> state { nvl = 0
                     , nac = 0
                     , fac = 0
                     , nrt = 0
                     , led = currTime
                     }
    SC      -> state { nac = nac + yearNrtNvl
                     , fac = newFac
                     , nsc = nsc -- todo
                     , isc = isc -- todo
                     , led = currTime
                     }
    IPCB    -> undefined
    XD      -> undefined
    STD     -> undefined
    MD      -> undefined
    AD      -> state { nac = nac + yearNrtNvl
                     , led = currTime
                     }
                -- Nact+ = Nact− + Y (Ledt−1, t)Nrtt− Nvlt−; Ledt+ = t
  where dperf = contractDefaultConvention prf
        rsign = contractRoleSign role
        yearFrac a b = yearFractionConvention a b
        yearNvl = yearFrac led currTime * fromIntegral nvl
        yearNrtNvl = round (yearNvl * nrt)
        newFac = case feeBasis of
            AbsoluteValue -> round feeRate -- todo
            NotionalOfUnderlying -> fac + round (yearNvl * feeRate)

-- pamSchedule =


cardanoEpochStart = 1506203091

dayToSlot :: Day -> Integer
dayToSlot d = let
    (MkSystemTime secs _) = utcToSystemTime (UTCTime d 0)
    in fromIntegral secs - cardanoEpochStart `mod` 20

zcb ied md notional discount = (emptyContractConfig ied)
    { maturityDate = Just md
    , notional = notional
    , premiumDiscountAtIED = discount }

cb ied md notional rate = (emptyContractConfig ied)
    { maturityDate = Just md
    , notional = notional
    , nominalInterestRate = rate
    , interestPaymentCycle = Just $ Cycle 3 Month LongLastStub
    , premiumDiscountAtIED = 0 }


genZcbContract investor issuer config@ContractConfig{..} = do
    let maturityDay = fromJust maturityDate
    let maturitySlot = dayToSlot maturityDay
    let state = pamStateInit initialExchangeDate maturityDay
    let schedule = asdf $ contractSchedule config state
    let startDate = dayToSlot initialExchangeDate
    When [Case (ValueEQ (CommittedBy investor) (Constant (notional - premiumDiscountAtIED))) -- Wait for investor to commit
         (When [] startDate
            (Pay [(Constant (notional - premiumDiscountAtIED), issuer)] -- We give issuer the discounted amount
                (Right (When [] maturitySlot  -- by maturity date
                    (Pay [(Constant notional, investor) -- we pay the investor back
                        -- OPTIONAL: return excess to issuer
                        , (SubValue (CommittedBy issuer) (Constant notional), issuer)]
                           (Left investor)))))) -- whatever is left goes back to the guarantor
        ] startDate -- if money is not provided by startDate we return all the money
        (Pay [(CommittedBy investor, investor)] (Left issuer))

genCouponBondContract :: Party -> Party -> ContractConfig -> Contract
genCouponBondContract investor issuer config@ContractConfig{..} = do
    foldr generator (Pay [(CommittedBy investor, investor)] (Left issuer)) sch
  where
    maturityDay = fromJust maturityDate
    maturitySlot :: Integer
    maturitySlot = dayToSlot maturityDay
    state = pamStateInit initialExchangeDate maturityDay
    cs = contractSchedule config state
    schedule = traceShow state $ asdf $ traceShow cs cs
    startDate = traceShow schedule $ dayToSlot initialExchangeDate
    sch = Map.toList schedule
    sum = 1020


    genEvent day event (committed, contract)  = case event of
        IED -> genIED contract
        IP  -> genIP committed 20 (dayToSlot day) contract
        PR  -> genPR committed contract
        _   -> (committed, contract)

    generator (sum, day, events) contract = snd $ foldr (genEvent day) (sum, contract) events

    genIED cont = (0, When [Case (ValueEQ (CommittedBy investor) (Constant notional)) -- Wait for investor to commit
        (When [] startDate (Pay [(Constant notional, issuer)] -- We give issuer the amount
            (Right cont)))] startDate -- if money is not provided by startDate we return all the money
                (Pay [(CommittedBy investor, investor)] (Left issuer)))

    genPR committed cont = let comm = (committed + notional)
        in (comm, When [ Case (ValueGE (CommittedBy issuer) (Constant comm))
                (Pay [(Constant notional, issuer)] (Right cont))] maturitySlot  -- by maturity date
            (Pay [] (Left investor)))

    genIP committed amount slot cont = let comm = committed + amount in
        (comm, When [
            Case (ValueEQ (CommittedBy issuer) (Constant comm))
                (Pay [(Constant amount, issuer)] (Right cont))] slot cont)

    genMD = Left investor

    {-
[x, y, z]

foldl = f (f (f acc x) y) z

foldr = f x (f y (f z acc))

    -}