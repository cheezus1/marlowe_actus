{-# LANGUAGE RecordWildCards #-}
module Language.Marlowe.ACTUS.Util.Event where

import Data.List as List
import Data.Maybe
import Data.Time
import Debug.Trace

import Language.Marlowe.ACTUS.Definitions
import Language.Marlowe.ACTUS.Util.Annuity
import Language.Marlowe.ACTUS.Util.Schedule
import Language.Marlowe.ACTUS.Util.Conventions.DateShift
import Language.Marlowe.ACTUS.Util.Conventions.ContractDefault
import Language.Marlowe.ACTUS.Util.Conventions.ContractRoleSign
import Language.Marlowe.ACTUS.Util.Conventions.YearFraction

-- POF

determinePayoff :: ContractState -> ContractConfig -> Day -> Event -> Double
determinePayoff ContractState{..} ContractConfig{..} eventDate IED
  | List.elem contractType [PAM, ANN] =
    1.0 * (contractRoleSign (fromJust contractRole)) * -- TODO: risk factor
      (-1.0) * (notionalPrincipal + premiumDiscountAtIED)

determinePayoff ContractState{..} ContractConfig{..} eventDate MD
  | List.elem contractType [PAM, ANN] =
    1.0 * (nsc - nt + isc * ipac + fac) -- TODO: risk factor

determinePayoff ContractState{..} ContractConfig{..} eventDate IPCI
  | List.elem contractType [PAM, ANN, LAM] =
    0.0

determinePayoff ContractState{..} ContractConfig{..} eventDate FP
  | List.elem contractType [PAM, ANN, LAM] =
    let c = 1.0 * (fromJust feeRate) -- TODO: risk factor
    in
      case feeBasis of
        Just FB_A -> (contractRoleSign (fromJust contractRole)) * c
        Just FB_N -> c * (yearFraction dayCountConvention sd eventDate (fromJust maturityDate)) * nt + fac

determinePayoff ContractState{..} ContractConfig{..} eventDate PY
  | List.elem contractType [PAM, ANN] =
    let c = 1.0 * (contractRoleSign (fromJust contractRole)) * -- TODO: risk factor
            (yearFraction dayCountConvention sd eventDate (fromJust maturityDate)) * nt
    in
      case penaltyType of
        PT_A ->
          1.0 * (contractRoleSign (fromJust contractRole)) * penaltyRate -- TODO: risk factor
        PT_N ->
          c * penaltyRate
        PT_I ->
          c * (max 0 (ipnr - 0)) -- TODO: risk factor

determinePayoff ContractState{..} ContractConfig{..} eventDate PP
  | List.elem contractType [PAM, ANN] =
    1.0 * 1.0 -- TODO: risk factor x2

determinePayoff ContractState{..} ContractConfig{..} eventDate RRF
  | List.elem contractType [PAM, ANN, LAM] =
    0.0

determinePayoff ContractState{..} ContractConfig{..} eventDate RR
  | List.elem contractType [PAM, ANN, LAM] =
    0.0

determinePayoff ContractState{..} ContractConfig{..} eventDate SC
  | List.elem contractType [PAM, ANN, LAM] =
    0.0

determinePayoff ContractState{..} ContractConfig{..} eventDate AD
  | List.elem contractType [PAM, ANN] =
    0.0

determinePayoff ContractState{..} ContractConfig{..} eventDate CE
  | List.elem contractType [PAM, ANN, LAM] =
    0.0

determinePayoff ContractState{..} ContractConfig{..} eventDate IP
  | List.elem contractType [LAM, ANN] =
     -- TODO: risk factor
    1 * isc * (ipac +
      (yearFraction dayCountConvention sd eventDate (fromJust maturityDate)) * ipnr * ipcb)
  | List.elem contractType [PAM] =
     -- TODO: risk factor
    1 * isc * (ipac +
      (yearFraction dayCountConvention sd eventDate (fromJust maturityDate)) * ipnr * nt)

determinePayoff ContractState{..} ContractConfig{..} eventDate PRD
  | List.elem contractType [LAM, ANN] =
     -- TODO: 1 = risk factor
    1 * (contractRoleSign (fromJust contractRole)) * (-1) * ((fromJust priceAtPurchaseDate) + ipac +
      (yearFraction dayCountConvention sd eventDate (fromJust maturityDate)) * ipnr * ipcb)
  | List.elem contractType [PAM] =
    1 * (contractRoleSign (fromJust contractRole)) * (-1) * ((fromJust priceAtPurchaseDate) + ipac +
      (yearFraction dayCountConvention sd eventDate (fromJust maturityDate)) * ipnr * nt)

determinePayoff ContractState{..} ContractConfig{..} eventDate TD
  | List.elem contractType [LAM, ANN] =
     -- TODO: 1 = risk factor
    1 * (contractRoleSign (fromJust contractRole)) * ((fromJust priceAtTerminationDate) + ipac +
      (yearFraction dayCountConvention sd eventDate (fromJust maturityDate)) * ipnr * ipcb)
  | List.elem contractType [PAM] =
    -- TODO: 1 = risk factor
   1 * (contractRoleSign (fromJust contractRole)) * ((fromJust priceAtTerminationDate) + ipac +
     (yearFraction dayCountConvention sd eventDate (fromJust maturityDate)) * ipnr * nt)

determinePayoff ContractState{..} ContractConfig{..} eventDate IPCB
  | List.elem contractType [LAM, ANN] =
    0.0

determinePayoff ContractState{..} ContractConfig{..} eventDate PR
  | List.elem contractType [NAM, ANN] =
    1.0 * nsc * (prnxt - ipac -
      (yearFraction dayCountConvention sd eventDate (fromJust maturityDate)) *
      ipnr * ipcb) -- TODO: risk factor

-- STF

applyStateTransition :: ContractState -> ContractConfig -> Day -> Event -> ContractState
applyStateTransition state@ContractState{..} config@ContractConfig{..} eventDate IP
  | List.elem contractType [PAM, ANN, LAM] =
    let maybeShiftedEventDate = maybeApplyBDC eventDate businessDayConvention calendar
        updated_ipac = 0.0
        updated_fac =
          case feeBasis of
            Just FB_N ->
              fac +
                (yearFraction dayCountConvention sd maybeShiftedEventDate (fromJust maturityDate)) *
                nt * (fromJust feeRate)
            _ ->
              let tFPPrev = eventScheduleCycleDatesBound config SUP FP (< t0)
                  tFPNext = eventScheduleCycleDatesBound config INF FP (> t0)
              in
                ((yearFraction dayCountConvention tFPPrev maybeShiftedEventDate (fromJust maturityDate)) /
                  (yearFraction dayCountConvention tFPPrev tFPNext (fromJust maturityDate))) *
                (contractRoleSign (fromJust contractRole)) * (fromJust feeRate)
        updated_sd = applyBDC eventDate businessDayConvention calendar
    in
      state { ipac = updated_ipac
            , fac  = updated_fac
            , sd   = updated_sd
            }

applyStateTransition state@ContractState{..} ContractConfig{..} eventDate TD
  | List.elem contractType [PAM, ANN, LAM] =
    state { nt   = 0.0
          , ipac = 0.0
          , fac  = 0.0
          , ipnr = 0.0
          , sd   = applyBDC eventDate businessDayConvention calendar
          }

applyStateTransition state@ContractState{..} ContractConfig{..} eventDate AD
  | List.elem contractType [PAM, ANN] =
    let maybeShiftedEventDate = maybeApplyBDC eventDate businessDayConvention calendar
        updated_ipac =
          ipac +
            (yearFraction dayCountConvention sd maybeShiftedEventDate (fromJust maturityDate)) *
            ipnr * nt
        updated_sd = applyBDC eventDate businessDayConvention calendar
    in
      state { ipac = updated_ipac, sd = updated_sd }

applyStateTransition state@ContractState{..} ContractConfig{..} eventDate IED
  | List.elem contractType [LAM, ANN] =
    let maybeShiftedEventDate = maybeApplyBDC eventDate businessDayConvention calendar
        updated_nt = (contractRoleSign (fromJust contractRole)) * notionalPrincipal
        updated_ipnr = nominalInterestRate
        y =
          yearFraction dayCountConvention
            (fromJust cycleAnchorDateOfInterestPayment) maybeShiftedEventDate (fromJust maturityDate)
        updated_ipac =
          if isJust accruedInterest then fromJust accruedInterest
          else
            if (isJust cycleAnchorDateOfInterestPayment) &&
              (fromJust cycleAnchorDateOfInterestPayment) < maybeShiftedEventDate then
                y * updated_nt * updated_ipnr
            else 0.0
        updated_sd = applyBDC eventDate businessDayConvention calendar
        updated_ipcb =
          if interestCalculationBase == ICB_NT then
            (contractRoleSign (fromJust contractRole)) * notionalPrincipal
          else
            (contractRoleSign (fromJust contractRole)) * (fromJust interestCalculationBaseAmount)
    in
      state { nt = updated_nt
            , ipnr = updated_ipnr
            , ipac = updated_ipac
            , sd = updated_sd
            , ipcb = updated_ipcb
            }
  | List.elem contractType [PAM] =
    let maybeShiftedEventDate = maybeApplyBDC eventDate businessDayConvention calendar
        updated_nt = (contractRoleSign (fromJust contractRole)) * notionalPrincipal
        updated_ipnr = nominalInterestRate
        updated_ipac =
          if isJust accruedInterest then fromJust accruedInterest
          else
            if isJust cycleAnchorDateOfInterestPayment &&
              (fromJust cycleAnchorDateOfInterestPayment) < maybeShiftedEventDate then
                let y =
                      yearFraction dayCountConvention
                        (fromJust cycleAnchorDateOfInterestPayment)
                        maybeShiftedEventDate (fromJust maturityDate)
                in
                  y * updated_nt * updated_ipnr
            else 0.0
        updated_sd = applyBDC eventDate businessDayConvention calendar
    in
      state { nt = updated_nt
            , ipnr = updated_ipnr
            , ipac = updated_ipac
            , sd = updated_sd
            }

applyStateTransition state@ContractState{..} ContractConfig{..} eventDate MD
  | List.elem contractType [LAM, ANN] =
    let updated_nt = 0.0
        updated_ipac = 0.0
        updated_fac = 0.0
        updated_ipcb = 0.0
        updated_sd = applyBDC eventDate businessDayConvention calendar
    in
      state { nt = updated_nt
            , ipac = updated_ipac
            , fac = updated_fac
            , ipcb = updated_ipcb
            , sd = updated_sd}
  | List.elem contractType [PAM] =
    let updated_nt = 0.0
        updated_ipac = 0.0
        updated_fac = 0.0
        updated_sd = applyBDC eventDate businessDayConvention calendar
    in
      state { nt = updated_nt
            , ipac = updated_ipac
            , fac = updated_fac
            , sd = updated_sd
            }

applyStateTransition state@ContractState{..} config@ContractConfig{..} eventDate IPCI
  | List.elem contractType [LAM, ANN] =
    let maybeShiftedEventDate = maybeApplyBDC eventDate businessDayConvention calendar
        yearFraction' = yearFraction dayCountConvention sd maybeShiftedEventDate (fromJust maturityDate)
        updated_nt = nt + ipac + yearFraction' * ipnr * ipcb
        updated_ipac = 0.0
        updated_fac =
          case feeBasis of
            Just FB_N -> fac + yearFraction' * nt * (fromJust feeRate)
            _ ->
              let tFPPrev = eventScheduleCycleDatesBound config SUP FP (< t0)
                  tFPNext = eventScheduleCycleDatesBound config INF FP (> t0)
              in
                ((yearFraction dayCountConvention tFPPrev maybeShiftedEventDate (fromJust maturityDate)) /
                  (yearFraction dayCountConvention tFPPrev tFPNext (fromJust maturityDate))) *
                (fromJust feeRate)
        updated_ipcb =
          if interestCalculationBase /= ICB_NT then ipcb
          else updated_nt
        updated_sd = applyBDC eventDate businessDayConvention calendar
    in
      state { nt   = updated_nt
            , ipac = updated_ipac
            , fac  = updated_fac
            , ipcb = updated_ipcb
            , sd   = updated_sd
            }
    | List.elem contractType [PAM] =
      let maybeShiftedEventDate = maybeApplyBDC eventDate businessDayConvention calendar
          yearFraction' = yearFraction dayCountConvention sd maybeShiftedEventDate (fromJust maturityDate)
          updated_nt = nt + ipac + yearFraction' * nt * ipnr
          updated_ipac = 0.0
          updated_fac =
            case feeBasis of
              Just FB_N -> fac + yearFraction' * nt * (fromJust feeRate)
              _ ->
                let tFPPrev = eventScheduleCycleDatesBound config SUP FP (< t0)
                    tFPNext = eventScheduleCycleDatesBound config INF FP (> t0)
                in
                  ((yearFraction dayCountConvention tFPPrev maybeShiftedEventDate (fromJust maturityDate)) /
                    (yearFraction dayCountConvention tFPPrev tFPNext (fromJust maturityDate))) *
                  (fromJust feeRate)
          updated_sd = applyBDC eventDate businessDayConvention calendar
      in
        state { nt   = updated_nt
              , ipac = updated_ipac
              , fac  = updated_fac
              , sd   = updated_sd
              }

applyStateTransition state@ContractState{..} ContractConfig{..} eventDate FP
  | List.elem contractType [LAM, ANN] =
    let maybeShiftedEventDate = maybeApplyBDC eventDate businessDayConvention calendar
        updated_ipac = ipac +
          (yearFraction dayCountConvention sd maybeShiftedEventDate (fromJust maturityDate)) * ipnr * ipcb
        updated_fac = 0.0
        updated_sd = applyBDC eventDate businessDayConvention calendar
    in
      state { ipac = updated_ipac
            , fac = updated_fac
            , sd = updated_sd
            }
  | List.elem contractType [PAM] =
    let maybeShiftedEventDate = maybeApplyBDC eventDate businessDayConvention calendar
        updated_ipac = ipac +
          (yearFraction dayCountConvention sd maybeShiftedEventDate (fromJust maturityDate)) * ipnr * nt
        updated_fac = 0.0
        updated_sd = applyBDC eventDate businessDayConvention calendar
    in
      state { ipac = updated_ipac
            , fac = updated_fac
            , sd = updated_sd
            }

applyStateTransition state@ContractState{..} config@ContractConfig{..} eventDate PY
  | List.elem contractType [LAM, ANN] =
    let maybeShiftedEventDate = maybeApplyBDC eventDate businessDayConvention calendar
        yearFraction' = yearFraction dayCountConvention sd maybeShiftedEventDate (fromJust maturityDate)
        updated_ipac = ipac + yearFraction' * ipnr * ipcb
        updated_fac =
          case feeBasis of
            Just FB_N -> fac + yearFraction' * nt * (fromJust feeRate)
            _ ->
              let tFPPrev = eventScheduleCycleDatesBound config SUP FP (< t0)
                  tFPNext = eventScheduleCycleDatesBound config INF FP (> t0)
              in
                ((yearFraction dayCountConvention tFPPrev maybeShiftedEventDate (fromJust maturityDate)) /
                  (yearFraction dayCountConvention tFPPrev tFPNext (fromJust maturityDate))) *
                (contractRoleSign (fromJust contractRole)) *
                (fromJust feeRate)
        updated_sd = applyBDC eventDate businessDayConvention calendar
    in
      state { ipac = updated_ipac
            , fac = updated_fac
            , sd = updated_sd
            }
  | List.elem contractType [PAM] =
    let maybeShiftedEventDate = maybeApplyBDC eventDate businessDayConvention calendar
        yearFraction' = yearFraction dayCountConvention sd maybeShiftedEventDate (fromJust maturityDate)
        updated_ipac = ipac + yearFraction' * ipnr * nt
        updated_fac =
          case feeBasis of
            Just FB_N -> fac + yearFraction' * nt * (fromJust feeRate)
            _ ->
              let tFPPrev = eventScheduleCycleDatesBound config SUP FP (< t0)
                  tFPNext = eventScheduleCycleDatesBound config INF FP (> t0)
              in
                ((yearFraction dayCountConvention tFPPrev maybeShiftedEventDate (fromJust maturityDate)) /
                  (yearFraction dayCountConvention tFPPrev tFPNext (fromJust maturityDate))) *
                (contractRoleSign (fromJust contractRole)) *
                (fromJust feeRate)
        updated_sd = applyBDC eventDate businessDayConvention calendar
    in
      state { ipac = updated_ipac
            , fac = updated_fac
            , sd = updated_sd
            }

applyStateTransition state@ContractState{..} config@ContractConfig{..} eventDate PP
  | List.elem contractType [LAM, ANN] =
    let maybeShiftedEventDate = maybeApplyBDC eventDate businessDayConvention calendar
        yearFraction' = yearFraction dayCountConvention sd maybeShiftedEventDate (fromJust maturityDate)
        updated_ipac = ipac + yearFraction' * ipnr * ipcb
        updated_fac =
          case feeBasis of
            Just FB_N -> fac + yearFraction' * nt * (fromJust feeRate)
            _ ->
              let tFPPrev = eventScheduleCycleDatesBound config SUP FP (< t0)
                  tFPNext = eventScheduleCycleDatesBound config INF FP (> t0)
              in
                ((yearFraction dayCountConvention tFPPrev maybeShiftedEventDate (fromJust maturityDate)) /
                  (yearFraction dayCountConvention tFPPrev tFPNext (fromJust maturityDate))) *
                (contractRoleSign (fromJust contractRole)) *
                (fromJust feeRate)
        updated_nt = nt - 0 -- TODO: risk factor
        updated_ipcb =
          if interestCalculationBase /= ICB_NT then nt
          else ipcb
        updated_sd = applyBDC eventDate businessDayConvention calendar
    in
      state { ipac = updated_ipac
            , fac = updated_fac
            , nt = updated_nt
            , ipcb = updated_ipcb
            , sd = updated_sd
            }
  | List.elem contractType [PAM] =
    let maybeShiftedEventDate = maybeApplyBDC eventDate businessDayConvention calendar
        yearFraction' = yearFraction dayCountConvention sd maybeShiftedEventDate (fromJust maturityDate)
        updated_ipac = ipac * yearFraction' * ipnr * nt
        updated_fac =
          case feeBasis of
            Just FB_N -> fac + yearFraction' * nt * (fromJust feeRate)
            _ ->
              let tFPPrev = eventScheduleCycleDatesBound config SUP FP (< t0)
                  tFPNext = eventScheduleCycleDatesBound config INF FP (> t0)
              in
                ((yearFraction dayCountConvention tFPPrev maybeShiftedEventDate (fromJust maturityDate)) /
                  (yearFraction dayCountConvention tFPPrev tFPNext (fromJust maturityDate))) *
                (contractRoleSign (fromJust contractRole)) *
                (fromJust feeRate)
        updated_nt = nt - 0 -- TODO: risk factor
        updated_sd = applyBDC eventDate businessDayConvention calendar
    in
      state { ipac = updated_ipac
            , fac = updated_fac
            , nt = updated_nt
            , sd = updated_sd
            }

applyStateTransition state@ContractState{..} config@ContractConfig{..} eventDate PRD
  | List.elem contractType [LAM, ANN] =
    let maybeShiftedEventDate = maybeApplyBDC eventDate businessDayConvention calendar
        yearFraction' = yearFraction dayCountConvention sd maybeShiftedEventDate (fromJust maturityDate)
        updated_ipac = ipac + yearFraction' * ipnr * ipcb
        updated_fac =
          case feeBasis of
            Just FB_N -> fac + yearFraction' * nt * (fromJust feeRate)
            _ ->
              let tFPPrev = eventScheduleCycleDatesBound config SUP FP (< t0)
                  tFPNext = eventScheduleCycleDatesBound config INF FP (> t0)
              in
                ((yearFraction dayCountConvention tFPPrev maybeShiftedEventDate (fromJust maturityDate)) /
                  (yearFraction dayCountConvention tFPPrev tFPNext (fromJust maturityDate))) *
                (contractRoleSign (fromJust contractRole)) *
                (fromJust feeRate)
        updated_sd = applyBDC eventDate businessDayConvention calendar
    in
      state { ipac = updated_ipac
            , fac = updated_fac
            , sd = updated_sd
            }
  | List.elem contractType [PAM] =
    let maybeShiftedEventDate = maybeApplyBDC eventDate businessDayConvention calendar
        yearFraction' = yearFraction dayCountConvention sd maybeShiftedEventDate (fromJust maturityDate)
        updated_ipac = ipac + yearFraction' * ipnr * nt
        updated_fac =
          case feeBasis of
            Just FB_N -> fac + yearFraction' * nt * (fromJust feeRate)
            _ ->
              let tFPPrev = eventScheduleCycleDatesBound config SUP FP (< t0)
                  tFPNext = eventScheduleCycleDatesBound config INF FP (> t0)
              in
                ((yearFraction dayCountConvention tFPPrev maybeShiftedEventDate (fromJust maturityDate)) /
                  (yearFraction dayCountConvention tFPPrev tFPNext (fromJust maturityDate))) *
                (contractRoleSign (fromJust contractRole)) *
                (fromJust feeRate)
        updated_sd = applyBDC eventDate businessDayConvention calendar
    in
      state { ipac = updated_ipac
            , fac = updated_fac
            , sd = updated_sd
            }

applyStateTransition state@ContractState{..} config@ContractConfig{..} eventDate SC
  | List.elem contractType [LAM, ANN] =
    let maybeShiftedEventDate = maybeApplyBDC eventDate businessDayConvention calendar
        yearFraction' = yearFraction dayCountConvention sd maybeShiftedEventDate (fromJust maturityDate)
        updated_ipac = ipac + yearFraction' * ipnr * ipcb
        updated_fac =
          case feeBasis of
            Just FB_N -> fac + yearFraction' * nt * (fromJust feeRate)
            _ ->
              let tFPPrev = eventScheduleCycleDatesBound config SUP FP (< t0)
                  tFPNext = eventScheduleCycleDatesBound config INF FP (> t0)
              in
                ((yearFraction dayCountConvention tFPPrev maybeShiftedEventDate (fromJust maturityDate)) /
                  (yearFraction dayCountConvention tFPPrev tFPNext (fromJust maturityDate))) *
                (contractRoleSign (fromJust contractRole)) * (fromJust feeRate)
        updated_nsc =
          if scalingEffect == SE_000 || scalingEffect == SE_00M ||
            scalingEffect == SE_I00 || scalingEffect == SE_I0M then
              nsc
          else
            (0 - (fromJust scalingIndexAtStatusDate)) / (fromJust scalingIndexAtStatusDate) -- TODO: risk factor
        updated_isc =
          if scalingEffect == SE_000 || scalingEffect == SE_0N0 ||
            scalingEffect == SE_00M || scalingEffect == SE_0NM then
              isc
          else
            (0 - (fromJust scalingIndexAtStatusDate)) / (fromJust scalingIndexAtStatusDate) -- TODO: risk factor
        updated_sd = applyBDC eventDate businessDayConvention calendar
    in
      state { ipac = updated_ipac
            , fac  = updated_fac
            , nsc  = updated_nsc
            , isc  = updated_isc
            , sd   = updated_sd
            }
  | List.elem contractType [PAM] =
    let maybeShiftedEventDate = maybeApplyBDC eventDate businessDayConvention calendar
        yearFraction' = yearFraction dayCountConvention sd maybeShiftedEventDate (fromJust maturityDate)
        updated_ipac = ipac + yearFraction' * ipnr * nt
        updated_fac =
          case feeBasis of
            Just FB_N -> fac + yearFraction' * nt * (fromJust feeRate)
            _ ->
              let tFPPrev = eventScheduleCycleDatesBound config SUP FP (< t0)
                  tFPNext = eventScheduleCycleDatesBound config INF FP (> t0)
              in
                ((yearFraction dayCountConvention tFPPrev maybeShiftedEventDate (fromJust maturityDate)) /
                  (yearFraction dayCountConvention tFPPrev tFPNext (fromJust maturityDate))) *
                (contractRoleSign (fromJust contractRole)) * (fromJust feeRate)
        updated_nsc =
          if scalingEffect == SE_000 || scalingEffect == SE_00M ||
            scalingEffect == SE_I00 || scalingEffect == SE_I0M then
              nsc
          else
            (0 - (fromJust scalingIndexAtStatusDate)) / (fromJust scalingIndexAtStatusDate) -- TODO: risk factor
        updated_isc =
          if scalingEffect == SE_000 || scalingEffect == SE_0N0 ||
            scalingEffect == SE_00M || scalingEffect == SE_0NM then
              isc
          else
            (0 - (fromJust scalingIndexAtStatusDate)) / (fromJust scalingIndexAtStatusDate) -- TODO: risk factor
        updated_sd = applyBDC eventDate businessDayConvention calendar
    in
      state { ipac = updated_ipac
            , fac  = updated_fac
            , nsc  = updated_nsc
            , isc  = updated_isc
            , sd   = updated_sd
            }

applyStateTransition state@ContractState{..} config@ContractConfig{..} eventDate IPCB
  | List.elem contractType [LAM, ANN] =
    let maybeShiftedEventDate = maybeApplyBDC eventDate businessDayConvention calendar
        yearFraction' = yearFraction dayCountConvention sd maybeShiftedEventDate (fromJust maturityDate)
        updated_ipcb = nt
        updated_ipac = ipac + yearFraction' * ipnr * ipcb
        updated_fac =
          case feeBasis of
            Just FB_N -> fac + yearFraction' * nt * (fromJust feeRate)
            _ ->
              let tFPPrev = eventScheduleCycleDatesBound config SUP FP (< t0)
                  tFPNext = eventScheduleCycleDatesBound config INF FP (> t0)
              in
                ((yearFraction dayCountConvention tFPPrev maybeShiftedEventDate (fromJust maturityDate)) /
                  (yearFraction dayCountConvention tFPPrev tFPNext (fromJust maturityDate))) *
                (contractRoleSign (fromJust contractRole)) * (fromJust feeRate)
        updated_sd = applyBDC eventDate businessDayConvention calendar
    in
      state { ipcb = updated_ipcb
            , ipac = updated_ipac
            , fac  = updated_fac
            , sd   = updated_sd
            }

applyStateTransition state@ContractState{..} config@ContractConfig{..} eventDate PR
  | List.elem contractType [NAM, ANN] =
    let maybeShiftedEventDate = maybeApplyBDC eventDate businessDayConvention calendar
        yearFraction' = yearFraction dayCountConvention sd maybeShiftedEventDate (fromJust maturityDate)
        updated_ipac = ipac + yearFraction' * ipnr * ipcb
        updated_nt = nt - (prnxt - updated_ipac)
        updated_fac =
          case feeBasis of
            Just FB_N -> fac + yearFraction' * nt * (fromJust feeRate)
            _ ->
              let tFPPrev = eventScheduleCycleDatesBound config SUP FP (< t0)
                  tFPNext = eventScheduleCycleDatesBound config INF FP (> t0)
              in
                ((yearFraction dayCountConvention tFPPrev maybeShiftedEventDate (fromJust maturityDate)) /
                  (yearFraction dayCountConvention tFPPrev tFPNext (fromJust maturityDate))) *
                contractRoleSign (fromJust contractRole) *
                (fromJust feeRate)
        updated_ipcb =
          if interestCalculationBase /= ICB_NT then ipcb
          else updated_nt
        updated_sd = applyBDC eventDate businessDayConvention calendar
    in
      state { nt = updated_nt
            , ipac = updated_ipac
            , fac = updated_fac
            , ipcb = updated_ipcb
            , sd = updated_sd
            }

applyStateTransition state@ContractState{..} config@ContractConfig{..} eventDate RRF
| contractType == LAM =
  let maybeShiftedEventDate = maybeApplyBDC eventDate businessDayConvention calendar
      yearFraction' = yearFraction dayCountConvention sd maybeShiftedEventDate (fromJust maturityDate)
      updated_ipac = ipac + yearFraction' * ipnr * ipcb
      updated_fac =
        case feeBasis of
          Just FB_N -> fac + yearFraction' * nt * (fromJust feeRate)
          _ ->
            let tFPPrev = eventScheduleCycleDatesBound config SUP FP (< t0)
                tFPNext = eventScheduleCycleDatesBound config INF FP (> t0)
            in
              ((yearFraction dayCountConvention tFPPrev maybeShiftedEventDate (fromJust maturityDate)) /
                (yearFraction dayCountConvention tFPPrev tFPNext (fromJust maturityDate))) *
              (contractRoleSign (fromJust contractRole)) * (fromJust feeRate)
      updated_ipnr = (fromJust nextResetRate)
      -- TODO:
      -- check (tmd must be updated_tmd??) - Tmdt+ is not stated in the document
      -- check (nt must be updated_nt) - Nt+ is not stated in the document
      updated_sd = applyBDC eventDate businessDayConvention calendar
  in
    state { ipac  = updated_ipac
          , fac   = updated_fac
          , ipnr  = updated_ipnr
          , sd    = updated_sd
          }
  | contractType == ANN =
    let maybeShiftedEventDate = maybeApplyBDC eventDate businessDayConvention calendar
        yearFraction' = yearFraction dayCountConvention sd maybeShiftedEventDate (fromJust maturityDate)
        updated_ipac = ipac + yearFraction' * ipnr * ipcb
        updated_fac =
          case feeBasis of
            Just FB_N -> fac + yearFraction' * nt * (fromJust feeRate)
            _ ->
              let tFPPrev = eventScheduleCycleDatesBound config SUP FP (< t0)
                  tFPNext = eventScheduleCycleDatesBound config INF FP (> t0)
              in
                ((yearFraction dayCountConvention tFPPrev maybeShiftedEventDate (fromJust maturityDate)) /
                  (yearFraction dayCountConvention tFPPrev tFPNext (fromJust maturityDate))) *
                (contractRoleSign (fromJust contractRole)) * (fromJust feeRate)
        updated_ipnr = (fromJust nextResetRate)
        -- TODO:
        -- check (tmd must be updated_tmd??) - Tmdt+ is not stated in the document
        -- check (nt must be updated_nt) - Nt+ is not stated in the document
        updated_prnxt = calculateAnnuity config maybeShiftedEventDate tmd nt updated_ipac updated_ipnr
        updated_sd = applyBDC eventDate businessDayConvention calendar
    in
      state { ipac  = updated_ipac
            , fac   = updated_fac
            , ipnr  = updated_ipnr
            , prnxt = updated_prnxt
            , sd    = updated_sd
            }
  | contractType == PAM =
    let maybeShiftedEventDate = maybeApplyBDC eventDate businessDayConvention calendar
        yearFraction' = yearFraction dayCountConvention sd maybeShiftedEventDate (fromJust maturityDate)
        updated_ipac = ipac + yearFraction' * ipnr * nt
        updated_fac =
          case feeBasis of
            Just FB_N -> fac + yearFraction' * nt * (fromJust feeRate)
            _ ->
              let tFPPrev = eventScheduleCycleDatesBound config SUP FP (< t0)
                  tFPNext = eventScheduleCycleDatesBound config INF FP (> t0)
              in
                ((yearFraction dayCountConvention tFPPrev maybeShiftedEventDate (fromJust maturityDate)) /
                  (yearFraction dayCountConvention tFPPrev tFPNext (fromJust maturityDate))) *
                (contractRoleSign (fromJust contractRole)) * (fromJust feeRate)
        updated_ipnr = (fromJust nextResetRate)
        updated_sd = applyBDC eventDate businessDayConvention calendar
    in
      state { ipac  = updated_ipac
            , fac   = updated_fac
            , ipnr  = updated_ipnr
            , sd    = updated_sd
            }

applyStateTransition state@ContractState{..} config@ContractConfig{..} eventDate RR
  | contractType == LAM =
    let maybeShiftedEventDate = maybeApplyBDC eventDate businessDayConvention calendar
        yearFraction' = yearFraction dayCountConvention sd maybeShiftedEventDate (fromJust maturityDate)
        updated_ipac = ipac + yearFraction' * ipnr * ipcb
        updated_fac =
          case feeBasis of
            Just FB_N -> fac + yearFraction' * nt * (fromJust feeRate)
            _ ->
              let tFPPrev = eventScheduleCycleDatesBound config SUP FP (< t0)
                  tFPNext = eventScheduleCycleDatesBound config INF FP (> t0)
              in
                ((yearFraction dayCountConvention tFPPrev maybeShiftedEventDate (fromJust maturityDate)) /
                  (yearFraction dayCountConvention tFPPrev tFPNext (fromJust maturityDate))) *
                (contractRoleSign (fromJust contractRole)) * (fromJust feeRate)
        orf = 1 -- TODO: risk factor
        deltaR =
          min
            (max
              (orf * rateMultiplier + rateSpread - ipnr)
              (fromJust periodFloor))
            (fromJust periodCap)
        updated_ipnr =
          min
            (max
              (ipnr + deltaR)
              (fromJust lifeFloor))
            (fromJust lifeCap)
        -- TODO:
        -- check (tmd must be updated_tmd??) - Tmdt+ is not stated in the document
        -- check (nt must be updated_nt) - Nt+ is not stated in the document
        updated_sd = applyBDC eventDate businessDayConvention calendar
    in
      state { ipac  = updated_ipac
            , fac   = updated_fac
            , ipnr  = updated_ipnr
            , sd    = updated_sd
            }
  | contractType == ANN =
    let maybeShiftedEventDate = maybeApplyBDC eventDate businessDayConvention calendar
        yearFraction' = yearFraction dayCountConvention sd maybeShiftedEventDate (fromJust maturityDate)
        updated_ipac = ipac + yearFraction' * ipnr * ipcb
        updated_fac =
          case feeBasis of
            Just FB_N -> fac + yearFraction' * nt * (fromJust feeRate)
            _ ->
              let tFPPrev = eventScheduleCycleDatesBound config SUP FP (< t0)
                  tFPNext = eventScheduleCycleDatesBound config INF FP (> t0)
              in
                ((yearFraction dayCountConvention tFPPrev maybeShiftedEventDate (fromJust maturityDate)) /
                  (yearFraction dayCountConvention tFPPrev tFPNext (fromJust maturityDate))) *
                (contractRoleSign (fromJust contractRole)) * (fromJust feeRate)
        orf = 1 -- TODO: risk factor
        deltaR =
          min
            (max
              (orf * rateMultiplier + rateSpread - ipnr)
              (fromJust periodFloor))
            (fromJust periodCap)
        updated_ipnr =
          min
            (max
              (ipnr + deltaR)
              (fromJust lifeFloor))
            (fromJust lifeCap)
        -- TODO:
        -- check (tmd must be updated_tmd??) - Tmdt+ is not stated in the document
        -- check (nt must be updated_nt) - Nt+ is not stated in the document
        updated_prnxt = calculateAnnuity config maybeShiftedEventDate tmd nt updated_ipac updated_ipnr
        updated_sd = applyBDC eventDate businessDayConvention calendar
    in
      state { ipac  = updated_ipac
            , fac   = updated_fac
            , ipnr  = updated_ipnr
            , prnxt = updated_prnxt
            , sd    = updated_sd
            }
  | contractType == PAM =
    let maybeShiftedEventDate = maybeApplyBDC eventDate businessDayConvention calendar
        yearFraction' = yearFraction dayCountConvention sd maybeShiftedEventDate (fromJust maturityDate)
        updated_ipac = ipac + yearFraction' * ipnr * nt
        updated_fac =
          case feeBasis of
            Just FB_N -> fac + yearFraction' * nt * (fromJust feeRate)
            _ ->
              let tFPPrev = eventScheduleCycleDatesBound config SUP FP (< t0)
                  tFPNext = eventScheduleCycleDatesBound config INF FP (> t0)
              in
                ((yearFraction dayCountConvention tFPPrev maybeShiftedEventDate (fromJust maturityDate)) /
                  (yearFraction dayCountConvention tFPPrev tFPNext (fromJust maturityDate))) *
                (contractRoleSign (fromJust contractRole)) * (fromJust feeRate)
        orf = 1 -- TODO: risk factor
        deltaR =
          min
            (max
              (orf * rateMultiplier + rateSpread - ipnr)
              (fromJust periodFloor))
            (fromJust periodCap)
        updated_ipnr =
          min
            (max
              (ipnr + deltaR)
              (fromJust lifeFloor))
            (fromJust lifeCap)
        updated_sd = applyBDC eventDate businessDayConvention calendar
    in
      state { ipac  = updated_ipac
            , fac   = updated_fac
            , ipnr  = updated_ipnr
            , sd    = updated_sd
            }

applyStateTransition state@ContractState{..} config@ContractConfig{..} eventDate CE
  | List.elem contractType [PAM, ANN, LAM] =
    applyStateTransition state config eventDate AD
