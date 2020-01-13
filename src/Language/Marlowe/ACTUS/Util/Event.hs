{-# LANGUAGE RecordWildCards #-}
module Language.Marlowe.ACTUS.Util.Event where

import Data.Maybe
import Debug.Trace

import Language.Marlowe.ACTUS.Definitions
import Language.Marlowe.ACTUS.Util.Annuity
import Language.Marlowe.ACTUS.Util.Conventions.ContractDefault
import Language.Marlowe.ACTUS.Util.Conventions.ContractRoleSign
import Language.Marlowe.ACTUS.Util.Conventions.YearFraction

-- PAM

pof_ied_pam ContractState{..} ContractConfig{..} =
  1.0 * (contractRoleSign (fromJust contractRole)) * -- TODO: risk factor
    (-1.0) * (notionalPrincipal + premiumDiscountAtIED)

pof_md_pam ContractState{..} ContractConfig{..} =
  1.0 * (nsc - nt + isc * ipac + fac)

pof_ipci_pam ContractState{..} ContractConfig{..} =
  0.0

stf_ip_pam state@ContractState{..} config@ContractConfig{..} eventDate =
  let updated_nac = 0.0
      updated_fac =
        case feeBasis of
          Just FB_N -> fac + (yearFraction dayCountConvention led eventDate (fromJust maturityDate)) * nvl * (fromJust feeRate)
          _ ->
            let tFPPrev = eventScheduleCycleDatesBound config SUP FP (< t0)
                tFPNext = eventScheduleCycleDatesBound config INF FP (> t0)
            in
              ((yearFraction dayCountConvention tFPPrev eventDate (fromJust maturityDate)) /
                (yearFraction dayCountConvention tFPPrev tFPNext (fromJust maturityDate))) *
              (fromJust feeRate)
      updated_led = eventDate
  in
    state { nac = updated_nac
          , fac = updated_fac
          , led = updated_led
          }

pof_fp_pam ContractState{..} ContractConfig{..} eventDate =
  let c = 1.0 * (fromJust feeRate) -- TODO: risk factor
  in
    case feeBasis of
      Just FB_A -> (contractRoleSign (fromJust contractRole)) * c
      Just FB_N -> c * (yearFraction dayCountConvention sd eventDate (fromJust maturityDate)) * nt + fac

pof_py_pam  ContractState{..} ContractConfig{..} eventDate =
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

pof_pp_pam state@ContractState{..} ContractConfig{..} =
  1.0 * 1.0 -- TODO: risk factor x2

pof_cd_pam state@ContractState{..} ContractConfig{..} =
  0.0

pof_rrf_pam state@ContractState{..} ContractConfig{..} =
  0.0

pof_rr_pam state@ContractState{..} ContractConfig{..} =
  0.0

stf_td_pam state@ContractState{..} ContractConfig{..} eventDate =
  state { nvl = 0.0
        , nac = 0.0
        , fac = 0.0
        , nrt = 0.0
        , led = eventDate
        }

pof_sc_pam state@ContractState{..} ContractConfig{..} =
  0.0

pof_ad_pam =
  0.0

stf_ad_pam state@ContractState{..} ContractConfig{..} eventDate =
  let updated_ipac = ipac + (yearFraction dayCountConvention sd eventDate (fromJust maturityDate)) * ipnr * nt
      updated_sd = eventDate
  in
    state { ipac = updated_ipac, sd = updated_sd }

-- LAM

stf_ied_lam state@ContractState{..} ContractConfig{..} eventDate =
  let updated_nt = (contractRoleSign (fromJust contractRole)) * notionalPrincipal
      updated_ipnr = nominalInterestRate
      y =
        yearFraction dayCountConvention
          (fromJust cycleAnchorDateOfInterestPayment) eventDate (fromJust maturityDate)
      updated_ipac =
        if isJust accruedInterest then fromJust accruedInterest
        else
          if (isJust cycleAnchorDateOfInterestPayment) &&
            (fromJust cycleAnchorDateOfInterestPayment) < eventDate then
              y * updated_nt * updated_ipnr
          else 0.0
      updated_sd = eventDate
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

stf_md_lam state@ContractState{..} config@ContractConfig{..} eventDate =
  let updated_nt = 0.0
      updated_ipac = 0.0
      updated_fac = 0.0
      updated_ipcb = 0.0
      updated_sd = eventDate
  in
    state { nt = updated_nt
          , ipac = updated_ipac
          , fac = updated_fac
          , ipcb = updated_ipcb
          , sd = updated_sd}

stf_ipci_lam state@ContractState{..} config@ContractConfig{..} eventDate =
  let yearFraction' = yearFraction dayCountConvention led eventDate (fromJust maturityDate)
      updated_nvl = nvl + nac + yearFraction' * nrt * icb
      updated_nac = 0.0
      updated_fac =
        case feeBasis of
          Just FB_N -> fac + yearFraction' * nvl * (fromJust feeRate)
          _ ->
            let tFPPrev = eventScheduleCycleDatesBound config SUP FP (< t0)
                tFPNext = eventScheduleCycleDatesBound config INF FP (> t0)
            in
              ((yearFraction dayCountConvention tFPPrev eventDate (fromJust maturityDate)) /
                (yearFraction dayCountConvention tFPPrev tFPNext (fromJust maturityDate))) *
              (fromJust feeRate)
      updated_icb =
        if interestCalculationBase /= ICB_NT then icb
        else updated_nvl
      updated_led = eventDate
  in
    state { nvl = updated_nvl
          , nac = updated_nac
          , fac = updated_fac
          , icb = updated_icb
          , led = updated_led
          }

pof_ip_lam state@ContractState{..} ContractConfig{..} eventDate =
  (contractDefault prf) * isc * (nac +
    (yearFraction dayCountConvention led eventDate (fromJust maturityDate)) * nrt * icb)

stf_fp_lam state@ContractState{..} ContractConfig{..} eventDate =
  let updated_ipac = ipac +
        (yearFraction dayCountConvention sd eventDate (fromJust maturityDate)) * ipnr * ipcb
      updated_fac = 0.0
      updated_sd = eventDate
  in
    state { ipac = updated_ipac
          , fac = updated_fac
          , sd = updated_sd
          }

stf_py_lam state@ContractState{..} config@ContractConfig{..} eventDate =
  let yearFraction' = yearFraction dayCountConvention sd eventDate (fromJust maturityDate)
      updated_ipac = ipac + yearFraction' * ipnr * ipcb
      updated_fac =
        case feeBasis of
          Just FB_N -> fac + yearFraction' * nt * (fromJust feeRate)
          _ ->
            let tFPPrev = eventScheduleCycleDatesBound config SUP FP (< t0)
                tFPNext = eventScheduleCycleDatesBound config INF FP (> t0)
            in
              ((yearFraction dayCountConvention tFPPrev eventDate (fromJust maturityDate)) /
                (yearFraction dayCountConvention led eventDate (fromJust maturityDate))) *
              (contractRoleSign (fromJust contractRole)) *
              (fromJust feeRate)
      updated_sd = eventDate
  in
    state { ipac = updated_ipac
          , fac = updated_fac
          , sd = updated_sd
          }

stf_pp_lam state@ContractState{..} config@ContractConfig{..} eventDate =
  let yearFraction' = yearFraction dayCountConvention sd eventDate (fromJust maturityDate)
      updated_ipac = ipac + yearFraction' * ipnr * ipcb
      updated_fac =
        case feeBasis of
          Just FB_N -> fac + yearFraction' * nt * (fromJust feeRate)
          _ ->
            let tFPPrev = eventScheduleCycleDatesBound config SUP FP (< t0)
                tFPNext = eventScheduleCycleDatesBound config INF FP (> t0)
            in
              ((yearFraction dayCountConvention tFPPrev eventDate (fromJust maturityDate)) /
                (yearFraction dayCountConvention tFPPrev tFPNext (fromJust maturityDate))) *
              (contractRoleSign (fromJust contractRole)) *
              (fromJust feeRate)
      updated_nt = nt - 0 -- TODO: risk factor
      updated_ipcb =
        if interestCalculationBase /= ICB_NT then nt
        else ipcb
      updated_sd = eventDate
  in
    state { ipac = updated_ipac
          , fac = updated_fac
          , nt = updated_nt
          , ipcb = updated_ipcb
          , sd = updated_sd
          }

stf_cd_lam state@ContractState{..} config@ContractConfig{..} eventDate =
  let yearFraction' = yearFraction dayCountConvention led eventDate (fromJust maturityDate)
      updated_nac = nac + yearFraction' * nrt * icb
      updated_fac =
        case feeBasis of
          Just FB_N -> fac + yearFraction' * nvl * (fromJust feeRate)
          _ ->
            let tFPPrev = eventScheduleCycleDatesBound config SUP FP (< t0)
                tFPNext = eventScheduleCycleDatesBound config INF FP (> t0)
            in
              ((yearFraction dayCountConvention tFPPrev eventDate (fromJust maturityDate)) /
                (yearFraction dayCountConvention tFPPrev tFPNext (fromJust maturityDate))) *
              (fromJust feeRate)
      updated_prf = CS_DF
      updated_led = eventDate
  in
    state { nac = updated_nac
          , fac = updated_fac
          , prf = updated_prf
          , led = updated_led
          }

pof_prd_lam ContractState{..} ContractConfig{..} eventDate =
  1.0 * (contractRoleSign (fromJust contractRole)) * (-1) * ((fromJust priceAtPurchaseDate) + ipac +
    (yearFraction dayCountConvention sd eventDate (fromJust maturityDate)) * ipnr * ipcb)

stf_prd_lam state@ContractState{..} config@ContractConfig{..} eventDate =
  let yearFraction' = yearFraction dayCountConvention sd eventDate (fromJust maturityDate)
      updated_ipac = ipac + yearFraction' * ipnr * ipcb
      updated_fac =
        case feeBasis of
          Just FB_N -> fac + yearFraction' * nt * (fromJust feeRate)
          _ ->
            let tFPPrev = eventScheduleCycleDatesBound config SUP FP (< t0)
                tFPNext = eventScheduleCycleDatesBound config INF FP (> t0)
            in
              ((yearFraction dayCountConvention tFPPrev eventDate (fromJust maturityDate)) /
                (yearFraction dayCountConvention tFPPrev tFPNext (fromJust maturityDate))) *
              (contractRoleSign (fromJust contractRole)) *
              (fromJust feeRate)
      updated_sd = eventDate
  in
    state { ipac = updated_ipac
          , fac = updated_fac
          , sd = updated_sd
          }

stf_sc_lam state@ContractState{..} config@ContractConfig{..} eventDate =
  let yearFraction' = yearFraction dayCountConvention led eventDate (fromJust maturityDate)
      updated_nac = nac + yearFraction' * nrt * icb
      updated_fac =
        case feeBasis of
          Just FB_N -> fac + yearFraction' * nvl * (fromJust feeRate)
          _ ->
            let tFPPrev = eventScheduleCycleDatesBound config SUP FP (< t0)
                tFPNext = eventScheduleCycleDatesBound config INF FP (> t0)
            in
              ((yearFraction dayCountConvention tFPPrev eventDate (fromJust maturityDate)) /
                (yearFraction dayCountConvention tFPPrev tFPNext (fromJust maturityDate))) *
              (fromJust feeRate)
      scef = fromJust scalingEffect
      updated_nsc =
        if scef == SE_000 || scef == SE_00M || scef == SE_I00 || scef == SE_I0M then
          nsc
        else
          (0 - (fromJust scalingIndexAtStatusDate)) / (fromJust scalingIndexAtStatusDate) -- TODO: risk factor
      updated_isc =
        if scef == SE_000 || scef == SE_0N0 || scef == SE_00M || scef == SE_0NM then
          isc
        else
          (0 - (fromJust scalingIndexAtStatusDate)) / (fromJust scalingIndexAtStatusDate) -- TODO: risk factor
      updated_led = eventDate
  in
    state { nac = updated_nac
          , fac = updated_fac
          , nsc = updated_nsc
          , isc = updated_isc
          , led = updated_led
          }

pof_td_lam ContractState{..} ContractConfig{..} eventDate =
  (contractDefault prf) * (contractRoleSign (fromJust contractRole)) * ((fromJust priceAtTerminationDate) + nac +
    (yearFraction dayCountConvention led eventDate (fromJust maturityDate)) * nrt * icb)

pof_ipcb_lam ContractState{..} ContractConfig{..} eventDate =
  0.0

stf_ipcb_lam state@ContractState{..} config@ContractConfig{..} eventDate =
  let yearFraction' = yearFraction dayCountConvention led eventDate (fromJust maturityDate)
      updated_icb = nvl
      updated_nac = nac + yearFraction' * nrt * icb
      updated_fac =
        case feeBasis of
          Just FB_N -> fac + yearFraction' * nvl * (fromJust feeRate)
          _ ->
            let tFPPrev = eventScheduleCycleDatesBound config SUP FP (< t0)
                tFPNext = eventScheduleCycleDatesBound config INF FP (> t0)
            in
              ((yearFraction dayCountConvention tFPPrev eventDate (fromJust maturityDate)) /
                (yearFraction dayCountConvention tFPPrev tFPNext (fromJust maturityDate))) *
              (fromJust feeRate)
      updated_led = led
  in
    state { icb = updated_icb
          , nac = updated_nac
          , fac = updated_fac
          , led = updated_led
          }


-- LAX
-- NAM

pof_pr_nam ContractState{..} ContractConfig{..} eventDate =
  1.0 * nsc * (prnxt - ipac - (yearFraction dayCountConvention sd eventDate (fromJust maturityDate)) * ipnr * ipcb) -- TODO: risk factor

stf_pr_nam state@ContractState{..} config@ContractConfig{..} eventDate =
  let yearFraction' = yearFraction dayCountConvention sd eventDate (fromJust maturityDate)
      updated_ipac = ipac + yearFraction' * ipnr * ipcb
      updated_nt = nt - (prnxt - updated_ipac)
      updated_fac =
        case feeBasis of
          Just FB_N -> fac + yearFraction' * nt * (fromJust feeRate)
          _ ->
            let tFPPrev = eventScheduleCycleDatesBound config SUP FP (< t0)
                tFPNext = eventScheduleCycleDatesBound config INF FP (> t0)
            in
              ((yearFraction dayCountConvention tFPPrev eventDate (fromJust maturityDate)) /
                (yearFraction dayCountConvention tFPPrev tFPNext (fromJust maturityDate))) *
              contractRoleSign (fromJust contractRole) *
              (fromJust feeRate)
      updated_ipcb =
        if interestCalculationBase /= ICB_NT then icb
        else updated_nt
      updated_sd = eventDate
  in
    state { nt = updated_nt
          , ipac = updated_ipac
          , fac = updated_fac
          , ipcb = updated_ipcb
          , sd = updated_sd
          }

-- ANN

stf_rrf_ann state@ContractState{..} config@ContractConfig{..} eventDate =
  let yearFraction' = yearFraction dayCountConvention led eventDate (fromJust maturityDate)
      updated_nac = nac + yearFraction' * nrt * icb
      updated_fac =
        case feeBasis of
          Just FB_N -> fac + yearFraction' * nvl * (fromJust feeRate)
          _ ->
            let tFPPrev = eventScheduleCycleDatesBound config SUP FP (< t0)
                tFPNext = eventScheduleCycleDatesBound config INF FP (> t0)
            in
              ((yearFraction dayCountConvention tFPPrev eventDate (fromJust maturityDate)) /
                (yearFraction dayCountConvention tFPPrev tFPNext (fromJust maturityDate))) *
              (fromJust feeRate)
      updated_nrt = (fromJust nextResetRate)
      -- TODO:
      -- check (tmd must be updated_tmd??) - Tmdt+ is not stated in the document
      -- check (nvl must be updated_nvl) - Nvlt+ is not stated in the document
      updated_npr = calculateAnnuity config eventDate tmd nvl updated_nac updated_nrt
      updated_led = eventDate
  in
    state { nac = updated_nac
          , fac = updated_fac
          , nrt = updated_nrt
          , led = updated_led
          }

stf_rr_ann state@ContractState{..} config@ContractConfig{..} eventDate =
  let yearFraction' = yearFraction dayCountConvention led eventDate (fromJust maturityDate)
      updated_nac = nac + yearFraction' * nrt * icb
      updated_fac =
        case feeBasis of
          Just FB_N -> fac + yearFraction' * nvl * (fromJust feeRate)
          _ ->
            let tFPPrev = eventScheduleCycleDatesBound config SUP FP (< t0)
                tFPNext = eventScheduleCycleDatesBound config INF FP (> t0)
            in
              ((yearFraction dayCountConvention tFPPrev eventDate (fromJust maturityDate)) /
                (yearFraction dayCountConvention tFPPrev tFPNext (fromJust maturityDate))) *
              (fromJust feeRate)
      updated_nrt = 0.0 -- TODO - risk factor
       -- TODO:
       -- check (tmd must be updated_tmd??) - Tmdt+ is not stated in the document
       -- check (nvl must be updated_nvl) - Nvlt+ is not stated in the document
      updated_npr = calculateAnnuity config eventDate tmd nvl updated_nac updated_nrt
      updated_led = eventDate
  in
    state { nac = updated_nac
          , fac = updated_fac
          , nrt = updated_nrt
          , npr = updated_npr
          , led = updated_led
          }

-- CLM
-- UMP
-- CSH
-- STK
-- COM
-- FXOUT
-- SWPPV
-- SWAPS
-- CAPFL
-- OPTNS
-- FUTUR
-- CEG
-- CEC
