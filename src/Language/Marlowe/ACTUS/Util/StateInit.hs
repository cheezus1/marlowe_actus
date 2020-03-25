{-# LANGUAGE RecordWildCards #-}
module Language.Marlowe.ACTUS.Util.StateInit where

import Data.List as List
import Data.Maybe
import Data.Time

import Language.Marlowe.ACTUS.Definitions
import Language.Marlowe.ACTUS.Util.Schedule
import Language.Marlowe.ACTUS.Util.Cycle
import Language.Marlowe.ACTUS.Util.Conventions.ContractRoleSign
import Language.Marlowe.ACTUS.Util.Conventions.YearFraction
import Language.Marlowe.ACTUS.Util.NegativeAmortizer as NegativeAmortizer

getDefaultState :: ContractState
getDefaultState =
  ContractState{ t0 = fromGregorian 1970 0 0
              ,  tmd = fromGregorian 1970 0 0
              ,  nt = 0.0
              ,  ipnr = 0.0
              ,  ipac = 0.0
              ,  fac = 0.0
              ,  nsc = 0.0
              ,  isc = 0.0
              ,  prf = CS_PF
              ,  sd = fromGregorian 1970 0 0
              ,  prnxt = 0.0
              ,  ipcb = 0.0
  }

initVariable :: ContractConfig -> StateVariables -> ContractState -> ContractState
initVariable config@ContractConfig{..} S_NT state@ContractState{..}
  | List.elem contractType [PAM, LAM, ANN] =
    let init_nt =
          if initialExchangeDate > t0 then 0.0
          else (contractRoleSign (fromJust contractRole)) * notionalPrincipal
    in
      state { nt = init_nt }

initVariable config@ContractConfig{..} S_IPNR state@ContractState{..}
  | List.elem contractType [PAM, LAM, ANN] =
    let init_ipnr =
          if initialExchangeDate > t0 then 0.0
          else nominalInterestRate
    in
      state { ipnr = init_ipnr }

initVariable config@ContractConfig{..} S_IPAC state@ContractState{..}
  | List.elem contractType [PAM, LAM, ANN] =
    let init_ipac =
          if nominalInterestRate == 0.0 then 0.0
          else
            if isJust accruedInterest then fromJust accruedInterest
            else
              let tPrev = eventScheduleCycleDatesBound config SUP IP (< t0)
                  yearFrac =
                    yearFraction dayCountConvention tPrev t0 (fromJust maturityDate)
              in
                yearFrac * nt * ipnr
    in
      state { ipac = init_ipac }

initVariable config@ContractConfig{..} S_FAC state@ContractState{..}
  | List.elem contractType [PAM, LAM, ANN] =
    let init_fac =
          if isNothing feeRate then 0.0
          else
            if isJust feeAccrued then fromJust feeAccrued
            else
              let tFPPrev = eventScheduleCycleDatesBound config SUP FP (< t0)
                  tFPNext = eventScheduleCycleDatesBound config INF FP (> t0)
              in
                case feeBasis of
                  Just FB_N ->
                    (yearFraction dayCountConvention tFPPrev t0 (fromJust maturityDate)) * nt * (fromJust feeRate)
                  _ ->
                    ((yearFraction dayCountConvention tFPPrev t0 (fromJust maturityDate)) /
                        (yearFraction dayCountConvention tFPPrev tFPNext (fromJust maturityDate))) *
                    (fromJust feeRate)
    in
      state { fac = init_fac }

initVariable config@ContractConfig{..} S_NSC state@ContractState{..}
  | List.elem contractType [PAM, LAM, ANN] =
    let init_nsc =
          if scalingEffect == SE_0N0 || scalingEffect == SE_0NM ||
            scalingEffect == SE_IN0 || scalingEffect == SE_INM then
              (fromJust scalingIndexAtStatusDate)
          else
            1.0
    in
      state { nsc = init_nsc }

initVariable config@ContractConfig{..} S_ISC state@ContractState{..}
  | List.elem contractType [PAM, LAM, ANN] =
    let init_isc =
          if scalingEffect == SE_I00 || scalingEffect == SE_IN0 ||
            scalingEffect == SE_I0M || scalingEffect == SE_INM then
              (fromJust scalingIndexAtStatusDate)
          else
            1.0
    in
      state { isc = init_isc }

initVariable config@ContractConfig{..} S_PRNXT state@ContractState{..}
  | contractType == LAM =
    let init_prnxt =
          if isJust nextPrincipalRedemptionPayment then fromJust nextPrincipalRedemptionPayment
          else
            let prCycle@Cycle{ n = prN } = createCycle (fromJust cycleOfPrincipalRedemption)
                s = if isJust cycleAnchorDateOfPrincipalRedemption &&
                      (fromJust cycleAnchorDateOfPrincipalRedemption) >= t0 then
                        fromJust cycleAnchorDateOfPrincipalRedemption
                    else
                      let prnxtt0' = incrementDate initialExchangeDate prCycle
                      in
                        if isNothing cycleAnchorDateOfPrincipalRedemption && prnxtt0' >= t0 then prnxtt0'
                        else
                          eventScheduleCycleDatesBound config SUP PR (< t0)
            in
              notionalPrincipal *
              (fromIntegral (ceiling
                ((yearFraction dayCountConvention s (fromJust maturityDate) (fromJust maturityDate)) /
                (yearFraction dayCountConvention s (incrementDate initialExchangeDate prCycle) (fromJust maturityDate))
                ))) ** (-1)
    in
      state { prnxt = init_prnxt }
  | contractType == ANN =
    let init_prnxt =
          case nextPrincipalRedemptionPayment of
            Just prnxt ->
              (contractRoleSign (fromJust contractRole)) * prnxt
            Nothing ->
              (notionalPrincipal + ipac) * 1 -- TODO: 1 = todo/todo ?
    in
      state { prnxt = init_prnxt }

initVariable config@ContractConfig{..} S_IPCB state@ContractState{..}
  | List.elem contractType [LAM, ANN] =
    let init_ipcb =
          if t0 < initialExchangeDate then 0.0
          else
            if interestCalculationBase == ICB_NT then
              (contractRoleSign (fromJust contractRole)) * notionalPrincipal
            else
              (contractRoleSign (fromJust contractRole)) * (fromJust interestCalculationBaseAmount)
    in
      state { ipcb = init_ipcb }

initVariable config@ContractConfig{..} S_TMD state@ContractState{..}
  | contractType == PAM =
    state { tmd = fromJust maturityDate }
  | contractType == LAM =
    let init_tmd =
          if isJust maturityDate then fromJust maturityDate
          else
            let prCycle = createCycle (fromJust cycleOfPrincipalRedemption)
                tPrev = if isJust cycleAnchorDateOfPrincipalRedemption &&
                          (fromJust cycleAnchorDateOfPrincipalRedemption) >= t0 then
                            fromJust cycleAnchorDateOfPrincipalRedemption
                        else
                          let tmdt0' = incrementDate initialExchangeDate prCycle
                          in
                            if tmdt0' >= t0 then tmdt0'
                            else
                              eventScheduleCycleDatesBound config SUP PR (< t0)
            in
              incrementDate' tPrev prCycle (ceiling
                    (notionalPrincipal / (fromJust nextPrincipalRedemptionPayment)))
    in
      state { tmd = init_tmd }
  | List.elem contractType [NAM, ANN] =
    state { tmd = NegativeAmortizer.calculateTMDt0 config }

initVariable config@ContractConfig{..} S_SD state@ContractState{..}
  | List.elem contractType [PAM, LAM, ANN] =
    state { sd = t0 }

initVariable config@ContractConfig{..} S_PRF state@ContractState{..}
  | List.elem contractType [PAM, LAM, ANN] =
    state { prf = CS_PF }
