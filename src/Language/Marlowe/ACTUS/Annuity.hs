{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Marlowe.ACTUS.Annuity where

import Language.Marlowe
import Data.Maybe
import Data.Time
import qualified Data.List as List
import Debug.Trace

import Language.Marlowe.ACTUS.Definitions
import Language.Marlowe.ACTUS.Util.Annuity
import Language.Marlowe.ACTUS.Util.Schedule
import Language.Marlowe.ACTUS.Util.Event
import Language.Marlowe.ACTUS.Util.Cycle as Cycle
import Language.Marlowe.ACTUS.Util.Conventions.BusinessDayShift
import Language.Marlowe.ACTUS.Util.Conventions.ContractDefault
import Language.Marlowe.ACTUS.Util.Conventions.ContractRoleSign
import Language.Marlowe.ACTUS.Util.Conventions.YearFraction
import Language.Marlowe.ACTUS.Util.Conventions.DayCount
import Language.Marlowe.ACTUS.Util.Conventions.EndOfMonthShift

loanee :: PubKey
loanee = "alice"

loaner :: PubKey
loaner = "bob"

events =
  [AD, IED, PR, MD, PP, PY, FP, PRD, TD, IP, IPCI, IPCB, RR, RRF, SC, CE]

stateInit config@ContractConfig{..} =
  let t0 = initialExchangeDate
      tmd =
        case maturityDate of
          Just md ->
            md
          Nothing ->
            let prcl = createCycle (fromJust cycleOfPrincipalRedemption)
                tPrev =
                  if isJust cycleAnchorDateOfPrincipalRedemption &&
                    (fromJust cycleAnchorDateOfPrincipalRedemption) >= t0 then
                      fromJust cycleAnchorDateOfPrincipalRedemption
                  else
                    if incrementDate initialExchangeDate prcl >= t0 then
                       incrementDate initialExchangeDate prcl
                    else
                      eventScheduleCycleDatesBound config SUP PR (< t0)
                n = ceiling (notionalPrincipal /
                      ((fromJust nextPrincipalRedemptionPayment) - notionalPrincipal *
                        (yearFraction dayCountConvention tPrev
                          (incrementDate tPrev prcl) (fromJust maturityDate)
                        ) * nominalInterestRate
                      )
                    )
            in
              incrementDate' tPrev prcl n
      nt =
        if initialExchangeDate > t0 then 0.0
        else (contractRoleSign (fromJust contractRole)) * notionalPrincipal
      ipnr =
        if initialExchangeDate > t0 then 0.0
        else nominalInterestRate
      ipac =
        if nominalInterestRate == 0.0 then 0.0
        else
          if isJust accruedInterest then fromJust accruedInterest
          else
            let tPrev = eventScheduleCycleDatesBound config SUP IP (< t0)
                yearFrac =
                  yearFraction dayCountConvention tPrev t0 (fromJust maturityDate)
            in
              yearFrac * nt * ipnr
      fac =
        if isNothing feeRate then 0.0
        else
          if isJust feeAccrued then fromJust feeAccrued
          else
            case feeBasis of
              Just FB_N ->
                let tPrev = eventScheduleCycleDatesBound config SUP IP (< t0) -- TODO: check - missing in comments
                in
                  (yearFraction dayCountConvention tPrev t0 (fromJust maturityDate)) * nt * (fromJust feeRate)
              _ ->
                let tFPPrev = eventScheduleCycleDatesBound config SUP FP (< t0)
                    tFPNext = eventScheduleCycleDatesBound config INF FP (> t0)
                in
                  ((yearFraction dayCountConvention tFPPrev t0 (fromJust maturityDate)) /
                      (yearFraction dayCountConvention tFPPrev tFPNext (fromJust maturityDate))) *
                  (fromJust feeRate)
      nsc =
        if scalingEffect == SE_0N0 || scalingEffect == SE_0NM ||
          scalingEffect == SE_IN0 || scalingEffect == SE_INM then
            (fromJust scalingIndexAtStatusDate)
        else
          1.0
      isc =
        if scalingEffect == SE_I00 || scalingEffect == SE_IN0 ||
          scalingEffect == SE_I0M || scalingEffect == SE_INM then
            (fromJust scalingIndexAtStatusDate)
        else
          1.0
      prf = CS_PF
      sd = t0
      prnxt =
        case nextPrincipalRedemptionPayment of
          Just prnxt ->
            (contractRoleSign (fromJust contractRole)) * prnxt
          Nothing ->
            (notionalPrincipal + ipac) * 1 -- TODO: 1 = todo/todo ?
      ipcb =
        if t0 < initialExchangeDate then
          0.0
        else
          if interestCalculationBase == ICB_NT then
            (contractRoleSign (fromJust contractRole)) * notionalPrincipal
          else
            (contractRoleSign (fromJust contractRole)) * (fromJust interestCalculationBaseAmount)
    in
      ContractState{ t0    = t0
                   , tmd   = tmd
                   , nt    = nt
                   , ipnr  = ipnr
                   , ipac  = ipac
                   , fac   = fac
                   , nsc   = nsc
                   , isc   = isc
                   , prf   = prf
                   , sd    = sd
                   , prnxt = prnxt
                   , ipcb  = ipcb
                   }

generateMarlowe [] _ _ =
  Close

generateMarlowe scheduledEvents@((date, events) : _) state config =
  eventsToMarlowe scheduledEvents date events state config

eventsToMarlowe [] _ [] _ _ =
  Close

eventsToMarlowe (eventsForDate : rest) _ [] state config =
  generateMarlowe rest state config

eventsToMarlowe scheduledEvents date (event : rest) state@ContractState{..} config =
  let payoff = determinePayoff event date state config
      updatedState@ContractState{
        nt = nt
      , ipnr = ipnr
      , ipac = ipac
      } = determineStateTransition event date state config
  in
    (traceShow event)
    (traceShow date)
    (traceShow ("event value", payoff))
    (traceShow ("nominal value", nt))
    (traceShow ("nominal rate", ipnr))
    (traceShow ("nominal accrued", ipac))
    (traceShow updatedState)
    (traceShow "----------------------------")
    (if event == IED then
      When
        [Case (Deposit (AccountId 0 loaner) loaner (Constant (round payoff)))
          (Pay (AccountId 0 loaner) (Party loanee) (Constant (round payoff))
            (eventsToMarlowe scheduledEvents date rest updatedState config)
          )
        ]
      (Slot 1)
      Close
    else
      When
        [Case (Deposit (AccountId 0 loanee) loanee (Constant (round payoff)))
          (Pay (AccountId 0 loanee) (Party loaner) (Constant (round payoff))
            (eventsToMarlowe scheduledEvents date rest updatedState config)
          )
        ]
      (Slot 1)
      Close)

determinePayoff event eventDate state config =
  case event of
    AD   -> pof_ad_pam
    IED  -> pof_ied_pam state config
    PR   -> pof_pr_nam state config eventDate
    MD   -> pof_md_pam state config
    PP   -> pof_pp_pam state config
    PY   -> pof_py_pam state config eventDate
    FP   -> pof_fp_pam state config eventDate
    PRD  -> pof_prd_lam state config eventDate
    TD   -> pof_td_lam state config eventDate
    IP   -> pof_ip_lam state config eventDate
    IPCI -> pof_ipci_pam
    IPCB -> pof_ipcb_lam
    RR   -> pof_rr_pam
    RRF  -> pof_rrf_pam
    SC   -> pof_sc_pam
    CE   -> pof_ce_pam

determineStateTransition event eventDate state config =
  case event of
    AD   -> stf_ad_pam state config eventDate
    IED  -> stf_ied_lam state config eventDate
    PR   -> stf_pr_nam state config eventDate
    MD   -> stf_md_lam state config eventDate
    PP   -> stf_pp_lam state config eventDate
    PY   -> stf_py_lam state config eventDate
    FP   -> stf_fp_lam state config eventDate
    PRD  -> stf_prd_lam state config eventDate
    TD   -> stf_td_pam state config eventDate
    IP   -> stf_ip_pam state config eventDate
    IPCI -> stf_ipci_lam state config eventDate
    IPCB -> stf_ipcb_lam state config eventDate
    RR   -> stf_rr_ann state config eventDate
    RRF  -> stf_rrf_ann state config eventDate
    SC   -> stf_sc_lam state config eventDate
    CE   -> stf_ad_pam state config eventDate
