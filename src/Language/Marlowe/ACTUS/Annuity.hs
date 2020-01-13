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
  [IED, IPCI, IP, FP, PR, PY, PP, CD, RRF, RR, PRD, TD, SC, IPCB, AD]

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
      nvl =
        if initialExchangeDate > t0 then 0.0
        else (contractRoleSign (fromJust contractRole)) * notionalPrincipal
      nrt =
        if initialExchangeDate > t0 then 0.0
        else nominalInterestRate
      nac =
        if nominalInterestRate == 0.0 then 0.0
        else
          if isJust accruedInterest then fromJust accruedInterest
          else
            let tPrev = eventScheduleCycleDatesBound config SUP IP (< t0)
                yearFrac =
                  yearFraction dayCountConvention tPrev t0 (fromJust maturityDate)
            in
              yearFrac * nvl * nrt
      fac =
        if isNothing feeRate then 0.0
        else
          if isJust feeAccrued then fromJust feeAccrued
          else
            case feeBasis of
              Just FB_N ->
                let tPrev = eventScheduleCycleDatesBound config SUP IP (< t0) -- TODO: check - missing in comments
                in
                  (yearFraction dayCountConvention tPrev t0 (fromJust maturityDate)) * nvl * (fromJust feeRate)
              _ ->
                let tFPPrev = eventScheduleCycleDatesBound config SUP FP (< t0)
                    tFPNext = eventScheduleCycleDatesBound config INF FP (> t0)
                in
                  ((yearFraction dayCountConvention tFPPrev t0 (fromJust maturityDate)) /
                      (yearFraction dayCountConvention tFPPrev tFPNext (fromJust maturityDate))) *
                  (fromJust feeRate)
      scef = fromJust scalingEffect
      nsc =
        if scef == SE_0N0 || scef == SE_0NM || scef == SE_IN0 || scef == SE_INM then
          (fromJust scalingIndexAtStatusDate)
        else
          1.0
      isc =
        if scef == SE_I00 || scef == SE_IN0 || scef == SE_I0M || scef == SE_INM then
          (fromJust scalingIndexAtStatusDate)
        else
          1.0
      prf = CS_PF
      led = t0
      npr =
        case nextPrincipalRedemptionPayment of
          Just prnxt ->
            (contractRoleSign (fromJust contractRole)) * prnxt
          Nothing ->
            0.0 -- TODO - todo/todo
      icb =
        if interestCalculationBase == ICB_NT then
          (contractRoleSign (fromJust contractRole)) * notionalPrincipal
        else
          (contractRoleSign (fromJust contractRole)) * (fromJust interestCalculationBaseAmount)
    in
      ContractState{ t0 = t0
                   , tmd = tmd
                   , nvl = nvl
                   , nrt = nrt
                   , nac = nac
                   , fac = fac
                   , nsc = nsc
                   , isc = isc
                   , prf = prf
                   , led = led
                   , npr = npr
                   , icb = icb
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
        nvl = nvl
      , nrt = nrt
      , nac = nac
      } = determineStateTransition event date state config
  in
    (traceShow event)
    (traceShow date)
    (traceShow ("event value", payoff))
    (traceShow ("nominal value", nvl))
    (traceShow ("nominal rate", nrt))
    (traceShow ("nominal accrued", nac))
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

determinePayoff event date state config =
  case event of
    IED -> pof_ied_pam state config
    IP  -> pof_ip_lam state config date
    PR  -> pof_pr_nam state config date
    RRF -> pof_rrf_pam state config
    RR  -> pof_rr_pam state config
    FP  -> pof_fp_pam state config date

determineStateTransition event eventDate state config =
  case event of
    IED -> stf_ied_lam state config eventDate
    IP  -> stf_ip_pam state config eventDate
    PR  -> stf_pr_nam state config eventDate
    RRF -> stf_rrf_ann state config eventDate
    RR  -> stf_rr_ann state config eventDate
    FP  -> stf_fp_lam state config eventDate
