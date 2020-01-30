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
import Language.Marlowe.ACTUS.Util.NegativeAmortizer as NegativeAmortizer
import Language.Marlowe.ACTUS.Util.Schedule
import Language.Marlowe.ACTUS.Util.Event
import Language.Marlowe.ACTUS.Util.Conventions.DateShift
import Language.Marlowe.ACTUS.Util.Conventions.ContractRoleSign
import Language.Marlowe.ACTUS.Util.Conventions.YearFraction

loanee :: PubKey
loanee = "alice"

loaner :: PubKey
loaner = "bob"

events :: [Event]
events =
  [AD, IED, PR, MD, PP, PY, FP, PRD, TD, IP, IPCI, IPCB, RR, RRF, SC, CE]

stateInit :: ContractConfig -> ContractState
stateInit config@ContractConfig{..} =
  let t0 = initialExchangeDate
      tmd = NegativeAmortizer.calculateTMDt0 config
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

generateMarlowe :: [(Day, [Event])] -> ContractState -> ContractConfig -> Contract
generateMarlowe [] _ _ =
  Close

generateMarlowe scheduledEvents@((date, events) : _) state config =
  eventsToMarlowe scheduledEvents date events state config

eventsToMarlowe ::
  [(Day, [Event])] -> Day -> [Event] -> ContractState -> ContractConfig -> Contract
eventsToMarlowe [] _ [] _ _ =
  Close

eventsToMarlowe (eventsForDate : rest) _ [] state config =
  generateMarlowe rest state config

eventsToMarlowe scheduledEvents date (event : rest) state@ContractState{..}
  config@ContractConfig{calendar = calendar, businessDayConvention = businessDayConvention} =
    let maybeShiftedEventDate =
          maybeApplyBDC date businessDayConvention calendar
        payoff = determinePayoff state config maybeShiftedEventDate event
        updatedState@ContractState{
          nt = nt
        , ipnr = ipnr
        , ipac = ipac
        } = applyStateTransition state config date event
        (payer, receiver) =
          if payoff > 0.0 then (loaner, loanee)
          else (loanee, loaner)
    in
      (traceShow event)
      (traceShow date)
      (traceShow ("event value", payoff))
      (traceShow ("nominal value", nt))
      (traceShow ("nominal rate", ipnr))
      (traceShow ("nominal accrued", ipac))
      (traceShow updatedState)
      (traceShow "----------------------------")
      (When
        [Case (Deposit (AccountId 0 payer) payer (Constant (round payoff)))
          (Pay (AccountId 0 payer) (Party receiver) (Constant (round payoff))
            (eventsToMarlowe scheduledEvents date rest updatedState config)
          )
        ]
      (Slot 1)
      Close)
