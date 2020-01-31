{-# LANGUAGE RecordWildCards #-}
module Language.Marlowe.ACTUS.PrincipalAtMaturity where

import Data.Maybe

import Language.Marlowe.ACTUS.Definitions
import Language.Marlowe.ACTUS.Util.Schedule
import Language.Marlowe.ACTUS.Util.Conventions.ContractRoleSign
import Language.Marlowe.ACTUS.Util.Conventions.YearFraction

events :: [Event]
events =
  [AD, IED, MD, PP, PY, FP, PRD, TD, IP, IPCI, RR, RRF, SC, CE]

stateInit :: ContractConfig -> ContractState
stateInit config@ContractConfig{..} =
  let t0 = initialExchangeDate
      tmd = fromJust maturityDate
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
  in
    ContractState { t0 = t0
                  , tmd = tmd
                  , nt = nt
                  , ipnr = ipnr
                  , ipac = ipac
                  , fac = fac
                  , nsc = nsc
                  , isc = isc
                  , prf = prf
                  , sd = sd
                  , prnxt = 0.0 -- unused
                  , ipcb = 0.0 -- unused
                  }
