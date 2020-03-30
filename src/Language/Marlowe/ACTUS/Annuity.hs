{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Marlowe.ACTUS.Annuity where

import Language.Marlowe
import Data.Maybe
import Data.Time
import Data.Time.Clock.System
import qualified Data.List as List
import Debug.Trace
import Flow

import Language.Marlowe.ACTUS.Definitions
import Language.Marlowe.ACTUS.Util.Annuity
import Language.Marlowe.ACTUS.Util.Event
import Language.Marlowe.ACTUS.Util.Conventions.DateShift
import Language.Marlowe.ACTUS.Util.StateInit

loanee :: PubKey
loanee = "alice"

loaner :: PubKey
loaner = "bob"

events :: [Event]
events =
  [AD, IED, PR, MD, PP, PY, FP, PRD, TD, IP, IPCI, IPCB, RR, RRF, SC, CE]

stateInit :: ContractConfig -> ContractState
stateInit config@ContractConfig{..} =
  let defaultState = getDefaultState
  in
    defaultState { t0 = initialExchangeDate }
      |> initVariable config S_TMD
      |> initVariable config S_NT
      |> initVariable config S_IPNR
      |> initVariable config S_IPAC
      |> initVariable config S_FAC
      |> initVariable config S_NSC
      |> initVariable config S_ISC
      |> initVariable config S_PRF
      |> initVariable config S_SD
      |> initVariable config S_PRNXT
      |> initVariable config S_IPCB

cardanoEpochStart :: Integer
cardanoEpochStart = 1506203091

dayToSlot :: Day -> Slot
dayToSlot d = let
    (MkSystemTime secs _) = utcToSystemTime (UTCTime d 0)
    in Slot ((fromIntegral secs - cardanoEpochStart) `div` 20)

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
      (dayToSlot date)
      Close)
