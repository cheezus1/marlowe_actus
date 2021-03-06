{-# LANGUAGE RecordWildCards #-}
module Language.Marlowe.ACTUS.PrincipalAtMaturity where

import Data.Maybe
import Flow

import Language.Marlowe.ACTUS.Definitions
import Language.Marlowe.ACTUS.Util.StateInit

events :: [Event]
events =
  [AD, IED, MD, PP, PY, FP, PRD, TD, IP, IPCI, RR, RRF, SC, CE]

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
