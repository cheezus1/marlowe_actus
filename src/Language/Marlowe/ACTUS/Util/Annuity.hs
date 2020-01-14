{-# LANGUAGE RecordWildCards #-}
module Language.Marlowe.ACTUS.Util.Annuity where

import Data.Maybe
import qualified Data.List as List

import Language.Marlowe.ACTUS.Definitions
import Language.Marlowe.ACTUS.Util.Schedule
import Language.Marlowe.ACTUS.Util.Cycle as Cycle
import Language.Marlowe.ACTUS.Util.Conventions.YearFraction

generateSchedules events config =
  generateSchedules' events config []

generateSchedules' [] _ schedules =
  schedules

generateSchedules' (event : rest) contractConfig@ContractConfig{..} schedules =
  let additionalSchedules =
        case event of
          AD ->
            [] -- TODO: find out where custom input is provided
          IED ->
            [(IED, Schedule{
              s = Just initialExchangeDate
            , c = Nothing
            , t = Nothing
            , b = True
            , dateToExclude = Nothing
            })]
          PR ->
            let s =
                  if isNothing cycleAnchorDateOfPrincipalRedemption &&
                    isNothing cycleOfPrincipalRedemption then
                      Nothing
                  else
                    if isNothing cycleAnchorDateOfPrincipalRedemption then
                      Just (incrementDate initialExchangeDate
                        (Cycle.createCycle (fromJust cycleOfPrincipalRedemption)))
                    else
                      cycleAnchorDateOfPrincipalRedemption
            in
              [(PR, Schedule{
                s = s
              , c = Just (Cycle.createCycle (fromJust cycleOfPrincipalRedemption))
              , t = maturityDate
              , b = False
              , dateToExclude = Nothing
              })]
          MD ->
            [(MD, Schedule{
              s = maturityDate
            , c = Nothing
            , t = Nothing
            , b = True
            , dateToExclude = Nothing
            })]
          PP ->
            let s =
                  if isNothing cycleAnchorDateOfOptionality &&
                    isNothing cycleOfOptionality then
                      Nothing
                  else
                    if isNothing cycleAnchorDateOfOptionality then
                      Just (incrementDate initialExchangeDate
                        (Cycle.createCycle (fromJust cycleOfOptionality)))
                    else
                      cycleAnchorDateOfOptionality
                scheduleU =
                  [(PP, Schedule{
                    s = s
                  , c = Just (Cycle.createCycle (fromJust cycleOfOptionality))
                  , t = maturityDate
                  , b = True
                  , dateToExclude = Nothing
                  })]
                scheduleV = [] -- TODO: risk factor
            in
              if prepaymentEffect == PE_N then
                []
              else
                scheduleU ++ scheduleV
          PY ->
            if penaltyType == PT_O then
              []
            else
              generateSchedules [PP] contractConfig
          FP ->
            let s =
                  if isNothing cycleAnchorDateOfFee && isNothing cycleOfFee then
                    Nothing
                  else
                    if isNothing cycleAnchorDateOfFee then
                      Just (incrementDate initialExchangeDate
                        (Cycle.createCycle (fromJust cycleOfFee)))
                    else
                      cycleAnchorDateOfFee
            in
              if isNothing feeRate || (fromJust feeRate) == 0.0 then
                []
              else
                [(FP, Schedule{
                  s = s
                , c = Just (Cycle.createCycle (fromJust cycleOfFee))
                , t = maturityDate
                , b = True
                , dateToExclude = Nothing
                })]
          PRD ->
            [(PRD, Schedule{
              s = purchaseDate
            , c = Nothing
            , t = Nothing
            , b = True
            , dateToExclude = Nothing
            })]
          TD ->
            [(TD, Schedule{
              s = terminationDate
            , c = Nothing
            , t = Nothing
            , b = True
            , dateToExclude = Nothing
            })]
          IP ->
            let r =
                  if isJust capitalizationEndDate then capitalizationEndDate
                  else
                    if isJust cycleAnchorDateOfInterestPayment then
                      cycleAnchorDateOfInterestPayment
                    else
                      if isJust cycleOfInterestPayment then
                        Just (incrementDate initialExchangeDate
                          (Cycle.createCycle (fromJust cycleOfInterestPayment)))
                      else
                        Nothing
                s =
                  if isNothing cycleAnchorDateOfPrincipalRedemption then
                    Just (incrementDate initialExchangeDate
                      (Cycle.createCycle (fromJust cycleOfPrincipalRedemption)))
                  else
                    cycleAnchorDateOfPrincipalRedemption
                t = Just (decrementDate (fromJust s) (Cycle.createCycle (fromJust cycleOfPrincipalRedemption)))
                scheduleU =
                  if (isNothing cycleAnchorDateOfInterestPayment) && (isNothing cycleOfInterestPayment) then
                    []
                  else
                    if (isJust capitalizationEndDate) && (fromJust capitalizationEndDate) >= (fromJust t) then
                      []
                    else
                      [(IP, Schedule{
                        s = r
                      , c = Just (Cycle.createCycle (fromJust cycleOfInterestPayment))
                      , t = t
                      , b = True
                      , dateToExclude = Nothing
                      })]
                scheduleV =
                  [(IP, Schedule{
                    s = s
                  , c = Just (Cycle.createCycle (fromJust cycleOfPrincipalRedemption))
                  , t = maturityDate
                  , b = True
                  , dateToExclude = Nothing
                  })]
            in
              scheduleU ++ scheduleV
          IPCI ->
            let s =
                  if isNothing cycleAnchorDateOfInterestPayment &&
                    isNothing cycleOfInterestPayment then
                      Nothing
                  else
                    if isNothing cycleAnchorDateOfInterestPayment then
                      Just (incrementDate initialExchangeDate
                        (Cycle.createCycle (fromJust cycleOfInterestPayment)))
                    else
                      cycleAnchorDateOfInterestPayment
            in
              if isNothing capitalizationEndDate then
                []
              else
                [(IPCI, Schedule{
                  s = s
                , c = Just (Cycle.createCycle (fromJust cycleOfInterestPayment))
                , t = capitalizationEndDate
                , b = True
                , dateToExclude = Nothing
                })]
          IPCB ->
            let s =
                  if isNothing cycleAnchorDateOfInterestCalculationBase &&
                    isNothing cycleOfInterestCalculationBase then
                      Nothing
                  else
                    if isNothing cycleAnchorDateOfInterestCalculationBase then
                      Just (incrementDate initialExchangeDate
                        (Cycle.createCycle (fromJust cycleOfInterestCalculationBase)))
                    else
                      cycleAnchorDateOfInterestCalculationBase
            in
              if interestCalculationBase /= ICB_NTL then
                []
              else
                [(IPCB, Schedule{
                  s = s
                , c = Just (Cycle.createCycle (fromJust cycleOfInterestCalculationBase))
                , t = maturityDate
                , b = True
                , dateToExclude = Nothing
                })]
          RR ->
            let s =
                  if isNothing cycleAnchorDateOfRateReset then
                    Just (incrementDate initialExchangeDate
                      (Cycle.createCycle (fromJust cycleOfRateReset)))
                  else
                    cycleAnchorDateOfRateReset
                rateResetCycleDates =
                  generateScheduleDates
                    Schedule{
                      s = s
                    , c = Just (Cycle.createCycle (fromJust cycleOfRateReset))
                    , t = maturityDate
                    , b = True
                    , dateToExclude = Nothing
                    }
            in
              if (isNothing cycleAnchorDateOfRateReset) && (isNothing cycleOfRateReset) then
                []
              else
                if isJust nextResetRate then
                  [(RR, Schedule{
                    s = s
                  , c = Just (Cycle.createCycle (fromJust cycleOfRateReset))
                  , t = maturityDate
                  , b = True
                  , dateToExclude = List.find (> (fromJust statusDate)) rateResetCycleDates
                  })]
                else
                  [(RR, Schedule{
                    s = s
                  , c = Just (Cycle.createCycle (fromJust cycleOfRateReset))
                  , t = maturityDate
                  , b = True
                  , dateToExclude = Nothing
                  })]
          RRF ->
            let s =
                  if isNothing cycleAnchorDateOfRateReset then
                    Just (incrementDate initialExchangeDate
                      (Cycle.createCycle (fromJust cycleOfRateReset)))
                  else
                    cycleAnchorDateOfRateReset
                rateResetCycleDates =
                  generateScheduleDates
                    Schedule{
                      s = s
                    , c = Just (Cycle.createCycle (fromJust cycleOfRateReset))
                    , t = maturityDate
                    , b = True
                    , dateToExclude = Nothing
                    }
            in
              if (isNothing cycleAnchorDateOfRateReset) && (isNothing cycleOfRateReset) then
                []
              else
                [(RRF, Schedule{
                  s = List.find (> (fromJust statusDate)) rateResetCycleDates
                , c = Nothing
                , t = Nothing
                , b = True
                , dateToExclude = Nothing
                })]
          SC ->
            let s =
                  if isNothing cycleAnchorDateOfScalingIndex &&
                    isNothing cycleOfScalingIndex then
                      Nothing
                  else
                    if isNothing cycleAnchorDateOfScalingIndex then
                      Just (incrementDate initialExchangeDate
                        (Cycle.createCycle (fromJust cycleOfScalingIndex)))
                    else
                      cycleAnchorDateOfScalingIndex
            in
              if scalingEffect == SE_000 then
                []
              else
                [(SC, Schedule{
                  s = s
                , c = Just (Cycle.createCycle (fromJust cycleOfScalingIndex))
                , t = maturityDate
                , b = True
                , dateToExclude = Nothing
                })]
          CE ->
             -- TODO
             []
  in
    generateSchedules' rest contractConfig (schedules ++ additionalSchedules)

-- TODO: maybe move to another module
generateEventDates config event =
  let schedules = generateSchedules [event] config
  in
    List.foldl
      (\eventDates' (_, schedule) ->
        let scheduleDates = generateScheduleDates schedule
        in
          List.foldl
            (\eventDates'' date ->
              List.insert date eventDates''
            ) eventDates' scheduleDates
      ) [] schedules

-- TODO: maybe move to another module
eventScheduleCycleDatesBound config boundType event predicate =
  let eventDates = generateEventDates config event
  in
    case boundType of
      INF ->
        fromJust (List.find predicate eventDates)
      SUP ->
        fromJust (List.find predicate (List.reverse eventDates))

calculateAnnuity config s t n a r =
  let scheduleTimes = List.filter (> s) (generateEventDates config PR)
      m = (List.length scheduleTimes) - 1
  in
    (n + a) *
    (
      (calculateAnnuityProductFragment config 0 m r scheduleTimes) /
      (1 + (calculateAnnuitySumFragment config 0 m r scheduleTimes))
    )

calculateAnnuityProductFragment config@ContractConfig{..} i m r scheduleTimes
  | i == m =
    1
  | otherwise =
    let yearFraction' =
          yearFraction dayCountConvention (scheduleTimes !! i)
            (scheduleTimes !! (i + 1)) (fromJust maturityDate)
    in
      (1 + r * yearFraction') * (calculateAnnuityProductFragment config (i + 1) m r scheduleTimes)

calculateAnnuitySumFragment config i m r scheduleTimes
  | i == m =
    0
  | otherwise =
    (calculateAnnuityProductFragment config i m r scheduleTimes) +
      (calculateAnnuitySumFragment config (i + 1) m r scheduleTimes)
