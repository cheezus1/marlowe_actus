{-# LANGUAGE RecordWildCards #-}
module Language.Marlowe.ACTUS.Util.Schedule where

import Data.Time
import Data.Maybe
import qualified Data.Map.Strict as Map
import qualified Data.List as List

import Language.Marlowe.ACTUS.Definitions
import Language.Marlowe.ACTUS.Util.Cycle
import Language.Marlowe.ACTUS.Util.Conventions.DateShift

scheduleEvents :: [(Event, Schedule)] -> EOMC -> Map.Map Day [Event]
scheduleEvents eventSchedules endOfMonthConvention =
  scheduleEvents' eventSchedules endOfMonthConvention Map.empty

scheduleEvents' :: [(Event, Schedule)] -> EOMC -> Map.Map Day [Event] -> Map.Map Day [Event]
scheduleEvents' [] _ scheduledEvents =
  scheduledEvents

scheduleEvents' ((event, schedule@Schedule{..}) : rest) endOfMonthConvention scheduledEvents =
  let scheduleDates = generateScheduleDates schedule endOfMonthConvention
      updatedScheduledEvents =
        mergeScheduleEvents event scheduleDates scheduledEvents
  in
    scheduleEvents' rest endOfMonthConvention updatedScheduledEvents

generateScheduleDates :: Schedule -> EOMC -> [Day]
generateScheduleDates
  schedule@Schedule{
    s = s
  , c = c
  , t = t
  , b = b
  , dateToExclude = dateToExclude
  }
  endOfMonthConvention =
    let scheduleCycleDates =
          if (isNothing s) && (isNothing t) then
            []
          else
            if isNothing t then
              [(fromJust s)]
            else
              if isNothing c then
                [(fromJust s), (fromJust t)]
              else
                generateCycleDates (fromJust c)
                  (fromJust s) (fromJust t) b endOfMonthConvention
    in
      if isJust dateToExclude then
        List.delete (fromJust dateToExclude) scheduleCycleDates
      else
        scheduleCycleDates

mergeScheduleEvents :: Event -> [Day] -> Map.Map Day [Event] -> Map.Map Day [Event]
mergeScheduleEvents _ [] scheduledEvents =
  scheduledEvents

mergeScheduleEvents event (date : rest) scheduledEvents =
  let updatedEventsForDate =
        case Map.lookup date scheduledEvents of
          Just events ->
            List.insert event events
          Nothing ->
            [event]
      updatedScheduledEvents =
        Map.insert date updatedEventsForDate scheduledEvents
  in
    mergeScheduleEvents event rest updatedScheduledEvents

eventScheduleCycleDatesBound :: ContractConfig -> BoundTypes -> Event -> (Day -> Bool) -> Day
eventScheduleCycleDatesBound
  config@ContractConfig{
    businessDayConvention = businessDayConvention
  , calendar = calendar
  }
  boundType event predicate =
    let eventDates = generateEventDates config event
    in
      case boundType of
        INF ->
          fromJust (List.find predicate eventDates)
        SUP ->
          fromJust (List.find predicate (List.reverse eventDates))

generateEventDates :: ContractConfig -> Event -> [Day]
generateEventDates
  config@ContractConfig{
    contractType = contractType
  , endOfMonthConvention = endOfMonthConvention
  , businessDayConvention = businessDayConvention
  , calendar = calendar
  }
  event =
    let schedules = getSchedule config event
    in
      -- there may be more than one schedule for an event
      List.foldl
        (\eventDates' (_, schedule) ->
          let scheduleDates = generateScheduleDates schedule endOfMonthConvention
          in
            List.foldl
              (\eventDates'' date ->
                List.insert (maybeApplyBDC date businessDayConvention calendar) eventDates''
              ) eventDates' scheduleDates
        ) [] schedules

getSchedule :: ContractConfig -> Event -> [(Event, Schedule)]
getSchedule contractConfig@ContractConfig{..} AD
  | List.elem contractType [PAM, LAM, ANN] =
    [] -- TODO: find out where custom input is provided

getSchedule contractConfig@ContractConfig{..} IED
  | List.elem contractType [PAM, LAM, ANN] =
    [(IED, Schedule{
      s = Just initialExchangeDate
    , c = Nothing
    , t = Nothing
    , b = True
    , dateToExclude = Nothing
    })]

getSchedule contractConfig@ContractConfig{..} PP
  | List.elem contractType [PAM, LAM, ANN] =
    let s =
          if isNothing cycleAnchorDateOfOptionality &&
            isNothing cycleOfOptionality then
              Nothing
          else
            if isNothing cycleAnchorDateOfOptionality then
              Just (incrementDate initialExchangeDate
                (createCycle (fromJust cycleOfOptionality)))
            else
              cycleAnchorDateOfOptionality
        scheduleU =
          [(PP, Schedule{
            s = s
          , c = Just (createCycle (fromJust cycleOfOptionality))
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


getSchedule contractConfig@ContractConfig{..} PY
  | List.elem contractType [PAM, LAM, ANN] =
    if penaltyType == PT_O then
      []
    else
      getSchedule contractConfig PP

getSchedule contractConfig@ContractConfig{..} FP
  | List.elem contractType [PAM, LAM, ANN] =
    let s =
          if isNothing cycleAnchorDateOfFee && isNothing cycleOfFee then
            Nothing
          else
            if isNothing cycleAnchorDateOfFee then
              Just (incrementDate initialExchangeDate
                (createCycle (fromJust cycleOfFee)))
            else
              cycleAnchorDateOfFee
    in
      if isNothing feeRate || (fromJust feeRate) == 0.0 then
        []
      else
        [(FP, Schedule{
          s = s
        , c = Just (createCycle (fromJust cycleOfFee))
        , t = maturityDate
        , b = True
        , dateToExclude = Nothing
        })]

getSchedule contractConfig@ContractConfig{..} PRD
  | List.elem contractType [PAM, LAM, ANN] =
    [(PRD, Schedule{
      s = purchaseDate
    , c = Nothing
    , t = Nothing
    , b = True
    , dateToExclude = Nothing
    })]

getSchedule contractConfig@ContractConfig{..} TD
  | List.elem contractType [PAM, LAM, ANN] =
    [(TD, Schedule{
      s = terminationDate
    , c = Nothing
    , t = Nothing
    , b = True
    , dateToExclude = Nothing
    })]

getSchedule contractConfig@ContractConfig{..} IPCI
  | List.elem contractType [PAM, LAM, ANN] =
    let s =
          if isNothing cycleAnchorDateOfInterestPayment &&
            isNothing cycleOfInterestPayment then
              Nothing
          else
            if isNothing cycleAnchorDateOfInterestPayment then
              Just (incrementDate initialExchangeDate
                (createCycle (fromJust cycleOfInterestPayment)))
            else
              cycleAnchorDateOfInterestPayment
    in
      if isNothing capitalizationEndDate then
        []
      else
        [(IPCI, Schedule{
          s = s
        , c = Just (createCycle (fromJust cycleOfInterestPayment))
        , t = capitalizationEndDate
        , b = True
        , dateToExclude = Nothing
        })]

getSchedule contractConfig@ContractConfig{..} RR
  | List.elem contractType [PAM, LAM, ANN] =
    let s =
          if isNothing cycleAnchorDateOfRateReset then
            Just (incrementDate initialExchangeDate
              (createCycle (fromJust cycleOfRateReset)))
          else
            cycleAnchorDateOfRateReset
        rateResetCycleDates =
          generateScheduleDates
            Schedule{
              s = s
            , c = Just (createCycle (fromJust cycleOfRateReset))
            , t = maturityDate
            , b = True
            , dateToExclude = Nothing
            }
            endOfMonthConvention
    in
      if (isNothing cycleAnchorDateOfRateReset) && (isNothing cycleOfRateReset) then
        []
      else
        if isJust nextResetRate then
          [(RR, Schedule{
            s = s
          , c = Just (createCycle (fromJust cycleOfRateReset))
          , t = maturityDate
          , b = True
          , dateToExclude = List.find (> (fromJust statusDate)) rateResetCycleDates
          })]
        else
          [(RR, Schedule{
            s = s
          , c = Just (createCycle (fromJust cycleOfRateReset))
          , t = maturityDate
          , b = True
          , dateToExclude = Nothing
          })]

getSchedule contractConfig@ContractConfig{..} RRF
  | List.elem contractType [PAM, LAM, ANN] =
    let s =
          if isNothing cycleAnchorDateOfRateReset then
            Just (incrementDate initialExchangeDate
              (createCycle (fromJust cycleOfRateReset)))
          else
            cycleAnchorDateOfRateReset
        rateResetCycleDates =
          generateScheduleDates
            Schedule{
              s = s
            , c = Just (createCycle (fromJust cycleOfRateReset))
            , t = maturityDate
            , b = True
            , dateToExclude = Nothing
            }
            endOfMonthConvention
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

getSchedule contractConfig@ContractConfig{..} SC
  | List.elem contractType [PAM, LAM, ANN] =
    let s =
          if isNothing cycleAnchorDateOfScalingIndex &&
            isNothing cycleOfScalingIndex then
              Nothing
          else
            if isNothing cycleAnchorDateOfScalingIndex then
              Just (incrementDate initialExchangeDate
                (createCycle (fromJust cycleOfScalingIndex)))
            else
              cycleAnchorDateOfScalingIndex
    in
      if scalingEffect == SE_000 then
        []
      else
        [(SC, Schedule{
          s = s
        , c = Just (createCycle (fromJust cycleOfScalingIndex))
        , t = maturityDate
        , b = True
        , dateToExclude = Nothing
        })]

getSchedule contractConfig@ContractConfig{..} CE
  | List.elem contractType [PAM, LAM, ANN] =
    -- TODO
    []

getSchedule contractConfig@ContractConfig{..} PR
  | List.elem contractType [LAM, ANN] =
    let s =
          if isNothing cycleAnchorDateOfPrincipalRedemption &&
            isNothing cycleOfPrincipalRedemption then
              Nothing
          else
            if isNothing cycleAnchorDateOfPrincipalRedemption then
              Just (incrementDate initialExchangeDate
                (createCycle (fromJust cycleOfPrincipalRedemption)))
            else
              cycleAnchorDateOfPrincipalRedemption
    in
      [(PR, Schedule{
        s = s
      , c = Just (createCycle (fromJust cycleOfPrincipalRedemption))
      , t = maturityDate
      , b = False
      , dateToExclude = Nothing
      })]

getSchedule contractConfig@ContractConfig{..} IPCB
  | List.elem contractType [LAM, ANN] =
    let s =
          if isNothing cycleAnchorDateOfInterestCalculationBase &&
            isNothing cycleOfInterestCalculationBase then
              Nothing
          else
            if isNothing cycleAnchorDateOfInterestCalculationBase then
              Just (incrementDate initialExchangeDate
                (createCycle (fromJust cycleOfInterestCalculationBase)))
            else
              cycleAnchorDateOfInterestCalculationBase
    in
      if interestCalculationBase /= ICB_NTL then
        []
      else
        [(IPCB, Schedule{
          s = s
        , c = Just (createCycle (fromJust cycleOfInterestCalculationBase))
        , t = maturityDate
        , b = True
        , dateToExclude = Nothing
        })]

getSchedule contractConfig@ContractConfig{..} IP
  | List.elem contractType [PAM, LAM] =
    let s =
          if isNothing cycleAnchorDateOfInterestPayment &&
            isNothing cycleOfInterestPayment then
              Nothing
          else
            if isJust capitalizationEndDate then
              capitalizationEndDate
            else
              if isNothing cycleAnchorDateOfInterestPayment then
                Just (incrementDate initialExchangeDate
                  (createCycle (fromJust cycleOfInterestPayment)))
              else
                cycleAnchorDateOfInterestPayment
    in
      if nominalInterestRate == 0.0 then
        []
      else
        [(IP, Schedule{
          s = s
        , c = Just (createCycle (fromJust cycleOfInterestPayment))
        , t = maturityDate
        , b = True
        , dateToExclude = Nothing
        })]

getSchedule contractConfig@ContractConfig{..} IP
  | List.elem contractType [NAM, ANN] =
    let r =
          if isJust capitalizationEndDate then capitalizationEndDate
          else
            if isJust cycleAnchorDateOfInterestPayment then
              cycleAnchorDateOfInterestPayment
            else
              if isJust cycleOfInterestPayment then
                Just (incrementDate initialExchangeDate
                  (createCycle (fromJust cycleOfInterestPayment)))
              else
                Nothing
        s =
          if isNothing cycleAnchorDateOfPrincipalRedemption then
            Just (incrementDate initialExchangeDate
              (createCycle (fromJust cycleOfPrincipalRedemption)))
          else
            cycleAnchorDateOfPrincipalRedemption
        t = Just (decrementDate (fromJust s) (createCycle (fromJust cycleOfPrincipalRedemption)))
        scheduleU =
          if (isNothing cycleAnchorDateOfInterestPayment) && (isNothing cycleOfInterestPayment) then
            []
          else
            if (isJust capitalizationEndDate) && (fromJust capitalizationEndDate) >= (fromJust t) then
              []
            else
              [(IP, Schedule{
                s = r
              , c = Just (createCycle (fromJust cycleOfInterestPayment))
              , t = t
              , b = True
              , dateToExclude = Nothing
              })]
        scheduleV =
          [(IP, Schedule{
            s = s
          , c = Just (createCycle (fromJust cycleOfPrincipalRedemption))
          , t = maturityDate
          , b = True
          , dateToExclude = Nothing
          })]
    in
      scheduleU ++ scheduleV

-- getSchedule contractConfig@ContractConfig{contractType = contractType} event =
--   error "No event called " <> (show event) <>
--     " defined for contract " <> (show contractType)

getSchedule' :: ContractConfig -> Event -> Day -> [(Event, Schedule)]
getSchedule' contractConfig@ContractConfig{..} MD tmdt0
  | List.elem contractType [PAM, LAM, ANN] =
    [(MD, Schedule{
      s = Just tmdt0
    , c = Nothing
    , t = Nothing
    , b = True
    , dateToExclude = Nothing
    })]

-- getSchedule' contractConfig@ContractConfig{contractType = contractType} event _ =
--   error "No event called " <> (show event) <>
--     " defined for contract " <> (show contractType)
