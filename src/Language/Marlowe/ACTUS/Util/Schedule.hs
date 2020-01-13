{-# LANGUAGE RecordWildCards #-}
module Language.Marlowe.ACTUS.Util.Schedule where

import Data.Time
import Data.Maybe
import qualified Data.Map.Strict as Map
import qualified Data.List as List

import Language.Marlowe.ACTUS.Util.Cycle
import Language.Marlowe.ACTUS.Util.Conventions.EndOfMonthShift
import Language.Marlowe.ACTUS.Util.Conventions.BusinessDayShift

data Schedule = Schedule
  { s :: Maybe Day -- kANX with kANX attribute cycle anchor date of event type k
  , c :: Maybe Cycle -- kCL with kCL event type k’s schedule cycle
  , t :: Maybe Day -- MD with MD the contract’s maturity
  , dateToExclude :: Maybe Day -- additional field
  } deriving (Show)

scheduleEvents eventSchedules =
  scheduleEvents' eventSchedules Map.empty

scheduleEvents' [] scheduledEvents =
  scheduledEvents

scheduleEvents' ((event, schedule@Schedule{..}) : rest) scheduledEvents =
  let scheduleDates = generateScheduleDates schedule
      updatedScheduledEvents =
        mergeScheduleEvents event scheduleDates scheduledEvents
  in
    scheduleEvents' rest updatedScheduledEvents

generateScheduleDates
  schedule@Schedule{
    s = s
  , c = c
  , t = t
  , dateToExclude = dateToExclude
  } =
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
                generateCycleDates (fromJust c) (fromJust s) (fromJust t)
    in
      if isJust dateToExclude then
        List.delete (fromJust dateToExclude) scheduleCycleDates
      else
        scheduleCycleDates

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
