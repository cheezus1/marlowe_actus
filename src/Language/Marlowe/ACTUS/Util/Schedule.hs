{-# LANGUAGE RecordWildCards #-}
module Language.Marlowe.ACTUS.Util.Schedule where

import Data.Time
import Data.Maybe
import qualified Data.Map.Strict as Map
import qualified Data.List as List

import Language.Marlowe.ACTUS.Definitions
import Language.Marlowe.ACTUS.Util.Cycle

scheduleEvents eventSchedules endOfMonthConvention =
  scheduleEvents' eventSchedules endOfMonthConvention Map.empty

scheduleEvents' [] _ scheduledEvents =
  scheduledEvents

scheduleEvents' ((event, schedule@Schedule{..}) : rest) endOfMonthConvention scheduledEvents =
  let scheduleDates = generateScheduleDates schedule endOfMonthConvention
      updatedScheduledEvents =
        mergeScheduleEvents event scheduleDates scheduledEvents
  in
    scheduleEvents' rest endOfMonthConvention updatedScheduledEvents

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
