module Language.Marlowe.ACTUS.Util.Conventions.DateShift where

import Debug.Trace

import Data.List as List
import Data.Time
import Data.Time.Calendar.WeekDate

import Language.Marlowe.ACTUS.Definitions

applyEOMC :: Day -> Day -> Cycle -> EOMC -> Day
applyEOMC date s cycle@Cycle{n = n, p = p} endOfMonthConvention
  | ((isLastDayOfMonthWithLessThan31Days s) &&
      p /= P_D && p /= P_W &&
      endOfMonthConvention == EOMC_EOM
    ) =
    moveToEndOfMonth date
  | otherwise =
    date

maybeApplyBDC :: Day -> BDC -> CalendarType -> Day
maybeApplyBDC date businessDayConvention calendarType
  | List.elem businessDayConvention [BDC_CSF, BDC_CSMF, BDC_CSP, BDC_CSMP] =
    applyBDC date businessDayConvention calendarType
  | otherwise =
    date

applyBDC :: Day -> BDC -> CalendarType -> Day
applyBDC date BDC_NULL _ =
  date

applyBDC date BDC_SCF calendarType =
  getFollowingBusinessDay date calendarType

applyBDC date BDC_SCMF calendarType =
  shiftModifiedFollowing date calendarType

applyBDC date BDC_CSF calendarType =
  getFollowingBusinessDay date calendarType

applyBDC date BDC_CSMF calendarType =
  shiftModifiedFollowing date calendarType

applyBDC date BDC_SCP calendarType =
  getPreceedingBusinessDay date calendarType

applyBDC date BDC_SCMP calendarType =
  shiftModifiedPreceeding date calendarType

applyBDC date BDC_CSP calendarType =
  getPreceedingBusinessDay date calendarType

applyBDC date BDC_CSMP calendarType =
  shiftModifiedPreceeding date calendarType

isLastDayOfMonthWithLessThan31Days :: Day -> Bool
isLastDayOfMonthWithLessThan31Days date =
  let (day, month, year) = toGregorian date
  in
    day < 31 && (gregorianMonthLength (toInteger year) month) == (fromInteger day) 

moveToEndOfMonth :: Day -> Day
moveToEndOfMonth date =
  let (day, month, year) = toGregorian date
      monthLength = gregorianMonthLength (toInteger year) month
  in
    fromGregorian (toInteger year) month monthLength

shiftModifiedFollowing :: Day -> CalendarType -> Day
shiftModifiedFollowing date calendarType =
  let (_, month, _) = toGregorian date
      shiftedFollowing = getFollowingBusinessDay date calendarType
      (_, shiftedMonth, _) = toGregorian shiftedFollowing
  in
    if month == shiftedMonth then shiftedFollowing
    else getPreceedingBusinessDay date calendarType

shiftModifiedPreceeding :: Day -> CalendarType -> Day
shiftModifiedPreceeding date calendarType =
  let (_, month, _) = toGregorian date
      shiftedPreceeding = getPreceedingBusinessDay date calendarType
      (_, shiftedMonth, _) = toGregorian shiftedPreceeding
  in
    if month == shiftedMonth then shiftedPreceeding
    else getFollowingBusinessDay date calendarType

getFollowingBusinessDay :: Day -> CalendarType -> Day
getFollowingBusinessDay date calendarType =
  case calendarType of
    MondayToFriday ->
      case toWeekDate date of
        (_, _, 6) ->
          addDays 2 date
        (_, _, 7) ->
          addDays 1 date
        _ ->
          date
    NoCalendar ->
      date

getPreceedingBusinessDay :: Day -> CalendarType -> Day
getPreceedingBusinessDay date calendarType =
  case calendarType of
    MondayToFriday ->
      case toWeekDate date of
        (_, _, 6) ->
          addDays (-1) date
        (_, _, 7) ->
          addDays (-2) date
        _ ->
          date
    NoCalendar ->
      date
