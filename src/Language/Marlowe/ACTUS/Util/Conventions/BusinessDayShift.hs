module Language.Marlowe.ACTUS.Util.Conventions.BusinessDayShift where

import Data.Time
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate

data BDC =  BDC_NULL
          | BDC_SCF
          | BDC_SCMF
          | BDC_CSF
          | BDC_CSMF
          | BDC_SCP
          | BDC_SCMP
          | BDC_CSP
          | BDC_CSMP deriving (Show)

data CalendarType = NoCalendar | MondayToFriday deriving (Show)

shiftDate :: Day -> BDC -> CalendarType -> Day
shiftDate date BDC_NULL _ =
  date

shiftDate date BDC_SCF calendarType =
  getFollowingBusinessDay date calendarType

shiftDate date BDC_SCMF calendarType =
  shiftModifiedFollowing date calendarType

shiftDate date BDC_CSF calendarType =
  getFollowingBusinessDay date calendarType

shiftDate date BDC_CSMF calendarType =
  shiftModifiedFollowing date calendarType

shiftDate date BDC_SCP calendarType =
  getPreceedingBusinessDay date calendarType

shiftDate date BDC_SCMP calendarType =
  shiftModifiedPreceeding date calendarType

shiftDate date BDC_CSP calendarType =
  getPreceedingBusinessDay date calendarType

shiftDate date BDC_CSMP calendarType =
  shiftModifiedPreceeding date calendarType

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
    NoCalendar ->
      date
