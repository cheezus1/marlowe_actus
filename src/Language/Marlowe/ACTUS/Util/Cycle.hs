{-# LANGUAGE RecordWildCards #-}
module Language.Marlowe.ACTUS.Util.Cycle where

import Data.Char
import Data.Time.Calendar
import qualified Data.List as List

import Language.Marlowe.ACTUS.Definitions

mapPeriod :: Char -> Period
mapPeriod periodChar = case periodChar of
  'D' -> P_D
  'W' -> P_W
  'M' -> P_M
  'Q' -> P_Q
  'H' -> P_H
  'Y' -> P_Y

mapStub :: Char -> Stub
mapStub stubChar = case stubChar of
  '-' -> ShortStub
  '+' -> LongStub

createCycle :: [Char] -> Cycle
createCycle [n, periodChar, stubChar] = Cycle
  { n = toInteger (digitToInt n)
  , p = mapPeriod periodChar
  , stub = mapStub stubChar
  }

generateCycleDates cycle anchorDate maturityDate includeLastDate =
  generateCycleDates' cycle maturityDate anchorDate includeLastDate []

generateCycleDates' cycle@Cycle{..} maturityDate currentDate includeLastDate cycleDates
  | currentDate >= maturityDate =
    if includeLastDate then
      if currentDate == maturityDate || stub == ShortStub then
        List.insert maturityDate cycleDates
      else
        cycleDates
    else
      cycleDates
  | otherwise =
    let cycleDates' = (List.insert currentDate cycleDates)
        nextDate = incrementDate currentDate cycle
    in
      generateCycleDates' cycle maturityDate nextDate includeLastDate cycleDates'

incrementDate date Cycle{..} =
  case p of
    P_D -> addDays n date
    P_W -> addDays (n * 7) date
    P_M -> addGregorianMonthsClip n date
    P_Q -> addGregorianMonthsClip (n * 3) date
    P_H -> addGregorianMonthsClip (n * 6) date
    P_Y -> addGregorianYearsClip n date

decrementDate date Cycle{..} =
  case p of
    P_D -> addDays (-n) date
    P_W -> addDays (-(n * 7)) date
    P_M -> addGregorianMonthsClip (-n) date
    P_Q -> addGregorianMonthsClip (-(n * 3)) date
    P_H -> addGregorianMonthsClip (-(n * 6)) date
    P_Y -> addGregorianYearsClip (-n) date

incrementDate' date _ 0 =
  date

incrementDate' date cycle times =
  incrementDate' (incrementDate date cycle) cycle (times - 1)
