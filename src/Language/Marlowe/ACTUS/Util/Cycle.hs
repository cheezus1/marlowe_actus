{-# LANGUAGE RecordWildCards #-}
module Language.Marlowe.ACTUS.Util.Cycle where

import Data.Char
import Data.Time.Calendar
import qualified Data.List as List

import Language.Marlowe.ACTUS.Definitions
import Language.Marlowe.ACTUS.Util.Conventions.DateShift

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

generateCycleDates cycle anchorDate maturityDate
  includeLastDate endOfMonthConvention =
    generateCycleDates' cycle anchorDate maturityDate
      anchorDate includeLastDate endOfMonthConvention []

generateCycleDates' cycle@Cycle{..} anchorDate maturityDate
  currentDate includeLastDate endOfMonthConvention cycleDates
    | currentDate >= maturityDate =
      if includeLastDate then
        if currentDate == maturityDate || stub == ShortStub then
          let eomcShiftedMD =
                applyEOMC maturityDate anchorDate cycle endOfMonthConvention
          in
            List.insert eomcShiftedMD cycleDates
        else
          cycleDates
      else
        cycleDates
    | otherwise =
      let eomcShiftedCD =
            applyEOMC currentDate anchorDate cycle endOfMonthConvention
          cycleDates' = (List.insert eomcShiftedCD cycleDates)
          nextDate = incrementDate currentDate cycle
      in
        generateCycleDates' cycle anchorDate maturityDate
          nextDate includeLastDate endOfMonthConvention cycleDates'

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
