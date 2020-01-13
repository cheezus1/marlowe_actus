{-# LANGUAGE RecordWildCards #-}
module Language.Marlowe.ACTUS.Util.Cycle where

import Data.Char
import Data.Time.Calendar
import qualified Data.List as List

data Period = P_D -- Day
            | P_W -- Week
            | P_M -- Month
            | P_Q -- Quarter
            | P_H -- Half Year
            | P_Y -- Year
            deriving (Show, Eq, Ord)

data Stub = ShortStub | LongStub deriving (Show, Eq, Ord)

data Cycle = Cycle
  { n :: Integer
  , p :: Period
  , s :: Stub
  } deriving (Show, Eq, Ord)

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
  , s = mapStub stubChar
  }

generateCycleDates cycle anchorDate maturityDate =
  generateCycleDates' cycle maturityDate anchorDate [anchorDate]

generateCycleDates' cycle@Cycle{s = stub, ..} maturityDate previousDate cycleDates =
  let nextDate = incrementDate previousDate cycle
  in
    if nextDate > maturityDate then
      if previousDate == maturityDate || stub == ShortStub then cycleDates
      else List.delete previousDate cycleDates
    else
      generateCycleDates' cycle maturityDate nextDate (List.insert nextDate cycleDates)


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
