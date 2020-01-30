{-# LANGUAGE RecordWildCards #-}
module Language.Marlowe.ACTUS.Util.Annuity where

import Data.Maybe
import Data.Time
import qualified Data.List as List

import Language.Marlowe.ACTUS.Definitions
import Language.Marlowe.ACTUS.Util.NegativeAmortizer as NegativeAmortizer
import Language.Marlowe.ACTUS.Util.Schedule
import Language.Marlowe.ACTUS.Util.Conventions.DateShift
import Language.Marlowe.ACTUS.Util.Conventions.YearFraction

generateSchedules :: [Event] -> ContractConfig -> [(Event, Schedule)]
generateSchedules events config =
  generateSchedules' events config []

generateSchedules' ::
  [Event] -> ContractConfig -> [(Event, Schedule)] -> [(Event, Schedule)]
generateSchedules' [] _ schedules =
  schedules

generateSchedules' (event : rest) contractConfig@ContractConfig{..} schedules =
  let additionalSchedules =
        case event of
          MD ->
            getSchedule' contractConfig event (NegativeAmortizer.calculateTMDt0 contractConfig)
          _ ->
            getSchedule contractConfig event
  in
    generateSchedules' rest contractConfig (schedules ++ additionalSchedules)

calculateAnnuity ::
  ContractConfig -> Day -> Day -> Double -> Double -> Double -> Double
calculateAnnuity config s t n a r =
  let scheduleTimes = List.filter (> s) (generateEventDates config PR)
      m = (List.length scheduleTimes) - 1
  in
    (n + a) *
    (
      (calculateAnnuityProductFragment config 0 m r scheduleTimes) /
      (1 + (calculateAnnuitySumFragment config 0 m r scheduleTimes))
    )

calculateAnnuityProductFragment ::
  ContractConfig -> Int -> Int -> Double -> [Day] -> Double
calculateAnnuityProductFragment config@ContractConfig{..} i m r scheduleTimes
  | i == m =
    1
  | otherwise =
    let yearFraction' =
          yearFraction dayCountConvention (scheduleTimes !! i)
            (scheduleTimes !! (i + 1)) (fromJust maturityDate)
    in
      (1 + r * yearFraction') * (calculateAnnuityProductFragment config (i + 1) m r scheduleTimes)

calculateAnnuitySumFragment ::
  ContractConfig -> Int -> Int -> Double -> [Day] -> Double
calculateAnnuitySumFragment config i m r scheduleTimes
  | i == m =
    0
  | otherwise =
    (calculateAnnuityProductFragment config i m r scheduleTimes) +
      (calculateAnnuitySumFragment config (i + 1) m r scheduleTimes)
