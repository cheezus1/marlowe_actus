{-# LANGUAGE RecordWildCards #-}
module Language.Marlowe.ACTUS.Util.NegativeAmortizer where

import Data.Maybe
import Data.Time

import Language.Marlowe.ACTUS.Definitions
import Language.Marlowe.ACTUS.Util.Cycle
import Language.Marlowe.ACTUS.Util.Schedule
import Language.Marlowe.ACTUS.Util.Conventions.YearFraction

calculateTMDt0 :: ContractConfig -> Day
calculateTMDt0 config@ContractConfig{..} =
  let t0 = initialExchangeDate
  in
    case maturityDate of
      Just md ->
        md
      Nothing ->
        let prcl = createCycle (fromJust cycleOfPrincipalRedemption)
            tPrev =
              if isJust cycleAnchorDateOfPrincipalRedemption &&
                (fromJust cycleAnchorDateOfPrincipalRedemption) >= t0 then
                  fromJust cycleAnchorDateOfPrincipalRedemption
              else
                if incrementDate initialExchangeDate prcl >= t0 then
                   incrementDate initialExchangeDate prcl
                else
                  eventScheduleCycleDatesBound config SUP PR (< t0)
            n = ceiling (notionalPrincipal /
                  ((fromJust nextPrincipalRedemptionPayment) - notionalPrincipal *
                    (yearFraction dayCountConvention tPrev
                      (incrementDate tPrev prcl) (fromJust maturityDate)
                    ) * nominalInterestRate
                  )
                )
        in
          incrementDate' tPrev prcl n
