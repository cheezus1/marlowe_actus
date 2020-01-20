module Main where

import Data.Map.Strict as Map
import Data.Time.Calendar

import Language.Marlowe.ACTUS.Definitions
import Language.Marlowe.ACTUS.Annuity as Annuity
import Language.Marlowe.ACTUS.Util.Annuity
import Language.Marlowe.ACTUS.Util.Event
import Language.Marlowe.ACTUS.Util.Schedule
import Language.Marlowe.ACTUS.Util.Conventions.DayCount
import Language.Marlowe.ACTUS.Util.Conventions.BusinessDayShift
import Language.Marlowe.ACTUS.Util.Conventions.EndOfMonthShift
import Language.Marlowe.ACTUS.Util.Conventions.ContractRoleSign
import Language.Marlowe.ACTUS.Util.Conventions.YearFraction


main :: IO ()
main =
  -- let config =
  --       ContractConfig{
  --       -- General
  --         nominalInterestRate = 0.05
  --       , dayCountConvention = E30_360
  --       , initialExchangeDate = fromGregorian 2015 1 2
  --       , notionalPrincipal = 1000
  --
  --       -- Calendar
  --       , calendar = MondayToFriday -- NoCalendar
  --       , businessDayConvention = BDC_SCF -- BDC_SCF
  --       , endOfMonthConvention = EOM_SD -- EOM_SD
  --
  --       -- Contract Identification
  --       , statusDate = Just (fromGregorian 2015 1 1)
  --       , contractRole = Just CR_RPL
  --
  --       -- Fees
  --       , cycleAnchorDateOfFee = Nothing
  --       , cycleOfFee = Nothing
  --       , feeBasis = Just FB_N
  --       , feeRate = Just 0.0
  --       , feeAccrued = Just 0.0
  --
  --       -- Interest
  --       , cycleAnchorDateOfInterestPayment = Just (fromGregorian 2016 1 2)
  --       , cycleOfInterestPayment = Just "1Y+"
  --       , accruedInterest = Just 0
  --       , capitalizationEndDate = Nothing
  --       , interestCalculationBase = ICB_NT -- ICB_NT
  --       , interestCalculationBaseAmount = Just 1
  --
  --       -- Notional Principal
  --       , maturityDate = Just (fromGregorian 2025 1 2)
  --       , contractDealDate = Just (fromGregorian 2015 1 1)
  --       , premiumDiscountAtIED = 0 -- 0
  --       , cycleAnchorDateOfPrincipalRedemption = Just (fromGregorian 2017 1 2)
  --       , cycleOfPrincipalRedemption = Just "1Y+"
  --       , nextPrincipalRedemptionPayment = Just 0.0
  --       , priceAtPurchaseDate = Just 0.0
  --       , priceAtTerminationDate = Just 0.0
  --       , marketObjectCodeOfScalingIndex = Just ""
  --       , scalingIndexAtStatusDate = Just 0.0
  --       , scalingEffect = Just SE_000 -- SE_000
  --
  --       -- Optionality
  --       , penaltyType = PT_O -- PT_O
  --       , penaltyRate = 0.0 -- 0.0
  --
  --       -- Rate Reset
  --       , cycleAnchorDateOfRateReset = Just (fromGregorian 2016 1 1)
  --       , cycleOfRateReset = Just "2Y+"
  --       , rateSpread = 0.05 -- 0.0
  --       , periodFloor = Just 0.0
  --       , periodCap = Just 0.0
  --       , lifeFloor = Just 0.0
  --       , lifeCap = Just 0.0
  --       , nextResetRate = Just 0.0
  --       }
  let config =
        ContractConfig{
        -- General
          nominalInterestRate = 0.05
        , dayCountConvention = E30_360
        , initialExchangeDate = fromGregorian 2015 1 2
        , notionalPrincipal = 1000

        -- Calendar
        , calendar = NoCalendar -- NoCalendar
        , businessDayConvention = BDC_SCF -- BDC_SCF
        , endOfMonthConvention = EOM_SD -- EOM_SD

        -- Contract Identification
        , statusDate = Just (fromGregorian 2015 1 1)
        , contractRole = Just CR_RPL

        -- Fees
        , cycleAnchorDateOfFee = Nothing
        , cycleOfFee = Nothing
        , feeBasis = Just FB_N
        , feeRate = Just 0.0
        , feeAccrued = Just 0.0

        -- Interest
        , cycleAnchorDateOfInterestPayment = Nothing
        , cycleOfInterestPayment = Nothing
        , accruedInterest = Nothing
        , capitalizationEndDate = Nothing
        , cycleAnchorDateOfInterestCalculationBase = Nothing
        , cycleOfInterestCalculationBase = Nothing
        , interestCalculationBase = ICB_NT -- ICB_NT
        , interestCalculationBaseAmount = Nothing

        -- Notional Principal
        , maturityDate = Just (fromGregorian 2025 1 2)
        , contractDealDate = Just (fromGregorian 2015 1 1)
        , premiumDiscountAtIED = 0 -- 0
        , cycleAnchorDateOfPrincipalRedemption = Just (fromGregorian 2018 1 2)
        , cycleOfPrincipalRedemption = Just "1Y+"
        , nextPrincipalRedemptionPayment = Just 0.0
        , purchaseDate = Nothing
        , priceAtPurchaseDate = Nothing
        , terminationDate = Nothing
        , priceAtTerminationDate = Nothing
        , marketObjectCodeOfScalingIndex = Nothing
        , scalingIndexAtStatusDate = Just 100.0
        , cycleAnchorDateOfScalingIndex = Nothing
        , cycleOfScalingIndex = Just "1Y+"
        , scalingEffect = SE_IN0 -- SE_000

        -- Optionality
        , cycleAnchorDateOfOptionality = Nothing
        , cycleOfOptionality = Nothing
        , penaltyType = PT_O -- PT_O
        , penaltyRate = 0.0 -- 0.0
        , prepaymentEffect = PE_N -- PE_N

        -- Rate Reset
        , cycleAnchorDateOfRateReset = Nothing
        , cycleOfRateReset = Nothing
        , rateSpread = 0.00 -- 0.0
        , periodFloor = Nothing
        , periodCap = Nothing
        , lifeFloor = Nothing
        , lifeCap = Nothing
        , nextResetRate = Nothing
        , rateMultiplier = 1.0 -- 1.0
        }
      initState = Annuity.stateInit config
      generatedSchedules = generateSchedules Annuity.events config
      scheduledEvents = scheduleEvents generatedSchedules
  in
    (print (yearFraction E30_360 (fromGregorian 2006 1 31) (fromGregorian 2006 2 28) (fromGregorian 2008 2 29)))
