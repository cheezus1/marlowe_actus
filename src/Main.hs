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
        , cycleAnchorDateOfInterestPayment = Just (fromGregorian 2016 1 2)
        , cycleOfInterestPayment = Just "1Y+"
        , accruedInterest = Just 0.0
        , capitalizationEndDate = Nothing
        , interestCalculationBase = ICB_NT -- ICB_NT
        , interestCalculationBaseAmount = Just 0.0

        -- Notional Principal
        , maturityDate = Just (fromGregorian 2025 1 2)
        , contractDealDate = Just (fromGregorian 2015 1 1)
        , premiumDiscountAtIED = 0 -- 0
        , cycleAnchorDateOfPrincipalRedemption = Just (fromGregorian 2017 1 2)
        , cycleOfPrincipalRedemption = Just "1Y+"
        , nextPrincipalRedemptionPayment = Just 0.0
        , priceAtPurchaseDate = Just 0.0
        , priceAtTerminationDate = Just 0.0
        , marketObjectCodeOfScalingIndex = Nothing
        , scalingIndexAtStatusDate = Just 0.0
        , scalingEffect = Just SE_000 -- SE_000

        -- Optionality
        , penaltyType = PT_O -- PT_O
        , penaltyRate = 0.0 -- 0.0

        -- Rate Reset
        , cycleAnchorDateOfRateReset = Just (fromGregorian 2016 1 1)
        , cycleOfRateReset = Just "2Y+"
        , rateSpread = 0.05 -- 0.0
        , periodFloor = Just 0.0
        , periodCap = Just 0.0
        , lifeFloor = Just 0.0
        , lifeCap = Just 0.0
        , nextResetRate = Just 0.1
        }
      initState = Annuity.stateInit config
      generatedSchedules = generateSchedules [IED, IP, RR, RRF, PR] config
      scheduledEvents = scheduleEvents generatedSchedules
  in
    -- print "dasdasdasd"
    -- print (pof_ied_pam initState config)
    -- print (stf_ied_lam initState config (fromGregorian 2015 1 2))
    -- print generatedSchedules
    -- (print scheduledEvents)
    (print (Annuity.generateMarlowe (Map.toList scheduledEvents) initState config))
    -- print (eventScheduleCycleDatesBound config INF PR (> t0))
    -- print initState





-- {-# LANGUAGE OverloadedStrings #-}
-- module Main where
--
-- import           Data.Map.Strict                ( Map )
-- import qualified Data.Map.Strict               as Map
-- import           Data.Set                       ( Set )
-- import qualified Data.Set                      as Set
--
-- import qualified Data.Maybe                    as Maybe
-- import           Control.Monad.State
-- import           Test.Tasty
-- import           Test.Tasty.QuickCheck
-- import           Test.Tasty.HUnit
-- import           Debug.Trace
-- import Data.Time
--
-- import           Language.Marlowe
-- import           Language.Marlowe.Examples.ZCBG2
-- import           Language.Marlowe.ACTUS.ActusContracts
--
-- main :: IO ()
-- main = do
--     print $ contractLifespanUpperBound $
--         zeroCouponBondGuaranteed "investor" "issuer" "guarantor" 1000 200 (Slot 10) (Slot 20)
--     now <- getCurrentTime
--     let td = utctDay now
--     let couponBondFor3Month12PercentConfig = cb td (addGregorianMonthsClip 3 td) 1000 0.12
--     let zcbConfig = zcb td (addGregorianMonthsClip 6 td) 1000 (-150)
--     print $ genPrincialAtMaturnityContract
--                 "investor" "issuer" couponBondFor3Month12PercentConfig
--     print $ genPrincialAtMaturnityGuaranteedContract
--                 "investor" "issuer" "guarantor" couponBondFor3Month12PercentConfig
--     print $ genPrincialAtMaturnityContract
--                 "investor" "issuer" zcbConfig
--
--
-- couponBondFor3Month12Percent =
--     -- investor deposits 1000 Ada
--     When [ Case (Deposit "investor" "investor" (Constant 1000))
--         -- and pays it to the issuer
--         (Pay "investor" (Party "issuer") (Constant 1000)
--             -- after a month expect to receive 10 Ada interest
--             (When [ Case (Deposit "investor" "issuer" (Constant 10))
--                 -- and pay it to the investor
--                 (Pay "investor" (Party "investor" ) (Constant 10)
--                     -- same for 2nd month
--                     (When [ Case (Deposit "investor" "issuer" (Constant 10))
--                         (Pay "investor" (Party "investor" ) (Constant 10)
--                             -- after maturity date investor
--                             -- expects to receive notional + interest payment
--                             (When [ Case (Deposit "investor" "issuer" (Constant 1010))
--                                 (Pay "investor" (Party "investor" ) (Constant 1010) Close)]
--                             (Slot 1571788789)
--                             Close))]
--                     (Slot 1569196789)
--                     Close))]
--             (Slot 1566518389)
--             Close))]
--     (Slot 1563839989)
--     Close
--
--
-- zeroCouponBond = When [ Case
--         (Deposit "investor" "investor" (Constant 850))
--         (Pay "investor" (Party "issuer") (Constant 850)
--             (When
--                 [ Case (Deposit "investor" "issuer" (Constant 1000))
--                         (Pay "investor" (Party "investor" ) (Constant 1000) Close)
--                 ]
--                 (Slot 1579305589)
--                 Close
--             )
--         )
--     ]
--     (Slot 1563407989)
--     Close
--
-- couponBondGuaranteed = When [Case (Deposit "investor" "guarantor" (Constant 1030))
--     (When [Case (Deposit "investor" "investor" (Constant 1000))
--         (Pay "investor" (Party "issuer") (Constant 1000)
--             (When [Case (Deposit "investor" "issuer" (Constant 10))
--                 (Pay "investor" (Party "investor" ) (Constant 10)
--                 (Pay "investor" (Party "guarantor") (Constant 10)
--                     (When [Case (Deposit "investor" "issuer" (Constant 10))
--                         (Pay "investor" (Party "investor" ) (Constant 10)
--                         (Pay "investor" (Party "guarantor") (Constant 10)
--                             (When [Case (Deposit "investor" "issuer" (Constant 1010))
--                                 (Pay "investor" (Party "investor" ) (Constant 1010)
--                                 (Pay "investor" (Party "guarantor") (Constant 1010) Close))]
--                             (Slot 1571788789) Close)))]
--                     (Slot 1569196789) Close)))]
--             (Slot 1566518389) Close))]
--     (Slot 1563839989) Close)]
--     (Slot 1563839989) Close
--
--
-- couponBondGuaranteedWithAccounts =
--     -- guarantor deposits a whole payoff amount including interest payments
--     When [Case (Deposit "party2" "guarantor" (Constant 1030))
--         -- then it's same as for simple coupon bond
--         (When [Case (Deposit "party1" "party1" (Constant 1000))
--             (Pay "party1" (Party "party2") (Constant 1000)
--                 (When [Case (Deposit "party1" "party2" (Constant 10))
--                     (Pay "party1" (Party "party1") (Constant 10)
--                         (When [Case (Deposit "party1" "party2" (Constant 10))
--                             (Pay "party1" (Party "party1") (Constant 10)
--                                 (When [Case (Deposit "party1" "party2" (Constant 1010))
--                                     (Pay "party1" (Party "party1") (Constant 1010) Close)]
--                                 (Slot 1571788789)
--                                 -- if the issues fails to return notional
--                                 -- guarantor pays from his account and refunds
--                                 (Pay "party2" (Party "party1") (Constant 1010) Close))
--                             )]
--                         (Slot 1569196789)
--                         -- guarantor pays from his account and refunds
--                         (Pay "party2" (Party "party1") (Constant 1020) Close)))]
--                 (Slot 1566518389)
--                 -- guarantor pays from his account and refunds
--                 (Pay "party2" (Party "party1") (Constant 1030) Close)))]
--         -- if partees do not proceed guarantor automatically gets his money back
--         (Slot 1563839989) Close)]
--     (Slot 1563839989) Close
--
--
-- couponBondGuaranteedWithoutAccounts =
--     -- guarantor deposits a whole payoff amount including interest payments
--     When [Case (Deposit "party1" "guarantor" (Constant 1030))
--             -- investor deposits 1000 Ada
--         (When [Case (Deposit "party1" "party1" (Constant 1000))
--             (Pay "party1" (Party "party2") (Constant 1000)
--                 (When [Case (Deposit "party1" "party2" (Constant 10))
--                     (Pay "party1" (Party "party1") (Constant 10)
--                         (When [Case (Deposit "party1" "party2" (Constant 10))
--                             (Pay "party1" (Party "party1") (Constant 10)
--                                 (When [Case (Deposit "party1" "party2" (Constant 1010))
--                                     (Pay "party1" (Party "party1") (Constant 1010) Close)]
--                                 (Slot 1571788789)
--                                 (Pay "party1" (Party "party1") (Constant 1010)
--                                 -- with single account we have to
--                                 -- manually redistibute money to guarantor
--                                 (Pay "party1" (Party "guarantor") (Constant 20) Close))))]
--                         (Slot 1569196789)
--                         (Pay "party1" (Party "party1") (Constant 1020)
--                         -- in all the cases
--                         (Pay "party1" (Party "guarantor") (Constant 10) Close))))]
--                 (Slot 1566518389)
--                 -- here as well
--                 (Pay "party1" (Party "party1") (Constant 1030) Close)))]
--         (Slot 1563839989)
--         -- and thes are only 3 payments
--         (Pay "party1" (Party "guarantor") (Constant 1030) Close))]
--     (Slot 1563839989) Close
--
--
-- {- Simply swap two payments between parties -}
-- swapExample =
--     When [ Case (Deposit acc1 "party1" (Constant 500))
--             -- when 1st party committed, wait for 2nd
--             (When [ Case (Deposit acc2 "party2" (Constant 300))
--                 (Pay acc1 (Party "party2") (Constant 500)
--                 (Pay acc2 (Party "party1") (Constant 300) Close))
--                 ] date1
--             -- if a party dosn't commit, simply Close to the owner
--             Close)
--           , Case (Deposit acc2 "party2" (Constant 300))
--             -- if 2nd party committed first wait for 1st
--             (When [ Case (Deposit acc1 "party1" (Constant 500))
--                 -- we can just pay a diff between account and refund
--                 (Pay acc1 (Account acc2) (Constant 200) Close)
--             ] date1
--             -- if a party dosn't commit, simply Close to the owner
--             Close)
--         ] (date1 - gracePeriod) Close
--   where
--     gracePeriod = Slot 3*60*24 -- 24 hours
--     date1 = Slot 1563839989
--     acc1 = AccountId 1 "party1"
--     acc2 = AccountId 2 "party2"
--
-- {- Simply swap two payments between parties using single account, fully fungible -}
-- swapSingleAccount =
--     When [ Case (Deposit acc1 "party1" (Constant 500))
--             (When [ Case (Deposit acc1 "party2" (Constant 300))
--                 (Pay acc1 (Party "party2") (Constant 500)
--                 (Pay acc1 (Party "party1") (Constant 300) Close))
--                 ] (Slot date1)
--             -- refund to the 1st party
--             (Pay acc1 (Party "party1") (Constant 500) Close))
--          , Case (Deposit acc1 "party2" (Constant 300))
--             (When [ Case (Deposit acc1 "party1" (Constant 500))
--                 (Pay acc1 (Party "party2") (Constant 500)
--                 (Pay acc1 (Party "party1") (Constant 300) Close))
--             ] (Slot date1)
--             -- refund to the 2nd party
--             (Pay acc1 (Party "party2") (Constant 300) Close))
--         ] (Slot (date1 - gracePeriod)) Close
--   where
--     gracePeriod = 3*60*24 -- 24 hours
--     date1 = 1563839989
--     acc1 = AccountId 1 "party1"
--     acc2 = AccountId 2 "party2"
--
-- {- Swap two payments between parties, all payments are guaranteed by a 3rd party -}
-- swapGuaranteedExample =
--     When [ Case (Deposit acc3 "guarantor" (Constant 800))
--         (When [
--             Case (Deposit acc1 "party1" (Constant 500))
--                 (When [ Case (Deposit acc2 "party2" (Constant 300))
--                     (Pay acc1 (Party "party2") (Constant 500)
--                     (Pay acc2 (Party "party1") (Constant 300) Close))
--                     ] date1
--                     -- just transfer from guarantor account to party1 and refund to owner
--                     (Pay acc3 (Account acc1) (Constant 300) Close))
--             , Case (Deposit acc2 "party2" (Constant 300))
--                 (When [ Case (Deposit acc2 "party2" (Constant 500))
--                     (Pay acc1 (Party "party2") (Constant 500)
--                     (Pay acc2 (Party "party1") (Constant 300) Close))
--                     ] date1
--                     -- just transfer from guarantor account to party1 and refund to owner
--                     (Pay acc3 (Account acc2) (Constant 500) Close))
--             ] (date1 - gracePeriod)
--             -- automatically refund to guarantor if parties don't proceed
--             Close)
--         ] (date1 - 2 * gracePeriod) Close
--   where
--     gracePeriod = Slot 3*60*24 -- 24 hours
--     date1 = Slot 1563839989
--     acc1 = AccountId 1 "party1"
--     acc2 = AccountId 2 "party2"
--     acc3 = AccountId 3 "guarantor"
--
-- {-  Swap two payments between parties using single account.
--     All payments are guaranteed by a 3rd party.
-- -}
-- swapSingleAccountGuaranteedExample =
--     When [ Case (Deposit acc1 "guarantor" (Constant 800))
--         (When [
--             Case (Deposit acc1 "party1" (Constant 500))
--                 (When [ Case (Deposit acc1 "party2" (Constant 300))
--                     (Pay acc1 (Party "party2") (Constant 500)
--                     (Pay acc1 (Party "party1") (Constant 300)
--                     (Pay acc1 (Party "guarantor") (Constant 800) Close)))
--                     ] date1
--                     -- with single account all payments must be duplicated
--                     (Pay acc1 (Party "party2") (Constant 500)
--                     (Pay acc1 (Party "party1") (Constant 300)
--                     (Pay acc1 (Party "guarantor") (Constant 500) Close)))
--                 )
--             , Case (Deposit acc2 "party2" (Constant 300))
--                 (When [ Case (Deposit acc2 "party2" (Constant 500))
--                     (Pay acc1 (Party "party2") (Constant 500)
--                     (Pay acc1 (Party "party1") (Constant 300)
--                     (Pay acc1 (Party "guarantor") (Constant 800) Close)))
--                     ] date1
--                     -- with single account all payments must be duplicated
--                     (Pay acc1 (Party "party2") (Constant 500)
--                     (Pay acc1 (Party "party1") (Constant 300)
--                     (Pay acc1 (Party "guarantor") (Constant 00) Close)))
--                 )
--             ] (date1 - gracePeriod)
--             -- manually refund to guarantor if parties don't proceed
--             (Pay acc1 (Party "guarantor") (Constant 800) Close))
--         ] (date1 - 2 * gracePeriod) Close
--     where
--     gracePeriod = Slot 3*60*24 -- 24 hours
--     date1 = Slot 1563839989
--     acc1 = AccountId 1 "party1"
--     acc2 = AccountId 2 "party2"
--     acc3 = AccountId 3 "guarantor"
--
-- choiceIdExample :: ChoiceId
-- choiceIdExample = ChoiceId "RockPaperScissors" "Alice"
