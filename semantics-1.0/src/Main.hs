module Main where

import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set

import qualified Data.Maybe                    as Maybe
import           Control.Monad.State
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Tasty.HUnit
import           Debug.Trace

import           Semantics                     as S
import           GenSemantics
import           SPJModel                hiding ( Contract(..) )
import qualified SPJModel                      as SPJ
import           CrowdFunding
import           DepositIncentive
import           Escrow

evaluateMaximumValueTests :: [TestTree]
evaluateMaximumValueTests = [evalCrowdFunding, evalDepositIncentive, evalEscrow]
  where
    evalCrowdFunding = testCase "Evaluate CrowdFunding balances" $ do
        let bounds = Bounds
                { oracleBounds = Map.empty
                , choiceBounds = Map.fromList
                    [ (IdentChoice 1, (0, 1000))
                    , (IdentChoice 2, (0, 1000))
                    , (IdentChoice 3, (0, 1000))
                    , (IdentChoice 4, (0, 1000))
                    ]
                }
        let balances = evaluateMaximumValue bounds crowdFunding
        balances @?= Map.fromList
            [ (1, Balance 0 1000)
            , (2, Balance 0 1000)
            , (3, Balance 0 1000)
            , (4, Balance 0 1000)
            , (5, Balance 4000 0)
            ]

    evalDepositIncentive = testCase "Evaluate DepositIncentive balances" $ do
        let balances = evaluateMaximumValue emptyBounds depositIncentive
        balances @?= Map.fromList [(1, Balance 20 0), (2, Balance 0 20)]

    evalEscrow = testCase "Evaluate Escrow balances" $ do
        let balances = evaluateMaximumValue emptyBounds escrow
        balances @?= Map.fromList [(1, Balance 0 450), (2, Balance 450 0)]

checkValueWithinBounds = do
    let bounds = Bounds
            { choiceBounds = Map.fromList [(IdentChoice 1, (400, 444)), (IdentChoice 2, (500, 555))]
            , oracleBounds = Map.singleton "oil" (200, 333)
            }
    let state  = EvalState Map.empty (Map.singleton (IdentCC 1) 1000)

    let values = boundedValue (Set.fromList [1, 2]) (Set.fromList [IdentCC 1]) bounds
    testProperty "Check Value is within bounds" $ forAll values $ \value -> do
        let maximum = evalMaxBoundedValue bounds state value
        let state = State
                { letEnv = Map.empty
                , sc     = Map.singleton (IdentCC 1) (1, NotRedeemed 1000 12345)
                , sch    = Map.fromList
                    [ ((IdentChoice 1, 1), 444)
                    , ((IdentChoice 1, 2), 444)
                    , ((IdentChoice 2, 1), 555)
                    , ((IdentChoice 2, 2), 555)
                    ]
                }
        let observables = OS { random = 0, blockNumber = 0, oracles = Map.singleton "oil" 256 }
        let actual = evalValue state observables value
        actual <= maximum

tests :: [TestTree]
tests = evaluateMaximumValueTests ++ [checkValueWithinBounds]

main :: IO ()
main = do
    print $ zcbMarlowe 100 12345 1 2
    let c = SPJ.One
    print $ translateSPJContractToMarlowe 1 8 (SPJ.One)
    print $ translateSPJContractToMarlowe 1 8 (SPJ.Give $ SPJ.One)
    defaultMain (testGroup "Marlowe Tests" tests)
