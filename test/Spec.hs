{-# LANGUAGE ImplicitParams #-}

import System.Exit
import Test.HUnit
import Test.HUnit.Approx

import Lib

allFront = [4, 2]
allRear = [1, 2]
allExpected = [ [4, 2]
              , [2, 1]
              ]

tests =
    let ?epsilon = 0.1 in
    test [ "GearRatio" ~: 0.67 ~~? computeOnePair 24 36 GearRatio
         , "GearInches" ~: 19.3 ~~? computeOnePair 24 36 (GearInches 28.94)
         , "MphAtRpm" ~: 29.6 ~~? computeOnePair 42 11 (MphAtRpm 90 28.94)
         , "allPairs" ~: allExpected ~=? allPairs allFront allRear GearRatio
         ]

run = runTestTT tests

main :: IO ()
main = do
    counts2 <- run
    if (errors counts2 + failures counts2 == 0)
        then exitSuccess
        else exitFailure
