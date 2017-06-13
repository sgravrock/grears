{-# LANGUAGE ImplicitParams #-}

import System.Exit
import Test.HUnit
import Test.HUnit.Approx

import Lib


tests =
    let ?epsilon = 0.1 in
    test ["GearRatio" ~: 0.67 ~~? computeOnePair 24 36 GearRatio
        , "GearInches" ~: 19.3 ~~? computeOnePair 24 36 (GearInches 28.94)
         ]

run = runTestTT tests

main :: IO ()
main = do
    counts2 <- run
    if (errors counts2 + failures counts2 == 0)
        then exitSuccess
        else exitFailure
