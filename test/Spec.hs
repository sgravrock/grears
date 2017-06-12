{-# LANGUAGE ImplicitParams, ScopedTypeVariables #-}

import Control.Monad
import System.Exit
import Test.HUnit
import Test.HUnit.Approx

import Lib


tests =
    let ?epsilon = 0.0001 in
    test ["manual" ~: 4.0 ~~? 4.000000001]

run = runTestTT tests

main :: IO ()
main = do
    counts2 <- run
    if (errors counts2 + failures counts2 == 0)
        then exitSuccess
        else exitFailure

