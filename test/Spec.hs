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

isOk :: Maybe a -> Bool
isOk (Just _) = True
isOk Nothing = False

inc :: Integer -> Maybe Integer
inc x = Just (x + 1)

tests =
    let ?epsilon = 0.1 in
    test [ "GearRatio" ~: 0.67 ~~? onePair 24 36 GearRatio
         , "GearInches" ~: 19.3 ~~? onePair 24 36 (GearInches 28.94)
         , "MphAtRpm" ~: 29.6 ~~? onePair 42 11 (MphAtRpm 90 28.94)
         , "allPairs" ~: allExpected ~=? allPairs allFront allRear GearRatio
         , "parseArgs GearRatio" ~: Just CliArgs { front = [1,2], rear = [3,4], units = GearRatio } ~=? parseArgs ["-f", "1", "2", "-r", "3", "4", "-u", "gearRatio"]
         , "parseArgs GearInches" ~: Just CliArgs { front = [1], rear = [2,3], units = GearInches 12.3 } ~=? parseArgs ["-f", "1", "-r", "2", "3", "-u", "gearInches", "12.3"]
         , "parseArgs MphAtRpm" ~: Just CliArgs { front = [1], rear = [2,3], units = MphAtRpm 80 12.3 } ~=? parseArgs ["-f", "1", "-r", "2", "3", "-u", "mphAtRpm", "80", "12.3"]
         , "parseArgs missing flag" ~: Nothing ~=? parseArgs ["-f", "1", "2", "-r", "3", "4", "gearRatio"]
         , "parseArgs empty gears" ~: False ~=? isOk (parseArgs ["-f", "-r", "3", "4", "-u", "gearRatio"])
         , "parseArgs bad units" ~: Nothing ~=? parseArgs ["-f", "1", "2", "-r", "3", "4", "-u", "bogus"]
         , "parseIntList non-empty" ~: ([1, 2], ["x"]) ~=? parseIntList ["1", "2", "x"]
         , "parseIntList empty" ~: ([], ["x"]) ~=? parseIntList ["x"]
         , "consume match" ~: Just("foo") ~=? consume "-x" ["-x", "foo"] (\x -> Just (head x))
         , "consume mismatch" ~: Nothing ~=? consume "-x" ["foo"] (\x -> Just 1)
         ]

run = runTestTT tests

main :: IO ()
main = do
    counts2 <- run
    if (errors counts2 + failures counts2 == 0)
        then exitSuccess
        else exitFailure
