module Lib
    ( CliArgs(..)
--    , ArgParseResult(..)
    , ResultUnits(..)
    , parseArgs
    , parseIntList
    , onePair
    , allPairs
    ) where

import Text.Read

data CliArgs = CliArgs
    { front :: [Integer]
    , rear :: [Integer]
    , units :: ResultUnits
    } deriving (Eq, Show)

--data ArgParseResult = ArgsOk CliArgs | ArgsBad String deriving (Eq, Show)

data ResultUnits = GearRatio
                 | GearInches { diameter :: Float }
                 | MphAtRpm { rpm :: Integer, diameter :: Float }
                 deriving (Eq, Show)

parseArgs :: [String] -> CliArgs --ArgParseResult
parseArgs args =
    let (front, rest0) = parseFrontGears args
        (rear, rest1) = parseRearGears rest0
        units = parseUnits rest1
    in CliArgs front rear units

parseFrontGears :: [String] -> ([Integer], [String])
-- TODO verify the -f
parseFrontGears (x:xs) = parseIntList xs

parseRearGears :: [String] -> ([Integer], [String])
-- TODO verify the -r
parseRearGears (x:xs) = parseIntList xs

parseIntList :: [String] -> ([Integer], [String])
parseIntList (x:xs) = case readMaybeInteger x of
                        Just n ->
                            let (restInts, extra) = parseIntList xs
                            in ((n:restInts), extra)
                        Nothing -> ([], (x:xs))

readMaybeInteger :: String -> Maybe Integer
readMaybeInteger s = readMaybe s

parseUnits :: [String] -> ResultUnits
-- TODO verify the -u
parseUnits (flag:(x:xs)) = case x of
                      "gearRatio" -> GearRatio
                      "gearInches" -> GearInches (read (head xs))
                      "mphAtRpm" -> let (r:(d:xxs)) = xs in
                        MphAtRpm (read r) (read d)
                      otherwise -> error ("nope: " ++ x)

onePair :: Integer -> Integer -> ResultUnits -> Float
onePair x y GearRatio = ratio x y
onePair x y (GearInches wheelDiameter) = wheelDiameter * ratio x y
onePair x y (MphAtRpm rpm wheelDiameter) = 
    let inchesDevelopment = (circumference wheelDiameter) * (ratio x y)
        inchesPerMinute = inchesDevelopment * (realToFrac rpm) * 60
    in inchesPerMinute / 63360

allPairs :: [Integer] -> [Integer] -> ResultUnits -> [[Float]]
allPairs fronts rears units = map (\f -> allForFront f rears units) fronts 

allForFront :: Integer -> [Integer] -> ResultUnits -> [Float]
allForFront front rears units = map (\r -> onePair front r units) rears

ratio :: Integer -> Integer -> Float
ratio x y = (fromInteger x) / (fromInteger y)

circumference :: Float -> Float
circumference diameter = pi * diameter
