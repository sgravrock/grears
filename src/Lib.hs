module Lib
    ( CliArgs(..)
    , ResultUnits(..)
    , parseArgs
    , parseIntList
    , consume
    , onePair
    , allPairs
    ) where

import Text.Read

data CliArgs = CliArgs
    { front :: [Integer]
    , rear :: [Integer]
    , units :: ResultUnits
    } deriving (Eq, Show)

data ResultUnits = GearRatio
                 | GearInches { diameter :: Float }
                 | MphAtRpm { rpm :: Integer, diameter :: Float }
                 deriving (Eq, Show)

parseArgs :: [String] -> Maybe CliArgs
parseArgs args = do
    (front, rest0) <- parseGearList args "-f"
    (rear, rest1) <- parseGearList rest0 "-r"
    units <- parseUnits rest1
    Just (CliArgs front rear units)

parseGearList :: [String] -> String -> Maybe ([Integer], [String])
parseGearList args flag = consume flag args parseNonEmptyIntList

consume :: String -> [String] -> ([String] -> Maybe a) -> Maybe a
consume prefix (x:xs) f
    | x == prefix = f xs
    | otherwise = Nothing


parseNonEmptyIntList :: [String] -> Maybe([Integer], [String])
parseNonEmptyIntList args =
    let (list, rest) = parseIntList args in
        if length list > 0
            then Just (list, rest)
            else Nothing

parseIntList :: [String] -> ([Integer], [String])
parseIntList (x:xs) = case readMaybeInteger x of
                        Just n ->
                            let (restInts, extra) = parseIntList xs
                            in ((n:restInts), extra)
                        Nothing -> ([], (x:xs))

readMaybeInteger :: String -> Maybe Integer
readMaybeInteger s = readMaybe s

parseUnits :: [String] -> Maybe ResultUnits
parseUnits args = consume "-u" args parseUnits2

parseUnits2 :: [String] -> Maybe ResultUnits
parseUnits2 (x:xs) = case x of
                      "gearRatio" -> Just GearRatio
                      "gearInches" -> Just (GearInches (read (head xs)))
                      "mphAtRpm" -> let (r:(d:xxs)) = xs in
                        Just (MphAtRpm (read r) (read d))
                      otherwise -> Nothing

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
