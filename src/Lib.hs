module Lib
    ( someFunc
    , ResultUnits(..)
    , onePair
    , allPairs
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data ResultUnits = GearRatio | GearInches Float | MphAtRpm Integer Float

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
