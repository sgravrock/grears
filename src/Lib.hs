module Lib
    ( someFunc
    , ResultUnits(..)
    , computeOnePair
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data ResultUnits a = GearRatio | GearInches a | MphAtRpm Integer a

computeOnePair :: Fractional a => Integer -> Integer -> ResultUnits a -> a
computeOnePair x y GearRatio = ratio x y
computeOnePair x y (GearInches wheelDiameter) = wheelDiameter * ratio x y
computeOnePair x y (MphAtRpm rpm wheelDiameter) = 
    let inchesDevelopment = (circumference wheelDiameter) * (ratio x y)
        inchesPerMinute = inchesDevelopment * (realToFrac rpm) * 60
    in inchesPerMinute / 63360

ratio :: Fractional a => Integer -> Integer -> a
ratio x y = (fromInteger x) / (fromInteger y)

circumference :: Fractional a => a -> a
circumference diameter = (realToFrac pi) * diameter
