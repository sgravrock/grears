module Lib
    ( someFunc
    , ResultUnits(..)
    , computeOnePair
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data ResultUnits a = GearRatio | GearInches a

computeOnePair :: Fractional a => Integer -> Integer -> ResultUnits a -> a
computeOnePair x y GearRatio = ratio x y
computeOnePair x y (GearInches wheelDiameter) = wheelDiameter * ratio x y

ratio :: Fractional a => Integer -> Integer -> a
ratio x y = (fromInteger x) / (fromInteger y)
