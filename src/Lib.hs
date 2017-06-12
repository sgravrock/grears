module Lib
    ( someFunc
    , Units(..)
    , computeOnePair
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Units a = GearRatio | GearInches a

computeOnePair :: Fractional a => Integer -> Integer -> Units a -> a
computeOnePair x y GearRatio = ratio x y
computeOnePair x y (GearInches wheelDiameter) = wheelDiameter * ratio x y

ratio :: Fractional a => Integer -> Integer -> a
ratio x y = (fromInteger x) / (fromInteger y)
