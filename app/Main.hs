module Main where

import System.Environment
import System.IO (hPutStrLn, stderr)
import System.Exit
import Lib


main :: IO ()
main = do
    rawArgs <- getArgs
    case parseArgs rawArgs of
        Just args -> 
            let result = allPairs (front args) (rear args) (units args) in
                do
                    putStrLn (show result)
                    exitSuccess
        Nothing -> do
            hPutStrLn stderr "Usage: grears -f front gears -r rear gears {gearRatio | gearInches wheelDiameter | mphAtRpm rpm wheelDiameter}"
            exitFailure
