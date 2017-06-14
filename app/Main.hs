module Main where

import System.Environment
import Lib

main :: IO ()
main = do
    rawArgs <- getArgs
    let args = parseArgs rawArgs
        result = allPairs (front args) (rear args) (units args)
    putStrLn (show result)
