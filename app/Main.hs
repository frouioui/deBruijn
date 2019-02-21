module Main
    ( main
    ) where

import Argument
import System.Environment
import System.Exit

import Argument
import Usage
import Version

main :: IO ()
main = do
    args <- Argument.handleArgument
    case args of
        Right opt -> do
            case (helper opt) of
                True -> printUsageSuccess
                False -> case (version opt) of
                    True -> printVersionSuccess
                    False -> putStrLn "the program starts here"
        Left err -> do
            case err of
                ["no args"] -> printUsageError
                err -> printErrorArgument err
