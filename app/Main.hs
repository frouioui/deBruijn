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
                    False -> handleOption opt
        Left err -> do
            case err of
                ["no args"] -> printUsageError
                err -> printErrorArgument err

handleOption :: Options -> IO ()
handleOption opt = do
    case (flag opt) of
        None -> print $ opt
        Check -> print "check"
        Clean -> print "clean"
        Unique -> print "unique"