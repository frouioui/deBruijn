module Main
    ( main
    ) where

import Argument
import System.Environment
import System.Exit

import Argument

main :: IO ()
main = do
    args <- Argument.handleArgument
    case args of
        Right opt -> do
            case (helper opt) of
                True -> do
                    printUsage
                    exitWith ExitSuccess
                False -> case (version opt) of
                    True -> print "version"
        Left err -> printErrorArgument err

