module Main
    ( main
    ) where

import Argument
import System.Environment
import System.Exit
import Text.Printf

import Argument
import Usage
import Version
import Generation
import Flags

main :: IO ()
main = do
    args <- Argument.handleArgument
    case args of
        Right opt -> do
            -- FIXME: Change this ugly way to handle flag
            case (helper opt) of
                True    -> printUsageSuccess
                False   -> case (version opt) of
                    True    -> printVersionSuccess
                    False   -> handleOption opt
        Left err -> do
            case err of
                ["no args"]     -> printUsageError
                err             -> printErrorArgument err

handleOption :: Options -> IO ()
handleOption opt = do
    case (flag opt) of
        None        -> do printResult (generation (startDeBruijn (order opt) (alphabet opt))) (alphabet opt)
        Check       -> check (order opt) (alphabet opt)
        Clean       -> clean (order opt) (alphabet opt)
        Unique      -> unique (order opt) (alphabet opt)

printResult :: [Int] -> String -> IO ()
printResult [] _        = printf "\n"
printResult (x:xs) s    = do printf "%c" (s !! x) ; printResult xs s