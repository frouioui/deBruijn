module Flags
    ( check
    ) where

import Prelude

import String

check :: Int -> String -> IO ()
check order alphabet = do
    line <- getLine
    case (isDeBruijnSequence order alphabet line) of
        True    -> putStrLn "OK"
        False   -> putStrLn "KO"