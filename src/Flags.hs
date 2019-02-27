module Flags
    ( check
    , unique
    ) where

import Prelude

import String

check :: Int -> String -> IO ()
check order alphabet = do
    line <- getLine
    case (isDeBruijnSequence order alphabet line) of
        True    -> putStrLn "OK"
        False   -> putStrLn "KO"

unique :: Int -> String -> IO ()
unique order alphabet = do
    line1 <- getLine
    line2 <- getLine
    case (checkSequence line1 && checkSequence line2 && (not $ areEquivalents line1 line2)) of
        True    -> putStrLn "OK"
        False   -> putStrLn "KO"
    where checkSequence = isDeBruijnSequence order alphabet
