module String
    ( singleChar
    , isDeBruijnSequence
    ) where

import Data.List
import Prelude hiding(sequence, words)
import Generation

singleChar :: String -> Bool
singleChar xs = checkSingle sorted (drop 1 sorted)
    where
        sorted                      = sort xs
        checkSingle (x:xs') (y:ys)  = if x /= y then checkSingle xs' ys else False
        checkSingle [] _            = True
        checkSingle _ []            = True

getWords :: Int -> [a] -> [[a]]
getWords size xs = getEachWords (xs ++ take (size - 1) xs) size

getEachWords :: [a] -> Int -> [[a]]
getEachWords arr size
        | length arr == size    = [take size arr]
        | otherwise             = take size arr : getEachWords (drop 1 arr) size

isDeBruijnSequence :: Int -> String -> String -> Bool
isDeBruijnSequence order alphabet sequence = reference == words
    where
        reference   = sort (getWords order (getStringFromGeneration (generation (startDeBruijn order alphabet)) alphabet))
        words       = sort (getWords order sequence)

getStringFromGeneration :: [Int] -> String -> String
getStringFromGeneration arr alphabet = [ alphabet !! x | x <- arr]