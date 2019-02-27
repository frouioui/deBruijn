module String
    ( singleChar
    , isDeBruijnSequence
    , areUniques
    , areEquivalents
    ) where

import Data.List
import Prelude hiding(sequence, words)
import Generation
import Data.List hiding(words)

singleChar :: String -> Bool
singleChar xs = checkSingle sorted (drop 1 sorted)
    where
        sorted                      = sort xs
        checkSingle (x:xs') (y:ys)  = if x /= y then checkSingle xs' ys else False
        checkSingle [] _            = True
        checkSingle _ []            = True

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate 0 xs = xs
rotate n xs = rotate (n - 1) (last xs : init xs)

areUniques :: (Ord a) => [a] -> Bool
areUniques xs = checkUnique (sort xs) (drop 1 sorted)

checkUnique :: [a] -> [a] -> Bool
checkUnique [] _            = True
checkUnique _ []            = True
checkUnique (x:xs) (y:ys)   = do
        case x /= y of
            True    -> checkUnique xs ys
            False   -> return False

areEquivalents :: (Eq a) => [a] -> [a] -> Bool
areEquivalents [] [] = True
areEquivalents xs ys = foldr (||) False [ rotate x xs == ys | x <- [0..(length xs)] ]

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