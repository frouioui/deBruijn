module Tools
    ( singleChar
    , checkDeBruijn
    , isSimilar
    , checkLine
    , checkLines
    , filterSequence
    ) where

import Prelude
import Generation
import Data.List
import System.Exit

{--
Check if every char in a string are unique
--}
singleChar :: String -> Bool
singleChar xs = checkSingle (sort xs) (drop 1 (sort xs))
    where
        checkSingle [] _            = True
        checkSingle _ []            = True
        checkSingle (x:xs') (y:ys)  = do
            case x /= y of
                True    -> checkSingle xs' ys
                False   -> False

{--
Move the n index from the tail to the head of the list and return the list
--}
rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate 0 xs = xs
rotate n xs = rotate (n - 1) (last xs : init xs)

{--
Check if 2 list are similiar in both ways (by rotating them)
--}
isSimilar :: (Eq a) => [a] -> [a] -> Bool
isSimilar [] []         = True
isSimilar (xs) (ys)     = checkIfArrayContainsTrue [ rotate x xs == ys | x <- [0..(length xs)] ]

{--
check if the given array of Bool contains at least one True value
--}
checkIfArrayContainsTrue :: [Bool] -> Bool
checkIfArrayContainsTrue []     = False
checkIfArrayContainsTrue (x:xs)
            | x         = True
            | otherwise = checkIfArrayContainsTrue xs

{--
Get all the words of size n in a list
--}
getWordsInArray :: Int -> [a] -> [[a]]
getWordsInArray size xs = getEachWords (xs ++ take (size - 1) xs) size

{--
Return an array containing all the substring of size n
--}
getEachWords :: [a] -> Int -> [[a]]
getEachWords arr size
        | length arr == size    = [take size arr]
        | otherwise             = take size arr : getEachWords (drop 1 arr) size

{--
Transform an array of Int into a String, each index of the [Int] correpond
to the index in the given alphabet
--}
getStringFromGeneration :: [Int] -> String -> String
getStringFromGeneration arr alphabet = [ alphabet !! x | x <- arr]

{--
Check if the given sequence is a debruijn sequence
--}
checkDeBruijn :: Int -> String -> String -> Bool
checkDeBruijn n str line = (sort (getWordsInArray n (getStringFromGeneration (generation (startDeBruijn n str)) str))) == (sort (getWordsInArray n line))

{--
Check if the given char is part of the given alphabet
--}
isInAlphabet :: Char -> String -> Bool
isInAlphabet _ []     = False
isInAlphabet c (x:xs) = do
    case c == x of
        True    -> True
        False   -> isInAlphabet c xs

{--
Line parser
--}
checkLine :: String -> String -> IO ()
checkLine [] s = putStr ""
checkLine (x:xs) s
    | isInAlphabet x s == True  = checkLine xs s
    | otherwise                 = exitWith (ExitFailure 84)

checkLines :: [String] -> String -> IO ()
checkLines [] _     = putStr ""
checkLines (x:xs) s = do checkLine x s ; checkLines xs s


{--
Filter
--}
filterSequence :: (a -> a -> Bool) -> [a] -> [a]
filterSequence _ []        = []
filterSequence _ [x]       = [x]
filterSequence f (x:xs)    = x : [ y | y <- filterSequence f xs, f y x ]
