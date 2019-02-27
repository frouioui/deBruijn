module Tools
    ( singleChar
    , checkDeBruijn
    , areUniques
    , isSimilar
    , checkLine
    , checkLines
    , filterSequence
    ) where

import Prelude
import Generation
import Data.List
import System.Exit

singleChar :: String -> Bool
singleChar xs = checkSingle (sort xs) (drop 1 (sort xs))
    where
        checkSingle [] _            = True
        checkSingle _ []            = True
        checkSingle (x:xs') (y:ys)  = do
            case x /= y of
                True    -> checkSingle xs' ys
                False   -> False

areUniques :: (Ord a) => [a] -> Bool
areUniques arr = unique (sort arr) (drop 1 (sort arr))
    where
        unique [] _            = True
        unique _ []            = True
        unique (x:xs) (y:ys)   = do
            case x /= y of
                True    -> unique xs ys
                False   -> False

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate 0 xs = xs
rotate n xs = rotate (n - 1) (last xs : init xs)

isSimilar :: (Eq a) => [a] -> [a] -> Bool
isSimilar [] []         = True
isSimilar (xs) (ys) = foldr (||) False [ rotate x xs == ys | x <- [0..(length xs)] ]

getWordsInArray :: Int -> [a] -> [[a]]
getWordsInArray size xs = getEachWords (xs ++ take (size - 1) xs) size

getEachWords :: [a] -> Int -> [[a]]
getEachWords arr size
        | length arr == size    = [take size arr]
        | otherwise             = take size arr : getEachWords (drop 1 arr) size

getStringFromGeneration :: [Int] -> String -> String
getStringFromGeneration arr alphabet = [ alphabet !! x | x <- arr]

checkDeBruijn :: Int -> String -> String -> Bool
checkDeBruijn n str line = (sort (getWordsInArray n (getStringFromGeneration (generation (startDeBruijn n str)) str))) == (sort (getWordsInArray n line))

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
