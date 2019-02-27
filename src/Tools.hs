module Tools
    ( singleChar
    , checkDeBruijn
    , areUniques
    , isSimilar
    , keepDeBruijn
    , checkLine
    , checkLines
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
areUniques arr = checkUnique (sort arr) (drop 1 (sort arr))
    where
        checkUnique [] _            = True
        checkUnique _ []            = True
        checkUnique (x:xs) (y:ys)   = do
            case x /= y of
                True    -> checkUnique xs ys
                False   -> False

rotateArray :: Int -> [a] -> [a]
rotateArray i arr
        | length arr == 0   = []
        | i == 0            = arr
        | otherwise         = rotateArray (i - 1) ((last arr):(init arr))

isSimilar :: (Eq a) => [a] -> [a] -> Bool
isSimilar [] [] = True
isSimilar (x:xs) (y:ys)
        | x == y    = isSimilar xs ys
        | otherwise = False

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

keepDeBruijn :: [String] -> Int -> String -> [String] -> [String]
keepDeBruijn [] _ _ res     = res
keepDeBruijn (x:xs) n s res = do
        case (checkDeBruijn n s x) of
            True    -> keepDeBruijn xs n s (res ++ [x])
            False   -> keepDeBruijn xs n s res

isInAlphabet :: Char -> String -> Bool
isInAlphabet _ []     = False
isInAlphabet c (x:xs) = do
    case c == x of
        True    -> True
        False   -> isInAlphabet c xs

checkLine :: String -> String -> IO ()
checkLine [] s = putStr ""
checkLine (x:xs) s
    | isInAlphabet x s == True  = checkLine xs s
    | otherwise                 = exitWith (ExitFailure 84)

checkLines :: [String] -> String -> IO ()
checkLines [] _     = putStr ""
checkLines (x:xs) s = do checkLine x s ; checkLines xs s