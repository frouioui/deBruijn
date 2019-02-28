module Flags
    ( check
    , unique
    , clean
    ) where

import Prelude

import Tools

{--
Check function, checking if the given line is a deBruijn sequence
--}
check :: Int -> String -> IO ()
check n s = do
    line <- getLine
    checkLine line s
    case (checkDeBruijn n s line) of
        True    -> putStrLn "OK"
        False   -> putStrLn "KO"

{--
Check if the given 2 lines are unique
--}
unique :: Int -> String -> IO ()
unique n s = do
    line1 <- getLine
    line2 <- getLine
    checkLines ([line1] ++ [line2]) s
    case ((checkDeBruijn n s line1) && (checkDeBruijn n s line2) && (not $ isSimilar line1 line2)) of
        True    -> putStrLn "OK"
        False   -> putStrLn "KO"

{--
Get multiple lines from the user and display only the valid line
and the lines that are unique
--}
clean :: Int -> String -> IO ()
clean n s = do
    lines <- getAllLines "END"
    checkLines lines s
    putStr $ foldr (\x y -> x ++ "\n" ++ y) "" (filterNotDeBruijn $ filterSequence (\x y -> not $ isSimilar x y) lines)
    where
        filterNotDeBruijn xs = [ x | x <- xs, checkDeBruijn n s x ]

{--
Get all lines from the stdin until the user enter the "end" string
--}
getAllLines :: String -> IO ([String])
getAllLines end = do
    lines <- getLines [] end
    return $ lines

{--
Store all the lines entered by the user until we reach the end string and
return the array of stored string
--}
getLines :: [String] -> String -> IO ([String])
getLines arr end = do
    line <- getLine
    case line == end of
        True    -> return arr
        False   -> getLines (arr ++ [line]) end

{--
Display the whole array of string
--}
displayLines :: [String] -> IO ()
displayLines []     = putStr ""
displayLines (x:xs) = do putStrLn x ; displayLines xs