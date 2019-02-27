module Flags
    ( check
    , unique
    , clean
    ) where

import Prelude

import Tools

check :: Int -> String -> IO ()
check n s = do
    line <- getLine
    checkLine line s
    case (checkDeBruijn n s line) of
        True    -> putStrLn "OK"
        False   -> putStrLn "KO"

unique :: Int -> String -> IO ()
unique n s = do
    line1 <- getLine
    line2 <- getLine
    checkLines ([line1] ++ [line2]) s
    case ((checkDeBruijn n s line1) && (checkDeBruijn n s line2) && (not $ isSimilar line1 line2)) of
        True    -> putStrLn "OK"
        False   -> putStrLn "KO"

clean :: Int -> String -> IO ()
clean n s = do lines <- (getAllLines "END") ; checkLines lines s ; displayLines lines

getAllLines :: String -> IO ([String])
getAllLines end = do
    lines <- getLines [] end
    return $ lines

getLines :: [String] -> String -> IO ([String])
getLines arr end = do
    line <- getLine
    case line == end of
        True    -> return arr
        False   -> getLines (arr ++ [line]) end

displayLines :: [String] -> IO ()
displayLines []     = putStr ""
displayLines (x:xs) = do putStrLn x ; displayLines xs
