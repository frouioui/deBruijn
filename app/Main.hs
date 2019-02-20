module Main
    ( main
    ) where

import Lib
import Argument
import System.Environment

import Argument

main :: IO ()
main = do
    x <- Argument.handleArgument
    print $ x
