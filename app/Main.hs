module Main
    ( main
    ) where

import Argument
import System.Environment

import Argument

main :: IO ()
main = do
    opts <- Argument.handleArgument
    print $ opts

