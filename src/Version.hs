module Version
    (
    printVersionSuccess,
    printVersion
    ) where

import System.Exit

{--
Print the version and then exit the program with 0
--}
printVersionSuccess :: IO ()
printVersionSuccess = do
    printVersion
    exitWith ExitSuccess

{--
Print the usage of the program
--}
printVersion :: IO ()
printVersion = do
    putStrLn "0.0.1"