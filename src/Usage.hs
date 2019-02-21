module Usage
    (
    printUsageError,
    printUsageSuccess,
    printUsage
    ) where

import System.Exit

{--
Print the usage and then exit the program with 84
--}
printUsageError :: IO ()
printUsageError = do
    printUsage
    exitWith (ExitFailure 84)

{--
Print the usage and then exit the program with 0
--}
printUsageSuccess :: IO ()
printUsageSuccess = do
    printUsage
    exitWith ExitSuccess

{--
Print the usage of the program
--}
printUsage :: IO ()
printUsage = do
    putStrLn "USAGE: ./deBruijn n [a] [--check|--unique|--clean|--version|--help]\n"
    putStrLn "\t--check\t\tcheck if a sequence is a de Bruijn sequence"
    putStrLn "\t--unique\tcheck if 2 sequences are distinct de Bruijn sequences"
    putStrLn "\t--clean\t\tlist cleaning"
    putStrLn "\t--version -v\tdisplay the version of the program"
    putStrLn "\t--help -h\tdisplay this usage"
    putStrLn "\tn\t\torder of the sequence"
    putStrLn "\ta\t\talphabet [def: \"01\"]"