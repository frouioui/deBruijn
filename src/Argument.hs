module Argument
    ( Options(..),
    Flag(..),
    handleArgument,
    printErrorArgument,
    ) where

{--
Imports
--}
import Control.Monad
import System.Environment(getArgs)
import System.Exit
import System.Console.GetOpt

import Usage

{--
Declaration of the Args datatype, this datatype
represent the program's arguments
--}
data Options = Options
    { helper :: Bool,
    version :: Bool
    } deriving Show

{--
Declaration of the Flag datatype used for GetOpt
--}
data Flag = None
            | Version
            | Helper
    deriving Show

{--
Hold the default value of the Options datatype
--}
startOption :: Options
startOption = Options
    { helper = False,
    version = False
    }

options :: [ OptDescr (Options -> Either String Options) ]
options =
    [
        Option ['v'] ["version"] (NoArg (\opt -> Right opt { version = True } )) "Print the version of the program",
        Option ['h'] ["help"] (NoArg (\opt -> Right opt { helper = True } )) "Print the usage of the program"
    ]

handleArgument :: IO (Either [String] Options)
handleArgument = do
    argv <- getArgs
    case getOpt Permute options argv of
        ([], [], []) -> return $ Left ["no args"]
        (opts, args, []) -> case foldM (flip id) startOption opts of
            Right opt -> return $ Right opt
        (_, _, err) -> return $ Left err

{--
Print all the given error and finaly print the usage before exiting program
--}
printErrorArgument :: [String] -> IO ()
printErrorArgument err = do
    case err of
        [] -> do
            putStr "\n"
            printUsageError
        (err:errs) -> do
            putStr err
            printErrorArgument errs