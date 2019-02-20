module Argument
    ( Options(..),
    Flag(..),
    handleArgument
    ) where

{--
Imports
--}
import Control.Monad
import System.Environment(getArgs)
import System.Exit
import System.Console.GetOpt

{--
Declaration of the Args datatype, this datatype
represent the program's arguments
--}
data Options = Options
    { flag :: Flag
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
    { flag = None
    }

options :: [ OptDescr (Options -> IO Options) ]
options =
    [
        Option ['h'] ["help"] (NoArg (\opt -> return opt { flag = Helper} )) "Print the usage of the program",
        Option ['v'] ["version"] (NoArg (\opt -> return opt { flag = Version } )) "Print the version of the program"
    ]

handleArgument :: IO (Either String Options)
handleArgument = do
    argv <- getArgs
    case getOpt Permute options argv of
        -- If there is no error
        (opts, args, []) -> case foldM (flip id) startOption opts of
            opt -> return $ Right startOption
        (_, _, err) -> return $ Left "usage"

{--
Print all the given error and finaly print the usage before exiting program
--}
printErrorArgument :: [String] -> IO (String)
printErrorArgument err = do
    case err of
        [] -> do
            printUsage
            exitWith (ExitFailure 84)
        (err:errs) -> do
            putStr err
            printErrorArgument errs

{--
Print the usage of the program
--}
printUsage :: IO ()
printUsage = putStrLn "usage ici"