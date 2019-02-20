module Argument
    ( Options(..),
    Flag(..),
    handleArgument
    ) where

{--
Imports
--}
import System.Environment(getArgs)
import System.Console.GetOpt

{--
Declaration of the Args datatype, this datatype
represent the program's arguments
--}
data Options = Options
    {
        helper :: Bool,
        version :: Bool
    } deriving Show

{--
Declaration of the Flag datatype used for GetOpt
--}
data Flag = Version
            | Helper

{--
Hold the default value of the Options datatype
--}
startOption :: Options
startOption = Options
    { helper = False,
    version = False
    }

options :: [ OptDescr (Options -> IO Options) ]
options =
    [
        Option ['v'] ["version"] (NoArg (\opt -> return opt { version = True } )) "Print the version of the program",
        Option ['h'] ["help"] (NoArg (\opt -> return opt { helper = True } )) "Print the usage of the program"
    ]

handleArgument :: IO (Options)
handleArgument = do
    argv <- getArgs
    let (actions, nonOptions, errors) = getOpt RequireOrder options argv
    opts <- foldl (>>=) (return startOption) actions
    return opts