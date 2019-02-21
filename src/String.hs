module String
    (singleChar
    ) where

import Data.List
import Prelude

singleChar :: String -> Bool
singleChar xs = checkSingle sorted (drop 1 sorted)
    where
        sorted = sort xs
        checkSingle (x:xs') (y:ys)  = if x /= y then checkSingle xs' ys else False
        checkSingle [] _            = True
        checkSingle _ []            = True
