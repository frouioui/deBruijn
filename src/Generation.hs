module Generation
    ( generation,
    DeBruijn(..),
    startDeBruijn
    ) where

data DeBruijn = DeBruijn
    { size          :: Int
    , str           :: String
    , index         :: Int
    , tempResult    :: [Int]
    , result        :: [Int]
    }

startDeBruijn :: Int -> String -> DeBruijn
startDeBruijn order alphabet = DeBruijn
    { size          = order
    , str           = alphabet
    , index         = 1
    , tempResult    = take order (repeat 0)
    , result        = []
    }

generation :: DeBruijn -> [Int]
generation debruijn
        | finish    = finalResult
        | otherwise = generation debruijn { index = ((index debruijn) - 1) }
        where
            finalResult = (result debruijn)
            finish      = (index debruijn) <= 0

