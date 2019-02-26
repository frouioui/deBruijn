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
    } deriving Show

startDeBruijn :: Int -> String -> DeBruijn
startDeBruijn order alphabet = DeBruijn
    { size          = order
    , str           = alphabet
    , index         = 1 -- PROBLEM: Maybe set this value to 2?
    , tempResult    = take order (repeat 0)
    , result        = []
    }

generation :: DeBruijn -> [Int]
generation debruijn
        | finish    = finalResult
        | otherwise = do
            myDebruijn <- checkExtendResultArray debruijn
            myDebruijn <- fillTempResult myDebruijn (index myDebruijn)
            myDebruijn <- changeIndexValueToSize myDebruijn
            myDebruijn <- moveIndexDecrementation myDebruijn
            case (index myDebruijn) of
                0           -> generation myDebruijn
                otherwise   -> generation myDebruijn { tempResult = incrementIndexList (tempResult myDebruijn) ((index myDebruijn)) }
        where
            finalResult     = (result debruijn)
            finish          = (index debruijn) <= 0

checkExtendResultArray :: DeBruijn -> [DeBruijn]
checkExtendResultArray d = do
    case (mod (size d) (index d) == 0) of
        True    -> [ d { result = (result d) ++ (fst (splitAt (index d) (tempResult d))) } ]
        False   -> [ d ]

moveIndexDecrementation :: DeBruijn -> [DeBruijn]
moveIndexDecrementation d
        | conditionDecrementation   = moveIndexDecrementation d { index = (index d) - 1 }
        | otherwise                 = [ d ]
        where
            conditionDecrementation = (index d) > 0 && ((tempResult d) !! ((index d) - 1)) >= ((size d) - 1)

changeIndexValueToSize :: DeBruijn -> [DeBruijn]
changeIndexValueToSize d = [d { index = (size d) }]

fillTempResult :: DeBruijn -> Int -> [DeBruijn]
fillTempResult d i
        | i < size d    = do
            let v = [ (tempResult d) !! (i - (index d)) ]
            let t = take (i + 1) (tempResult d)
            let a = (init t) ++ v
            let q = snd (splitArray (i + 1) (tempResult d))
            a <- [a ++ q]
            fillTempResult d { tempResult = a } (i + 1)
        | otherwise     = [d]

splitArray :: Int -> [Int] -> ([Int], [Int])
splitArray _ []   = ([], [])
splitArray i arr
        | length arr < i    = ([], [])
        | otherwise         = (splitAt i arr)

incrementIndexList :: [Int] -> Int -> [Int]
incrementIndexList [] i     = []
incrementIndexList arr i    = (init (fst (splitArray i arr))) ++ [((last (fst (splitArray i arr))) + 1)] ++ snd (splitArray i arr)