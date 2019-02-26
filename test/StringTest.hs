module StringTest
    ( singleCharTest1
    , singleCharTest2
    , singleCharTest3
    ) where

import Test.Tasty
import Test.Tasty.HUnit

import String

singleCharTest1 :: TestTree
singleCharTest1 = testCase "Valid string"
    (assertEqual "Should be True" True (singleChar "abc"))

singleCharTest2 :: TestTree
singleCharTest2 = testCase "Invalid string"
    (assertEqual "Should be False" False (singleChar "abcka"))

singleCharTest3 :: TestTree
singleCharTest3 = testCase "Empty string"
    (assertEqual "Should be True" True (singleChar ""))