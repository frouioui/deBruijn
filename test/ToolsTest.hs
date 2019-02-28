module ToolsTest
    ( singleCharTest1
    , singleCharTest2
    , singleCharTest3
    , rotateTest1
    , rotateTest2
    , rotateTest3
    , isSimilar1
    , isSimilar2
    , isSimilar3
    , checkIsArrayContainsTrue1
    , checkIsArrayContainsTrue2
    , checkIsArrayContainsTrue3
    ) where

import Test.Tasty
import Test.Tasty.HUnit

import Tools

singleCharTest1 :: TestTree
singleCharTest1 = testCase "Valid string"
    (assertEqual "Should be True" True (singleChar "abc"))

singleCharTest2 :: TestTree
singleCharTest2 = testCase "Invalid string"
    (assertEqual "Should be False" False (singleChar "abcka"))

singleCharTest3 :: TestTree
singleCharTest3 = testCase "Empty string"
    (assertEqual "Should be True" True (singleChar ""))

rotateTest1 :: TestTree
rotateTest1 = testCase "No rotation"
    (assertEqual "Should be the same list" True ((rotate 0 [1, 2]) == [1, 2]))

rotateTest2 :: TestTree
rotateTest2 = testCase "Small rotation"
    (assertEqual "The list should be [2, 1]" True ((rotate 1 [1, 2]) == [2, 1]))

rotateTest3 :: TestTree
rotateTest3 = testCase "Bigger rotation"
    (assertEqual "The list shoud be [2, 3, 1]" True ((rotate 2 [1, 2, 3]) == [2, 3, 1]))

isSimilar1 :: TestTree
isSimilar1 = testCase "Similar same sequence"
    (assertEqual "The list is similar, the function said it was not" True (isSimilar [0,0,1,1] [0,0,1,1]))

isSimilar2 :: TestTree
isSimilar2 = testCase "Similar different sequence"
    (assertEqual "The list is similar, the function said it was not" True (isSimilar [0,0,1,1] [1,1,0,0]))

isSimilar3 :: TestTree
isSimilar3 = testCase "Not similar sequence"
    (assertEqual "The list is not similar, the function said it was" False (isSimilar [0,0,1,1] [0,1,0,1]))

checkIsArrayContainsTrue1 :: TestTree
checkIsArrayContainsTrue1 = testCase "List with only True index"
    (assertEqual "The list contains True" True (checkIfArrayContainsTrue [True, True, True, True]))

checkIsArrayContainsTrue2 :: TestTree
checkIsArrayContainsTrue2 = testCase "List with True and False index"
    (assertEqual "The list contains a True value" True (checkIfArrayContainsTrue [False, False, True, False]))

checkIsArrayContainsTrue3 :: TestTree
checkIsArrayContainsTrue3 = testCase "List with only False index"
    (assertEqual "The list does not contain True index" False (checkIfArrayContainsTrue [False, False, False, False]))
