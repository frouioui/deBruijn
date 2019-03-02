module ToolsTest
    ( tools
    ) where

import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Test.Tasty

import Generation
import Tools

tools :: TestTree
tools = testGroup "Tools" [ singleCharTest,
                            rotateTest,
                            isSimilarTest,
                            checkIsArrayContainsTrueTest,
                            getWordsInArrayTest,
                            checkDeBruijnTest,
                            isInAlphabetTest
                        ]


singleCharTest :: TestTree
singleCharTest = testGroup "singleChar" [singleCharTest1, singleCharTest2, singleCharTest3]

singleCharTest1 :: TestTree
singleCharTest1 = testCase "Valid string"
    (assertEqual "Should be True" True (singleChar "abc"))

singleCharTest2 :: TestTree
singleCharTest2 = testCase "Invalid string"
    (assertEqual "Should be False" False (singleChar "abcka"))

singleCharTest3 :: TestTree
singleCharTest3 = testCase "Empty string"
    (assertEqual "Should be True" True (singleChar ""))


rotateTest :: TestTree
rotateTest = testGroup "rotate" [rotateTest1, rotateTest2, rotateTest3]

rotateTest1 :: TestTree
rotateTest1 = testCase "No rotation"
    (assertEqual "Should be the same list" [1, 2] (rotate 0 [1, 2]))

rotateTest2 :: TestTree
rotateTest2 = testCase "Small rotation"
    (assertEqual "The list should be [2, 1]" [2, 1] (rotate 1 [1, 2]))

rotateTest3 :: TestTree
rotateTest3 = testCase "Bigger rotation"
    (assertEqual "The list shoud be [2, 3, 1]" [2, 3, 1] (rotate 2 [1, 2, 3]))


isSimilarTest :: TestTree
isSimilarTest = testGroup "isSimilar" [isSimilarTest1, isSimilarTest2, isSimilarTest3]

isSimilarTest1 :: TestTree
isSimilarTest1 = testCase "Similar same sequence"
    (assertEqual "The list is similar, the function said it was not" True (isSimilar [0,0,1,1] [0,0,1,1]))

isSimilarTest2 :: TestTree
isSimilarTest2 = testCase "Similar different sequence"
    (assertEqual "The list is similar, the function said it was not" True (isSimilar [0,0,1,1] [1,1,0,0]))

isSimilarTest3 :: TestTree
isSimilarTest3 = testCase "Not similar sequence"
    (assertEqual "The list is not similar, the function said it was" False (isSimilar [0,0,1,1] [0,1,0,1]))


checkIsArrayContainsTrueTest :: TestTree
checkIsArrayContainsTrueTest = testGroup "checkIsArrayContainsTrue" [checkIsArrayContainsTrueTest1, checkIsArrayContainsTrueTest2, checkIsArrayContainsTrueTest3]

checkIsArrayContainsTrueTest1 :: TestTree
checkIsArrayContainsTrueTest1 = testCase "List with only True index"
    (assertEqual "The list contains True" True (checkIfArrayContainsTrue [True, True, True, True]))

checkIsArrayContainsTrueTest2 :: TestTree
checkIsArrayContainsTrueTest2 = testCase "List with True and False index"
    (assertEqual "The list contains a True value" True (checkIfArrayContainsTrue [False, False, True, False]))

checkIsArrayContainsTrueTest3 :: TestTree
checkIsArrayContainsTrueTest3 = testCase "List with only False index"
    (assertEqual "The list does not contain True index" False (checkIfArrayContainsTrue [False, False, False, False]))


getWordsInArrayTest :: TestTree
getWordsInArrayTest = testGroup "getWordsInArray" [getWordsInArrayTest1, getWordsInArrayTest2, getWordsInArrayTest3, getWordsInArrayTest4]

getWordsInArrayTest1 :: TestTree
getWordsInArrayTest1 = testCase "Simple list with a size of 1"
    (assertEqual "The list should be [[1], [2], [3], [4]]" [[1], [2], [3], [4]] ((getWordsInArray 1 [1, 2, 3, 4])))

getWordsInArrayTest2 :: TestTree
getWordsInArrayTest2 = testCase "Simple list with a size of 2"
    (assertEqual "The list should be [[1, 2], [2, 3], [3, 4], [4, 1]]" [[1, 2], [2, 3], [3, 4], [4, 1]] ((getWordsInArray 2 [1, 2, 3, 4])))

getWordsInArrayTest3 :: TestTree
getWordsInArrayTest3 = testCase "Simple list with a size of 3"
    (assertEqual "The list should be [[1, 2, 3], [2, 3, 4], [3, 4, 1], [4, 1, 2]]" [[1, 2, 3], [2, 3, 4], [3, 4, 1], [4, 1, 2]] ((getWordsInArray 3 [1, 2, 3, 4])))

getWordsInArrayTest4 :: TestTree
getWordsInArrayTest4 = testCase "Longer list of size 2"
    (assertEqual "The list should be [[1, 2], [2, 3], [3, 4], [4, 5], [5, 6], [6, 1]]" [[1, 2], [2, 3], [3, 4], [4, 5], [5, 6], [6, 1]] ((getWordsInArray 2 [1, 2, 3, 4, 5, 6])))


checkDeBruijnTest :: TestTree
checkDeBruijnTest = testGroup "checkDeBruijn" [checkDeBruijnTest1, checkDeBruijnTest2, checkDeBruijnTest3, checkDeBruijnTest4, checkDeBruijnTest5, checkDeBruijnTest6, checkDeBruijnTest7, checkDeBruijnTest8]

checkDeBruijnTest1 :: TestTree
checkDeBruijnTest1 = testCase "Small valid sequence"
    (assertEqual "The sequence is valid but the function said it was not" True (checkDeBruijn 2 "01" (getStringFromGeneration (generation (startDeBruijn 2 "01")) "01")))

checkDeBruijnTest2 :: TestTree
checkDeBruijnTest2 = testCase "Bigger valid sequence"
    (assertEqual "The sequence is valid but the function said it was not" True (checkDeBruijn 3 "01" "00010111"))

checkDeBruijnTest3 :: TestTree
checkDeBruijnTest3 = testCase "Huge valid sequence"
    (assertEqual "The sequence is valid but the function said it was not" True (checkDeBruijn 10 "49" (getStringFromGeneration (generation (startDeBruijn 10 "49")) "49")))

checkDeBruijnTest4 :: TestTree
checkDeBruijnTest4 = testCase "Huge 2 valid sequence"
    (assertEqual "The sequence is valid but the function said it was not" True (checkDeBruijn 8 "901" (getStringFromGeneration (generation (startDeBruijn 8 "901")) "901")))

checkDeBruijnTest5 :: TestTree
checkDeBruijnTest5 = testCase "Small invalid sequence"
    (assertEqual "The sequence is invalid but the function said it was" False (checkDeBruijn 2 "02" (getStringFromGeneration (generation (startDeBruijn 2 "01")) "01")))

checkDeBruijnTest6 :: TestTree
checkDeBruijnTest6 = testCase "Bigger invalid sequence"
    (assertEqual "The sequence is invalid but the function said it was" False (checkDeBruijn 3 "02" (getStringFromGeneration (generation (startDeBruijn 3 "01")) "01")))

checkDeBruijnTest7 :: TestTree
checkDeBruijnTest7 = testCase "Huge invalid sequence"
    (assertEqual "The sequence is invalid but the function said it was" False (checkDeBruijn 10 "47" (getStringFromGeneration (generation (startDeBruijn 10 "49")) "49")))

checkDeBruijnTest8 :: TestTree
checkDeBruijnTest8 = testCase "Huge 2 invalid sequence"
    (assertEqual "The sequence is invalid but the function said it was" False (checkDeBruijn 8 "902" (getStringFromGeneration (generation (startDeBruijn 8 "901")) "901")))


isInAlphabetTest :: TestTree
isInAlphabetTest = testGroup "isInAlphabet" [isInAlphabetTest1, isInAlphabetTest2]

isInAlphabetTest1 :: TestTree
isInAlphabetTest1 = testCase "Letter in alphabet"
    (assertEqual "The function should return True" True (isInAlphabet 'c' "abc"))

isInAlphabetTest2 :: TestTree
isInAlphabetTest2 = testCase "Letter not in alphabet"
    (assertEqual "The function should return False" False (isInAlphabet 'o' "abc"))
