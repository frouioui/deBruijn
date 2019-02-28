import Test.Tasty
import Test.Tasty.HUnit

import ToolsTest

main :: IO ()
main = do
    defaultMain (testGroup "String test"    [ singleCharTest1
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
                                            ])
