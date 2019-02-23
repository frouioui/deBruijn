import Test.Tasty
import Test.Tasty.HUnit

import StringTest

main :: IO ()
main = do
    defaultMain (testGroup "String test" [singleCharTest1, singleCharTest2, singleCharTest3])
