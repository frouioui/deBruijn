import Test.Tasty
import Test.Tasty.HUnit

import ToolsTest

main :: IO ()
main = do
    defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [tools]
