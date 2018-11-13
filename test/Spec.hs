import qualified Parser.ClassicNamesTests as CNT
import qualified Parser.ClassicExprsTests as CET

import Test.Tasty (TestTree, defaultMain, testGroup)

testTree :: IO TestTree
testTree = fmap (testGroup "Classic Parser Tests") $ sequence
  [ CNT.testTree
  , CET.testTree
  ]

main :: IO ()
main = testTree >>= defaultMain
