import qualified Parser.ClassicNamesTests as CNT
import qualified Parser.ClassicTypesTests as CTT
import qualified Parser.ClassicExprsTests as CET
import qualified Parser.ClassicConstDeclsTests as CCDT

import Test.Tasty (TestTree, defaultMain, testGroup)

testTree :: IO TestTree
testTree = fmap (testGroup "Classic Parser Tests") $ sequence
  [ CNT.testTree
  , CTT.testTree
  , CET.testTree
  , CCDT.testTree
  ]

main :: IO ()
main = testTree >>= defaultMain
