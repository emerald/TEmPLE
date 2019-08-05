import qualified Parser.ClassicIdentsTests as CIT
import qualified Parser.ClassicTextLitsTests as CTLT
import qualified Parser.ClassicLitsTests as CLT
import qualified Parser.ClassicTypesTests as CTT
import qualified Parser.ClassicExprsTests as CET
import qualified Parser.ClassicConstDeclsTests as CCDT
import qualified Parser.ClassicVarDeclsTests as CVDT

import Test.Tasty (TestTree, defaultMain, testGroup)

testTree :: IO TestTree
testTree = fmap (testGroup "Classic Parser Tests") $ sequence
  [ CIT.testTree
  , CTLT.testTree
  , CLT.testTree
  , CTT.testTree
  , CET.testTree
  , CCDT.testTree
  , CVDT.testTree
  ]

main :: IO ()
main = testTree >>= defaultMain
