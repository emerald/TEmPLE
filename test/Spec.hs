import qualified Parser.Classic.Test.Idents as CIT
import qualified Parser.Classic.Test.TextLits as CTLT
import qualified Parser.Classic.Test.Lits as CLT
import qualified Parser.Classic.Test.Types as CTT
import qualified Parser.Classic.Test.Exprs as CET
import qualified Parser.Classic.Test.ConstDecls as CCDT
import qualified Parser.Classic.Test.VarDecls as CVDT

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
