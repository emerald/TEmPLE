import qualified Parser.Classic.Test.Idents as CIT
import qualified Parser.Classic.Test.TextLits as CTLT
import qualified Parser.Classic.Test.Lits as CLT
import qualified Parser.Classic.Test.Types as CTT
import qualified Parser.Classic.Test.Exprs as CET
import qualified Parser.Classic.Test.ConstDecls as CCDT
import qualified Parser.Classic.Test.VarDecls as CVDT
import qualified Parser.Classic.Test.AssignOrInvoke as CAOIT
import qualified Parser.Classic.Test.IfThenElse as CIFT
import qualified Parser.Classic.Test.Objects as COT
import qualified Parser.Classic.Test.Loops as CLOOPT
import qualified Parser.Classic.Test.Decls as CDT

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
  , CAOIT.testTree
  , CIFT.testTree
  , COT.testTree
  , CLOOPT.testTree
  , CDT.testTree
  ]

main :: IO ()
main = testTree >>= defaultMain
