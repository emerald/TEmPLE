import qualified Parser.Classic.ReadP.Test.Idents as CIT
import qualified Parser.Classic.ReadP.Test.TextLits as CTLT
import qualified Parser.Classic.ReadP.Test.Lits as CLT
import qualified Parser.Classic.ReadP.Test.Types as CTT
import qualified Parser.Classic.ReadP.Test.Exprs as CET
import qualified Parser.Classic.ReadP.Test.Examples as CExT
import qualified Parser.Classic.ReadP.Test.ConstDecls as CCDT
import qualified Parser.Classic.ReadP.Test.VarDecls as CVDT
import qualified Parser.Classic.ReadP.Test.AssignOrInvoke as CAOIT
import qualified Parser.Classic.ReadP.Test.IfThenElse as CIFT
import qualified Parser.Classic.ReadP.Test.Objects as COT
import qualified Parser.Classic.ReadP.Test.Loops as CLOOPT
import qualified Parser.Classic.ReadP.Test.Decls as CDT
import qualified Parser.Classic.ReadP.Test.DeclStats as CDST

import qualified Parser.Classic.Megaparsec.Test.TextLits as CMTLT

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
  , CDST.testTree
  , CExT.testTree
  , CMTLT.testTree
  ]

main :: IO ()
main = testTree >>= defaultMain
