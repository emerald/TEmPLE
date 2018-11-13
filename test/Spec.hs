import Parser.ClassicNamesTests (testTree)

import Test.Tasty (defaultMain)

main :: IO ()
main = testTree >>= defaultMain
