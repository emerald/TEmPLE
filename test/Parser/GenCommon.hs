module Parser.GenCommon
  ( spaces
  , token
  ) where

import Test.Tasty.QuickCheck
  ( Gen
  , elements, listOf
  )

spaces :: Gen String
spaces = listOf $ elements [' ', '\t', '\n', '\r', '\f', '\v']

token :: String -> Gen String
token s = fmap (s ++) spaces
