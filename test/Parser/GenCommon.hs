module Parser.GenCommon
  ( spaces
  ) where

import Test.Tasty.QuickCheck
  ( Gen
  , elements, listOf
  )

spaces :: Gen String
spaces = listOf $ elements [' ', '\t', '\n', '\r', '\f', '\v']
