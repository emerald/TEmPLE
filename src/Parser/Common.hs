{-# LANGUAGE DeriveGeneric #-}

module Parser.Common
  ( ParseErrorImpl
  , fullParse, parse
  , commaList
  , prefix, prefixInfix
  , skipFilling, stoken, stoken1, stoken1Bool, token
  , word, word1
  , parseFile', parseString'
  ) where

import Data.Char (isSpace, toUpper, toLower)
import Data.List.NonEmpty (NonEmpty((:|)))
import Control.Applicative ((<*), (*>), liftA2)
import Control.Monad (void)
import Text.ParserCombinators.ReadP
  ( ReadP
  , eof, get, look
  , char, choice, eof, many, manyTill, munch1, option
  , pfail, satisfy
  , readP_to_S
  )
import Text.PrettyPrint.GenericPretty (Generic, Out)

data ParseErrorImpl a
  = NoParse FilePath
  | AmbiguousGrammar [a] FilePath
  | NotImplemented
  deriving (Eq, Generic, Ord, Show)

instance (Out a) => Out (ParseErrorImpl a)

string :: String -> ReadP String
-- ^ Case-insensitive variant of ReadP's string
string this = do s <- look; scan this s
 where
  scan []     _               = do return this
  scan (x:xs) (y:ys)
    | x == (toLower y) || x == (toUpper y)
    = do _ <- get; scan xs ys
  scan _      _               = do pfail

anyChar :: ReadP Char
anyChar = satisfy (\ _ -> True)

skipFilling :: ReadP ()
skipFilling =
  do s <- look
     skip s
 where
  skip ('%':_)           = (void $ manyTill anyChar (char '\n')) >> skipFilling
  skip (c:s) | isSpace c = do _ <- get; skip s
  skip _                 = do return ()

token :: ReadP a -> ReadP a
token = flip (<*) skipFilling

stoken :: String -> ReadP ()
stoken = void . token . string

stoken1 :: String -> ReadP ()
stoken1 s = string s >> choice
  [ munch1 isSpace >> skipFilling
  , eof
  ]

stoken1Bool :: String -> ReadP Bool
stoken1Bool s = option False ((stoken1 s) *> return True)

word :: [(String, a)] -> ReadP a
word = choice . map (\(w, a) -> stoken w *> return a)

word1 :: [(String, a)] -> ReadP a
word1 = choice . map (\(w, a) -> stoken1 w *> return a)

prefix :: Show w => (a -> b) -> w -> ReadP a -> ReadP b
prefix f w p = stoken1 (show w) *> fmap f p

prefixInfix :: Show w =>
  (a -> a -> b) -> w -> w -> ReadP a -> ReadP b
prefixInfix f w1 w2 p
  = stoken1 (show w1) *> fmap f p <* stoken1 (show w2) <*> p

commaList :: ReadP a -> ReadP (NonEmpty a)
commaList p = liftA2 (:|) p (many (stoken "," *> p))

parse :: ReadP a -> String -> [(a, String)]
parse = readP_to_S

fullParse :: ReadP a -> String -> [a]
fullParse p s = fmap fst $ parse (p <* eof) s

parseString' :: ReadP a -> FilePath -> String -> Either (ParseErrorImpl a) a
parseString' p path s =
  case fullParse p s of
    [] -> Left $ NoParse path
    [a] -> Right a
    as -> Left $ AmbiguousGrammar as path

parseFile' :: ReadP a -> FilePath -> IO (Either (ParseErrorImpl a) a)
parseFile' p path = fmap (parseString' p path) $ readFile path
