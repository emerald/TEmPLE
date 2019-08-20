module Parser.Common
  ( ParseErrorImpl
  , fullParse, parse
  , prefixInfix
  , skipFilling, stoken, stoken1, token, word
  , parseFile', parseString'
  ) where

import Data.Char (isSpace, toUpper, toLower)
import Control.Applicative ((<*), (*>))
import Control.Monad (void)
import Text.ParserCombinators.ReadP
  ( ReadP
  , get, look
  , char, choice, eof, manyTill, munch1, pfail, satisfy
  , readP_to_S
  )

data ParseErrorImpl a
  = NoParse FilePath
  | AmbiguousGrammar [a] FilePath
  | NotImplemented
  deriving (Eq, Show)

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
stoken1 s = string s >> munch1 isSpace >> skipFilling

word :: [(String, a)] -> ReadP a
word = choice . map (\(w, a) -> stoken w *> return a)

prefixInfix :: Show w =>
  (a -> a -> a) -> w -> w -> ReadP a -> ReadP a
prefixInfix f w1 w2 p
  = stoken1 (show w1) *> fmap f p <* stoken1 (show w2) <*> p

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
