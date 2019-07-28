module Parser.Common where

import Data.Char (isSpace, toUpper, toLower)
import Control.Applicative ((<*), (*>))
import Control.Monad (void)
import Text.ParserCombinators.ReadP
  ( ReadP
  , get, look
  , choice, eof, munch1, pfail, skipSpaces
  , readP_to_S
  )

data ParseErrorImpl a
  = NoParse FilePath
  | AmbiguousGrammar [a] FilePath
  | NotImplemented
  deriving (Eq, Show)

string :: String -> ReadP String
string this = do s <- look; scan this s
 where
  scan []     _               = do return this
  scan (x:xs) (y:ys)
    | x == (toLower y) || x == (toUpper y)
    = do _ <- get; scan xs ys
  scan _      _               = do pfail

token :: ReadP a -> ReadP a
token = flip (<*) skipSpaces

stoken :: String -> ReadP ()
stoken = void . token . string

stoken1 :: String -> ReadP ()
stoken1 s = string s >> munch1 isSpace >> return ()

word :: [(String, a)] -> ReadP a
word = choice . map (\(w, a) -> string w *> return a)

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
