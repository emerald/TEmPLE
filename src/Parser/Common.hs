module Parser.Common where

import Data.Char (isSpace)
import Control.Applicative ((<*), (*>))
import Control.Monad (void)
import Text.ParserCombinators.ReadP

data ParseErrorImpl a
  = NoParse FilePath
  | AmbiguousGrammar [a] FilePath
  | NotImplemented
  deriving (Eq, Show)

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
