{-# LANGUAGE DeriveGeneric #-}

module Parser.Common
  ( ParseErrorImpl
  , fullParse, parse
  , optCommaList, commaList
  , inBrackets
  , prefix, prefixInfix
  , skipFilling
  , stoken, stoken1, stoken1Bool
  , token, token1
  , word, word1
  , parseFile', parseString'
  ) where

import Parser.ParserM
import qualified Parser.CommonPrime as P

import Data.List.NonEmpty (NonEmpty((:|)), toList)
import Control.Applicative ((<*), (*>), liftA2)
import Control.Monad (void)
import Control.Monad.Combinators ( between, choice, many, option )
import Text.PrettyPrint.GenericPretty (Generic, Out)
import qualified Text.ParserCombinators.ReadP as R ( eof )

data ParseErrorImpl e a
  = NoParse FilePath
  | AmbiguousGrammar [Either e a] FilePath
  | NotImplemented
  | SyntaxError e
  deriving (Eq, Generic, Ord, Show)

instance (Out e, Out a) => Out (ParseErrorImpl e a)

data BasicError
  = ExpectedString String
  | ExpectedOneOrMoreWhiteSpace
  | ExpectedEOF
  deriving (Eq, Generic, Ord, Show)

instance Out BasicError

eof :: ParserM BasicError ()
eof = liftRP R.eof ExpectedEOF

string :: String -> ParserM BasicError String
-- ^ Demand that the given string be parsed
string this = liftRP (P.string this) (ExpectedString this)

skipFilling :: ParserM e ()
skipFilling = liftRP' P.skipFilling

skip1Filling :: ParserM e ()
skip1Filling = liftRP' P.skip1Filling

token :: ParserM e a -> ParserM e a
-- ^ Skip comments and whitespace after token
token = flip (<*) skipFilling

token1 :: ParserM BasicError a -> ParserM BasicError a
-- ^ Skip at least one comment or whitespace after token
token1 = flip (<*) $ choice
  [ skip1Filling
  , eof
  , pfail ExpectedOneOrMoreWhiteSpace
  ]

stoken :: String -> ParserM BasicError ()
-- ^ Skip comments and whitespace after string token
stoken = void . token . string

stoken1 :: String -> ParserM BasicError ()
-- ^ Skip at least one comment or whitespace after string token
stoken1 = void . token1 . string

stoken1Bool :: String -> ParserM BasicError Bool
stoken1Bool s = option False ((stoken1 s) *> return True)

word :: [(String, a)] -> ParserM BasicError a
word = choice . map (\(w, a) -> stoken w *> return a)

word1 :: [(String, a)] -> ParserM BasicError a
word1 = choice . map (\(w, a) -> stoken1 w *> return a)

prefix :: Show w => (a -> b) -> w -> ParserM BasicError a -> ParserM BasicError b
prefix f w p = stoken1 (show w) *> fmap f p

prefixInfix :: Show w =>
  (a -> a -> b) -> w -> w -> ParserM BasicError a -> ParserM BasicError b
prefixInfix f w1 w2 p
  = stoken1 (show w1) *> fmap f p <* stoken1 (show w2) <*> p

commaList :: ParserM BasicError a -> ParserM BasicError (NonEmpty a)
commaList p = liftA2 (:|) p (many (stoken "," *> p))

optCommaList :: ParserM BasicError a
  -> (ParserM BasicError [a] -> ParserM BasicError [a])
  -> ParserM BasicError [a]
optCommaList p opt = choice
  [ opt (fmap toList $ commaList p)
  , return []
  ]

inBrackets :: ParserM BasicError a -> ParserM BasicError a
inBrackets = between (stoken "[") (stoken "]")

fullParse :: ParserM e a -> String -> [Either e a]
fullParse p s = fmap fst $ parse (p <* eof) s

parseString' :: ParserM e a -> FilePath -> String
  -> Either (ParseErrorImpl e a) a
parseString' p path s =
  case fullParse p s of
    [] -> Left $ NoParse path
    [Right a] -> Right a
    [Left e] -> Left $ SyntaxError e
    as -> Left $ AmbiguousGrammar as path

parseFile' :: ParserM e a -> FilePath
  -> IO (Either (ParseErrorImpl e a) a)
parseFile' p path = fmap (parseString' p path) $ readFile path
