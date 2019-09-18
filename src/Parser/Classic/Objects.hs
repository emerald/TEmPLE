module Parser.Classic.Objects
  ( parseObject
  ) where

import Ast (Object(..), BlockBody, Operation)
import Parser.Common (stoken, stoken1, stoken1Bool)
import Parser.Classic.Idents (parseIdent)
import Parser.Classic.BlockBody (parseBlockBody)
import Parser.Classic.Operations (parseOperation)
import Parser.Types (Parser, parseAttDecl)

import qualified Parser.Classic.Words as W
  ( Keywords(Object, End, Initially, Immutable, Monitor, Process, Recovery) )

import Control.Monad (void)
import Text.ParserCombinators.ReadP
  ( ReadP
  , choice, many
  , pfail
  )

parseObject :: Parser -> ReadP Object
parseObject p = do
  immutable <- stoken1Bool (show W.Immutable)
  monitor <- stoken1Bool (show W.Monitor)
  name <- (stoken1 (show W.Object) *> parseIdent)
  (decls, declOps) <- fmap unzip $ many (parseAttDecl p)
  (initially, process, recovery, ops) <- parseTail
    ( parseInitially p
    , parseProcess p
    , parseRecovery p
    , parseOperation p
    , (Nothing, Nothing, Nothing, [])
    )
  void (stoken1 (show W.End) >> stoken name)
  return $ Object immutable monitor
    name decls (concat declOps ++ (reverse ops))
    initially process recovery

data ObjectTail
  = Initially BlockBody
  | Process BlockBody
  | Recovery BlockBody
  | Operation Operation
  | NoTail
  deriving (Eq, Ord)

parseTail :: (ReadP BlockBody, ReadP BlockBody, ReadP BlockBody, ReadP Operation,
              (Maybe BlockBody, Maybe BlockBody, Maybe BlockBody, [Operation]))
             -> ReadP (Maybe BlockBody, Maybe BlockBody, Maybe BlockBody, [Operation])
parseTail (p1, p2, p3, po, r @ (r1, r2, r3, ro)) = do
  b <- choice
    [ fmap Initially  p1
    , fmap Process    p2
    , fmap Recovery   p3
    , fmap Operation  po
    , return NoTail
    ]
  case b of
    (Initially  b') -> parseTail (pfail, p2, p3, po, (Just b', r2, r3, ro))
    (Process    b') -> parseTail (p1, pfail, p3, po, (r1, Just b', r3, ro))
    (Recovery   b') -> parseTail (p1, p2, pfail, po, (r1, r2, Just b', ro))
    (Operation  b') -> parseTail (p1, p2, p3, po,    (r1, r2, r3, b':ro))
    NoTail          -> return r

parseBlock :: Show w => w -> Parser -> ReadP BlockBody
parseBlock w p =
  stoken1 (show w) *>
  parseBlockBody p <*
  stoken1 (show W.End) <*
  stoken1 (show w)

parseInitially :: Parser -> ReadP BlockBody
parseInitially = parseBlock W.Initially

parseProcess :: Parser -> ReadP BlockBody
parseProcess = parseBlock W.Process

parseRecovery :: Parser -> ReadP BlockBody
parseRecovery = parseBlock W.Recovery
