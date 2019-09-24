module Parser.Classic.Objects
  ( parseObject
  , parseObjectBody
  ) where

import Ast (Object(..), ObjectBody(..), BlockBody, Operation)
import Parser.Common (stoken1)
import Parser.Classic.Builtins ( parseBuiltin )
import Parser.Classic.Common ( end, endShow )
import Parser.Classic.Decls ( parseObjConstrDecl )
import Parser.Classic.Idents (prefixedIdent)
import Parser.Classic.BlockBody (parseBlockBody)
import Parser.Classic.Operations (parseOperation)
import Parser.Types (Parser)

import qualified Parser.Classic.Words as W
  ( Keywords(Object, Initially, Process, Recovery) )

import Control.Applicative ( optional )
import Text.ParserCombinators.ReadP
  ( ReadP
  , choice, many
  , pfail
  )

parseObject :: Parser -> Bool -> Bool -> ReadP Object
parseObject p imm mon = do
  name <- prefixedIdent W.Object
  builtin <- optional $ parseBuiltin
  body <- parseObjectBody p
  end name
  return $ Object imm mon builtin name body

parseObjectBody :: Parser -> ReadP ObjectBody
parseObjectBody p = do
  (decls, declOps) <- fmap unzip $ many (parseObjConstrDecl p)
  (initially, process, recovery, ops) <- parseTail
    ( parseInitially p
    , parseProcess p
    , parseRecovery p
    , parseOperation p
    , (Nothing, Nothing, Nothing, [])
    )
  let allOps = concat declOps ++ reverse ops
  return $ ObjectBody (decls, allOps, initially, process, recovery)

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
  endShow w

parseInitially :: Parser -> ReadP BlockBody
parseInitially = parseBlock W.Initially

parseProcess :: Parser -> ReadP BlockBody
parseProcess = parseBlock W.Process

parseRecovery :: Parser -> ReadP BlockBody
parseRecovery = parseBlock W.Recovery
