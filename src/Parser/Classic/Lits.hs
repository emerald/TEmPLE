module Parser.Classic.Lits
  ( parseLit
  ) where

import Ast (Lit(LNil, LBool, LSelf, LObj, LTypeObj, LClass, LEnum))

import Parser.Common (stoken1Bool, word)
import Parser.Classic.Classes ( parseClass )
import Parser.Classic.Enums (parseEnum)
import Parser.Classic.NumLits (parseNumLit)
import Parser.Classic.Objects ( parseObject )
import Parser.Classic.Records (parseRecord)
import Parser.Classic.TextLits (parseTextLit)
import Parser.Classic.VecLits (parseVecLit)
import Parser.Types (Parser, parseTypeObject)

import qualified Parser.Classic.Words as W
  ( Literals(..), Keywords(Immutable, Monitor) )

import Text.ParserCombinators.ReadP (ReadP, choice)

parseNil :: ReadP Lit
parseNil = word [(show W.Nil, LNil)]

parseSelf :: ReadP Lit
parseSelf = word [(show W.Self, LSelf)]

parseBool :: ReadP Lit
parseBool = word
  [ (show W.True,  LBool True  )
  , (show W.False, LBool False )
  ]

parseLit :: Parser -> ReadP Lit
parseLit p = choice
  [ parseNil
  , parseSelf
  , parseBool
  , parseNumLit
  , parseTextLit
  , parseVecLit p
  , parseOptImmLit p
  , fmap LEnum $ parseEnum
  ]

parseOptImmLit :: Parser -> ReadP Lit
parseOptImmLit p = do
  imm <- stoken1Bool (show W.Immutable)
  choice
    [ fmap LTypeObj $ parseTypeObject p imm
    , fmap LClass $ parseRecord imm
    , parseOptMonitorLit p imm
    ]

parseOptMonitorLit :: Parser -> Bool -> ReadP Lit
parseOptMonitorLit p imm = do
  mon <- stoken1Bool (show W.Monitor)
  choice
    [ fmap LObj $ parseObject p imm mon
    , fmap LClass $ parseClass p imm mon
    ]
