module Parser.Classic.ReadP.Lits
  ( parseLit
  ) where

import Ast ( Lit ( LNil, LBool, LSelf, LObj, LTypeObj, LClass, LEnum ) )

import Parser.Classic.ReadP.Classes ( parseClass )
import Parser.Classic.ReadP.Enums ( parseEnum )
import Parser.Classic.ReadP.NumLits ( parseNumLit )
import Parser.Classic.ReadP.Objects ( parseObject )
import Parser.Classic.ReadP.Records ( parseRecord )
import Parser.Classic.ReadP.TextLits ( parseTextLit )
import Parser.Classic.ReadP.TypeObjects ( parseTypeObject )
import Parser.Classic.ReadP.VecLits ( parseVecLit )
import Parser.Types ( Parser )
import Parser.Utils.ReadP ( stoken1Bool, word )

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
