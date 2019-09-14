module Parser.Classic.Operations
  ( parseOperation
  ) where

import Ast (Operation(..), OpSig(..))

import qualified Parser.Classic.Words as W
  ( Keywords(End, Export) )

import Parser.Classic.OpSigs ( parseOpSig )
import Parser.Common ( stoken1, stoken1Bool )
import Parser.Types ( Parser, parseBlockBody )

import Control.Monad ( void )
import Text.ParserCombinators.ReadP ( ReadP )

parseOperation :: Parser -> ReadP Operation
parseOperation p = do
  export <- stoken1Bool (show W.Export)
  opsig <- parseOpSig p
  body <- parseBlockBody p
  let OpSig (_, name, _, _, _) = opsig
  void (stoken1 (show W.End) >> stoken1 name)
  return $ Operation (export, opsig, body)
