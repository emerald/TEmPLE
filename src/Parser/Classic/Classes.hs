module Parser.Classic.Classes
  ( parseClass
  ) where

import Ast
  ( Ident, Object(..), ObjectBody(..), Operation(Operation)
  , Expr(ELit, EVar), DeclStat(AssignOrInvoke)
  , AssignOrInvoke(AssignExpr)
  , Type(TIdent)
  , Param(Param)
  , TypeObject(TypeObject)
  , OpKind(Op, Fun)
  , OpSig(OpSig)
  , BlockBody(BlockBody)
  , Lit(LObj, LTypeObj)
  , ConstDecl(Const)
  , Decl(DConst)
  )
import Parser.Common ( stoken1, stoken1Bool )
import Parser.Classic.Idents ( parseIdent )
import Parser.Classic.Operations ( parseOperation )
import Parser.Types ( Parser, parseObjectBody )

import qualified Parser.Classic.Words as W
  ( Keywords( Class, End, Immutable, Monitor ) )

import Data.List.NonEmpty ( NonEmpty ((:|)) )
import Control.Monad ( void )
import Text.ParserCombinators.ReadP ( ReadP, many )

parseClass :: Bool -> Bool -> Parser -> ReadP Object
parseClass immutable monitor p = do
  name <- (stoken1 (show W.Class) *> parseIdent)
  classOps <- many $ (stoken1 (show W.Class) *> parseOperation p)
  body <- parseObjectBody p
  void (stoken1 (show W.End) >> stoken1 name)
  return $ classToObject (immutable, monitor, name, classOps, body)

classToObject :: (Bool, Bool, Ident, [Operation], ObjectBody) -> Object
classToObject (immutable, monitor, name, classOps, body) =
  Object immutable monitor name $ ObjectBody
    ( [ ( False
        , DConst $ Const
          ( typeName
          , Nothing
          , ELit $ LTypeObj $ TypeObject
            ( immutable, Nothing, typeName, opSigs )
          )
        )
      ]
    , [ Operation
        ( True
        , OpSig
          ( Fun
          , "getSignature"
          , []
          , [Param (False, Just "r", TIdent "Signature")]
          , []
          )
        , BlockBody
          ( [AssignOrInvoke $ AssignExpr
              (EVar "x" :| [], EVar typeName :| [])]
          , Nothing
          , Nothing
          )
        )
      , Operation
        ( True
        , OpSig
          ( Op
          , "create"
          , []
          , [Param (False, Just "e", TIdent name)]
          , []
          )
        , BlockBody
          ( [AssignOrInvoke $ AssignExpr
              ( EVar "e" :| []
              , ELit (LObj $
                  Object immutable monitor innerName body) :| []
              )]
          , Nothing
          , Nothing
          )
        )

      ] ++ classOps
    , Nothing
    , Nothing
    , Nothing
    )
  where
    typeName = name ++ "Type"
    getOps (ObjectBody (_, ops, _, _, _)) = ops
    getOpSig (Operation (_, opsig, _)) = opsig
    opSigs = map getOpSig $ getOps body
    innerName = "the" ++ name
