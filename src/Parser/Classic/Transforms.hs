-- |
-- Module   : TEmPLE.Parser.Classic.Transforms
-- Copyright   :  (c) Oleks Shturmov
-- License     :  BSD 3-Clause (see the file LICENSE)
--
-- Maintainer  :  oleks@oleks.info
--
-- Declares various syntactical transformations.

module Parser.Classic.Transforms
  ( classToObject
  ) where

import Ast
  ( Object(..), ObjectBody(..), Operation(Operation)
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
  , Class(Class)
  )

import Data.List.NonEmpty ( NonEmpty ((:|)) )

-- | Bar the baseclass, converts the class to an object. The baseclass
-- is simply ignored here (for now). TODO: Expand to take baseclass
-- into account.
classToObject :: Class -> Object
classToObject (Class (immutable, monitor, name, _, params, classOps, body)) =
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
          , params
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
