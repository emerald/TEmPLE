-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Oleks Shturmov
-- License     :  BSD 3-Clause (see the file LICENSE)
--
-- Maintainer  :  oleks@oleks.info
--
-- Declares various syntactical transformations.
-----------------------------------------------------------------------------
module Parser.Classic.ReadP.Transforms
  ( classToObject
  , makeConstField
  , makeVarField
  ) where

import Ast

import Data.List.NonEmpty ( NonEmpty ((:|)) )

makeConstField :: (Ident, Type, Expr) -> (Decl, [Operation])
makeConstField (i, t, e) =
  ( DConst $ Const (i, Just t, e)
  , [ Operation
      ( True
      , OpSig(Fun, "get" ++ i, [], [Param (False, Just "x", t)], [])
      , BlockBody
        ( [AssignOrInvoke $ AssignExpr (EVar "x" :| [], EVar i :| [])]
        , Nothing
        , Nothing
        )
      )
    ]
  )

makeVarField :: (Ident, Type, Maybe Expr) -> (Decl, [Operation])
makeVarField (i, t, me) =
  ( DVar $ Var (i :| [], t, me)
  , [ Operation
      ( True
      , OpSig (Fun, "get" ++ i, [], [Param (False, Just "x", t)], [])
      , BlockBody
        ( [AssignOrInvoke $ AssignExpr (EVar "x" :| [], EVar i :| [])]
        , Nothing
        , Nothing
        )
      )
    , Operation
      ( True
      , OpSig(Op, "set" ++ i, [Param (False, Just "x", t)], [], [])
      , BlockBody
        ( [AssignOrInvoke $ AssignExpr (EVar i :| [], EVar "x" :| [])]
        , Nothing
        , Nothing
        )
      )
    ]
  )

-- | Bar the baseclass, and builtin id, converts the class to an
-- object. The baseclass and builtin id is simply ignored here
-- (for now). TODO: Expand to take baseclass and builtin id into
-- account.
classToObject :: Class -> Object
classToObject (Class ((immutable, monitor), _, name, _, params, classOps, body)) =
  Object (immutable, monitor, Nothing, name, ObjectBody
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
                  Object (immutable, monitor, Nothing, innerName, body)) :| []
              )]
          , Nothing
          , Nothing
          )
        )

      ] ++ classOps
    , Nothing
    , Nothing
    , Nothing
    ))
  where
    typeName = name ++ "Type"
    getOps (ObjectBody (_, ops, _, _, _)) = ops
    getOpSig (Operation (_, opsig, _)) = opsig
    opSigs = map getOpSig $ getOps body
    innerName = "the" ++ name
