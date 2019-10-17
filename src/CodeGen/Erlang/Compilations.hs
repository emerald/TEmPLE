module CodeGen.Erlang.Compilations where

import Ast ( Compilation )
import CodeGen.Erlang.AbstractFormat
  ( Attr ( File, Module )
  , Form ( Attribute, Eof )
  , ModuleDecl ( ModuleDecl )
  )

import CodeGen.Erlang.Decls ( genConstDecl )

import System.FilePath ( dropExtension, takeFileName )

genCompilation :: FilePath -> Compilation -> ModuleDecl
genCompilation path cs = ModuleDecl $ header ++ body ++ [Eof]
  where
    filename :: FilePath
    filename = takeFileName path

    header :: [Form]
    header
      = [ Attribute $ File filename 0
        , Attribute $ Module $ dropExtension filename
        ]

    body :: [Form]
    body = map genConstDecl cs
