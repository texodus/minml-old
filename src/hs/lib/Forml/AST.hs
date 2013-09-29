--------------------------------------------------------------------------------

-- | Meta module, exporting the Types for Forml's abstract syntax tree.
--   There is some static info we need to define about our language.  The 
--   keywords and operators, arranged in precedence order.

--------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}

module Forml.AST (
    module Forml.AST.Type,
    module Forml.AST.Val,
    module Forml.AST.Patt,
    module Forml.AST.Expr,
    module Forml.AST.Record,
    module Forml.AST.Macro,
    Err(..),
    isFun,
    isInfix,
    keywords,
    ops,
) where

import Forml.AST.Type
import Forml.AST.Patt
import Forml.AST.Val
import Forml.AST.Expr
import Forml.AST.Record
import Forml.AST.Macro
import Forml.Utils

--------------------------------------------------------------------------------

-- | built-in keywords; TODO extract these to custom notations

keywords :: [String]
keywords = [ "let", "fun", "match", "with", "data" ]

-- | built-in operators.  Arranged in precedence order for parsing via
--   `buildExpressionParser`

ops :: [[String]]
ops = [ [ "" ]
      , [ "^" ]
      , [ "*", "/" ]
      , [ "+", "-" ]
      , [ "<", "<=", ">=", ">", "==", "!=" ]
      , [ "&&", "||" ] ]

-- | Error type

newtype Err = Err String deriving (Eq, Show, Ord)

instance Fmt Err where

    fmt (Err x) = "ERROR " ++ x

-- | Is type t a function type?

isFun :: Type t -> Maybe (Type (), Type ())
isFun (TypeApp (TypeApp (TypeSym (TypeSymP "->")) x) y) = Just (x, y)
isFun _ = Nothing

-- | Is an expression an application of an infix operator?`

isInfix :: Expr -> Maybe (Expr, String, Expr)
isInfix (AppExpr (AppExpr (VarExpr (SymVal (Sym o))) x) y) 
    | o `elem` concat ops  = Just (x, o, y)
isInfix _ = Nothing

--------------------------------------------------------------------------------