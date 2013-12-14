--------------------------------------------------------------------------------

-- | Meta module, exporting the Types for Minml's abstract syntax tree.
--   There is some static info we need to define about our language.  The 
--   keywords and operators, arranged in precedence order.

--------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}

module Minml.AST (
    module Minml.AST.Err,
    module Minml.AST.Type,
    module Minml.AST.Val,
    module Minml.AST.Patt,
    module Minml.AST.Expr,
    module Minml.AST.Record,
    module Minml.AST.Macro,
    module Minml.AST.Meta,
    isFun,
    isInfix,
    keywords,
    ops,
) where

import Minml.AST.Err
import Minml.AST.Type
import Minml.AST.Patt
import Minml.AST.Val
import Minml.AST.Expr
import Minml.AST.Record
import Minml.AST.Macro
import Minml.AST.Meta

--------------------------------------------------------------------------------

-- | built-in keywords; TODO extract these to custom notations

keywords :: [String]
keywords = [ "match", "with", "data" ]

-- | built-in operators.  Arranged in precedence order for parsing via
--   `buildExpressionParser`

ops :: [[String]]
ops = [ [ "" ]
      , [ "^" ]
      , [ "*", "/" ]
      , [ "+", "-" ]
      , [ "<=", ">=", "==", "!=" ]
      , [ "&&", "||" ] ]

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