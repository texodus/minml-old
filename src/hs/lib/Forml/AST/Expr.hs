------------------------------------------------------------------------------

-- Expressions.

-- Here, `AbsExpr` is an Abstraction, and `AppExpr` is an Application.

------------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverlappingInstances #-}

module Forml.AST.Expr (
    Expr( .. )
) where

import Language.Javascript.JMacro
import Text.PrettyPrint.Leijen.Text

import Forml.AST.Patt
import Forml.AST.Type
import Forml.AST.Val
import Forml.AST.Record
import Forml.Utils

------------------------------------------------------------------------------

data Expr where
    LetExpr :: Sym -> Expr -> Expr -> Expr
    AppExpr :: Expr -> Expr -> Expr
    AbsExpr :: Sym  -> Expr -> Expr
    VarExpr :: Val  -> Expr
    MatExpr :: Expr -> [(Patt, Expr)] -> Expr
    RecExpr :: Record Expr -> Expr
    JSExpr  :: JExpr -> Expr

    TypExpr :: TypeSym () -> TypeAbs () -> Expr -> Expr

    deriving (Eq, Ord, Show)

instance Fmt Expr where

    fmt (LetExpr binding ex cont) =
        "let " ++ fmt binding ++ " = " ++ fmt ex ++ "; " ++ fmt cont

    fmt (AppExpr f x) =
        "(" ++ fmt f ++ " " ++ fmt x ++ ")"

    fmt (AbsExpr x ret) =
        "(\\ " ++ fmt x ++ " = " ++ fmt ret ++ ")"

    fmt (MatExpr x cs) =
        "match " ++ fmt x ++ " with " ++ fmt cs

    fmt (TypExpr name def cont) =
        "data " ++ fmt name ++ " = " ++ fmt def ++ "; " ++ fmt cont

    fmt (JSExpr js) =
        "`" ++ (show . renderOneLine . renderJs) js ++ "`"

    fmt (RecExpr js) =
        fmt js

    fmt (VarExpr x) = fmt x

instance Fmt [(Patt, Expr)] where

    fmt ((patt, expr) : rs) =
        fmt patt ++ " -> " ++ fmt expr ++ "; " ++ fmt rs

    fmt [] = ""

------------------------------------------------------------------------------





