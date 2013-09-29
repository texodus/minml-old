------------------------------------------------------------------------------

-- | Expressions.

------------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Forml.AST.Expr (
    Expr(..)
) where

import Control.Arrow
import Language.Javascript.JMacro
import Text.PrettyPrint.Leijen.Text

import Forml.AST.Patt
import Forml.AST.Type
import Forml.AST.Val
import Forml.AST.Record
import Forml.AST.Replace
import Forml.Utils

------------------------------------------------------------------------------

-- | The Expression AST data type.  `App` is a function application; `Abs` is
--   function literal (function abstraction)

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

instance Replace String Expr where
    repGen g sym  = repGen g sym . VarExpr . SymVal . Sym
    replaceLet sym = replaceLet sym . VarExpr . SymVal . Sym

instance Replace Expr Expr where

    repGen _ f _  (LetExpr (Sym f') a b) | f == f' = 
        LetExpr (Sym f') a b

    repGen g f ex (LetExpr f' a b) =
        LetExpr f' (g f ex a) (g f ex b)

    repGen g f ex (AppExpr a b) =
        AppExpr (g f ex a) (g f ex b)

    repGen _ f ex (VarExpr (SymVal (Sym f'))) | f == f' =
        ex

    repGen _ _ _  (VarExpr x) =
        VarExpr x

    repGen g f ex (MatExpr e xs) =
        MatExpr (g f ex e) (map (second (g f ex)) xs)

    repGen g f ex (TypExpr a b e) =
        TypExpr a b $ g f ex e

    repGen _ f _ (AbsExpr (Sym f') _) | f == f' = undefined
    repGen _ _ _ (AbsExpr _ _) = undefined
    repGen _ _ _ (RecExpr _)   = undefined
    repGen _ _ _ (JSExpr _)    = undefined

    replaceLet f t @ (VarExpr (SymVal f'')) (LetExpr (Sym f') a b) | f == f' =
        LetExpr f'' (replaceLet f t a) (replaceLet f t b)

    replaceLet f g h = repGen replaceLet f g h

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





