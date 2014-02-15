------------------------------------------------------------------------------

-- | Notation parsing

------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Minml.Macro.LetPatt where

import Control.Arrow
import Control.Applicative

import Minml.AST
import Minml.Replace

------------------------------------------------------------------------------

newtype LetPatt = LetPatt Patt

instance Replace LetPatt Expr where

    replace f (LetPatt p) (LetExpr (Sym f') a b) | f == f' =
        LetExpr (Sym "__match__") a (match <$> b)
        where
            match ex = MatExpr (VarExpr (SymVal (Sym "__match__"))) [(p, ex)]

    replace f ex (LetExpr f' a b) =
        LetExpr f' (replace f ex a) (replace f ex b)

    replace f ex (AnnExpr f' a b) =
        AnnExpr f' a (replace f ex b)

    replace f ex (AppExpr a b) =
        AppExpr (replace f ex a) (replace f ex b)

    replace _ _ (VarExpr x) =
        VarExpr x

    replace f ex (MatExpr e xs) =
        MatExpr (replace f ex e) (second (replace f ex) `fmap` xs)

    replace f ex (TypExpr a b e) =
        TypExpr a b $ replace f ex e

    replace f (LetPatt p) (AbsExpr (Sym f') ex) | f == f' =
        AbsExpr (Sym "__match__") match
        where
            match = MatExpr (VarExpr (SymVal (Sym "__match__"))) [(p, ex)]

    replace f x (AbsExpr y z) =
        AbsExpr y (replace f x z)

    replace f x (RecExpr xs) =
        RecExpr (fmap (replace f x) xs)

    replace _ _ (JSExpr y) =
        JSExpr y

------------------------------------------------------------------------------
