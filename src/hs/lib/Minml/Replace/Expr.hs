------------------------------------------------------------------------------

-- | Replace typeclass

------------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Minml.Replace.Expr where

import GHC.Generics
import Language.Javascript.JMacro

import Minml.AST
import Minml.Optimize
import Minml.Javascript.Expr ()
import Minml.Replace.Base
import Minml.Replace.Generic
import Minml.Replace.Patt    ()

------------------------------------------------------------------------------

instance Replace String Expr where

    replace s x =

        replace s (Sym x)
            . replace s (TypeVar (TypeVarP x))
            . replace s (ValPatt (SymVal (Sym x)))
            . replace s (VarExpr (SymVal (Sym x)))

instance Replace Expr Expr where

    replace f ex (VarExpr (SymVal (Sym f')))
        | f == f' = ex

    --replace f _ t @ (AnnExpr (Sym f') _ _)
    --    | f == f' = t

    replace f _  t @ (LetExpr (Sym f') _ _)
        | f == f' = t

    replace f _ (AbsExpr (Sym f') ex) | f == f' =
        AbsExpr (Sym f') ex

    replace f x (JSExpr y) =
        JSExpr (replace f (toJExpr $ optimize x) y)

    replace f x y =
        to . greplace (replace f x :: Expr -> Expr) . from $ y

instance Replace (Type ()) Expr where

    replace f ex (AnnExpr z (TypeVar (TypeVarP f')) cont) | f == f' =
        AnnExpr z ex (replace f ex cont)
        --to . greplace (replace f x :: Type () -> Type ()) . greplace (replace f x :: Expr -> Expr) . from $ y

    replace f x y =
        to . greplace (replace f x :: Expr -> Expr) . from $ y

instance Replace Sym Expr where

    replace f ex (LetExpr (Sym f') a b) | f == f' =
        LetExpr ex a b

    replace f ex (AnnExpr c a b) =
        AnnExpr (replace f ex c) a (replace f ex b)
        --AnnExpr ex a b -- (replace f (VarExpr (SymVal (Sym f))) b)

    replace f x (AbsExpr (Sym f') ex) | f == f' =
        AbsExpr x ex

    replace f x y =
        to . greplace (replace f x :: Expr -> Expr) . from $ y

instance Replace Patt Expr where

    replace f x y @ (MatExpr _ _) =
        to . greplace (replace f x :: Patt -> Patt) . greplace (replace f x :: Expr -> Expr) . from $ y

    replace f x y =
        to . greplace (replace f x :: Expr -> Expr) . from $ y

------------------------------------------------------------------------------
