------------------------------------------------------------------------------

-- | Replace typeclass

------------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Forml.Parse.Replace(
	module Forml.AST.Replace
) where


import Control.Arrow
import Language.Javascript.JMacro

import Forml.AST.Replace
import Forml.AST
import Forml.Javascript.Expr()

------------------------------------------------------------------------------

instance Replace Expr JExpr where
 
    replace s x = replace s (toJExpr x)

instance Replace Sym JExpr where
 
    replace s x = replace s (toJExpr (VarExpr (SymVal x)))


instance Replace String Expr where

    -- Lets are special cases to handle recursion

    replace f t (LetExpr (Sym f') a b) | f == f' =
        LetExpr (Sym t) (replace f t . replace f (VarExpr (SymVal (Sym t))) $ a)
                    (replace f t . replace f (VarExpr (SymVal (Sym t))) $ b)

    --replace sym patt (LetExpr (Sym sym') a b) | sym == sym' =
    --    MatExpr (replace sym patt a) [(patt, replace sym patt b)]

    replace f t (AbsExpr (Sym f') ex) | f == f' =
        AbsExpr (Sym t) (replace f t . replace f (VarExpr (SymVal (Sym t))) $ ex)

    replace f _  (LetExpr (Sym f') a b) | f == f' =
        LetExpr (Sym f') a b

    replace f ex (LetExpr f' a b) =
        LetExpr f' (replace f ex a) (replace f ex b)

    replace f ex (AppExpr a b) =
        AppExpr (replace f ex a) (replace f ex b)

    replace f t (VarExpr (SymVal (Sym f'))) | f == f' =
        VarExpr (SymVal (Sym t))

    replace _ _  (VarExpr x) =
        VarExpr x

    replace f ex (MatExpr e xs) =
        MatExpr (replace f ex e) (fmap (second (replace f ex)) xs)

    replace f ex (TypExpr a b e) =
        TypExpr a b $ replace f ex e

    replace f _ (AbsExpr (Sym f') ex) | f == f' =
        AbsExpr (Sym f') ex

    replace f x (AbsExpr y z) =
        AbsExpr y (replace f x z)

    replace f x (RecExpr xs) = RecExpr (fmap (replace f x) xs)
    replace f x (JSExpr y) = JSExpr (replace f (VarExpr (SymVal (Sym x))) y)

instance Replace Expr Expr where

    replace f _  t @ (LetExpr (Sym f') _ _) | f == f' = t

    replace f ex (LetExpr f' a b) =
        LetExpr f' (replace f ex a) (replace f ex b)

    replace f ex (AppExpr a b) =
        AppExpr (replace f ex a) (replace f ex b)

    replace f ex (VarExpr (SymVal (Sym f'))) | f == f' =
        ex

    replace _ _ (VarExpr x) =
        VarExpr x

    replace f ex (MatExpr e xs) =
        MatExpr (replace f ex e) (map (second (replace f ex)) xs)

    replace f ex (TypExpr a b e) =
        TypExpr a b $ replace f ex e

    replace f _ (AbsExpr (Sym f') ex) | f == f' =
        AbsExpr (Sym f') ex

    replace f x (AbsExpr y z) =
        AbsExpr y (replace f x z)

    replace f x (RecExpr xs) = RecExpr (fmap (replace f x) xs)
    replace f x (JSExpr y) = JSExpr (replace f x y)

instance Replace Sym Expr where

    replace f x (LetExpr (Sym f') a b) | f == f' =
        LetExpr x (replace f (VarExpr (SymVal x)) a) (replace f (VarExpr (SymVal x)) b)

    replace f ex (LetExpr f' a b) =
        LetExpr f' (replace f ex a) (replace f ex b)

    replace f ex (AppExpr a b) =
        AppExpr (replace f ex a) (replace f ex b)

    replace _ _ (VarExpr x) =
        VarExpr x

    replace f ex (MatExpr e xs) =
        MatExpr (replace f ex e) (map (second (replace f ex)) xs)

    replace f ex (TypExpr a b e) =
        TypExpr a b $ replace f ex e

    replace f x (AbsExpr (Sym f') ex) | f == f' =
        AbsExpr x (replace f (VarExpr (SymVal x)) ex)

    replace f x (AbsExpr y z) =
        AbsExpr y (replace f x z)

    replace f x (RecExpr xs) = RecExpr (fmap (replace f x) xs)
    replace f x (JSExpr y) = JSExpr y -- (replace f x y)

instance Replace Patt Expr where

    replace f ex (LetExpr f' a b) =
        LetExpr f' (replace f ex a) (replace f ex b)

    replace f ex (AppExpr a b) =
        AppExpr (replace f ex a) (replace f ex b)

    replace _ _  (VarExpr x) =
        VarExpr x

    replace f ex (MatExpr e xs) =
        MatExpr (replace f ex e) (fmap (first (replace f ex) . second (replace f ex)) xs)

    replace f ex (TypExpr a b e) =
        TypExpr a b $ replace f ex e

    replace f x (AbsExpr y z) =
        AbsExpr y (replace f x z)

    replace f x (RecExpr xs) = RecExpr (fmap (replace f x) xs)
    replace f x (JSExpr y) = JSExpr y


------------------------------------------------------------------------------