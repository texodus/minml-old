------------------------------------------------------------------------------

-- | Replace typeclass

------------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances  #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE UndecidableInstances         #-}

module Minml.Macro.Replace(
    module Minml.AST.Replace
) where

import Control.Arrow
import GHC.Generics
import Language.Javascript.JMacro

import Minml.AST
import Minml.AST.Replace
import Minml.Javascript.Expr ()

------------------------------------------------------------------------------

instance Replace Expr JExpr where

    replace s x = replace s (toJExpr x)

instance Replace String Expr where

    replace s x =

        replace s (Sym x)
            . replace s (ValPatt (SymVal (Sym x)))
            . replace s (VarExpr (SymVal (Sym x)))

instance Replace Expr Expr where

    replace f ex (VarExpr (SymVal (Sym f'))) 
        | f == f' = ex

    replace f _  t @ (LetExpr (Sym f') _ _) 
        | f == f' = t

    replace f _ (AbsExpr (Sym f') ex) | f == f' =
        AbsExpr (Sym f') ex

    replace f x (JSExpr y) =
        JSExpr (replace f x y)

    replace f x y = 
        to . greplace (replace f x :: Expr -> Expr) . from $ y

    replace f ex (MatExpr e xs) =
        MatExpr (replace f ex e) (map (second (replace f ex)) xs)

instance Replace Sym Expr where

    replace f ex (LetExpr (Sym f') a b) | f == f' =
        LetExpr ex a b

    replace f x (AbsExpr (Sym f') ex) | f == f' =
        AbsExpr x ex

    replace f x y =
        to . greplace (replace f x :: Expr -> Expr) . from $ y

class GReplace a f where
    greplace :: (a -> a) -> f b -> f b

instance Replace Patt Expr where

    replace f ex (MatExpr e xs) =
        MatExpr (replace f ex e) (fmap (first (replace f ex) . second (replace f ex)) xs)

    replace f x y =
        to . greplace (replace f x :: Expr -> Expr) . from $ y

instance GReplace a U1 where 
    greplace _ ex = ex

instance (GReplace a f) => GReplace a (M1 i c f) where
    greplace f (M1 ex) = M1 $ greplace f ex

instance GReplace a (K1 i a) where
    greplace f (K1 ex) = K1 (f ex)

instance GReplace a (K1 i JExpr) where
    greplace _ x = x

instance GReplace a (K1 i c) where
    greplace _ (K1 x) = K1 x

instance (Functor c) => GReplace a (K1 i (c a)) where
    greplace f (K1 x) = K1 (f `fmap` x)

instance (Functor c) => GReplace a (K1 i (c (b, a))) where
    greplace f (K1 x) = K1 (second f `fmap` x)

instance (Functor c) => GReplace a (K1 i (c (a, b))) where
    greplace f (K1 x) = K1 (first f `fmap` x)

instance (GReplace a f, GReplace a g) => GReplace a (f :+: g) where
    greplace f (L1 x) = L1 $ greplace f x
    greplace f (R1 x) = R1 $ greplace f x

instance (GReplace a f, GReplace a g) => GReplace a (f :*: g) where
    greplace f (a :*: b) = greplace f a :*: greplace f b

------------------------------------------------------------------------------
