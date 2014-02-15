
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances  #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Minml.Optimize where

import Control.Applicative
import Data.Monoid
import GHC.Generics
import Language.Javascript.JMacro

import Minml.AST
import Minml.Replace.Base
import Minml.Replace.Generic

------------------------------------------------------------------------------

optimize :: (Replace Expr Expr) => 
    Expr -> Expr

optimize (AnnExpr _ _ (Just x)) =
    optimize x

optimize (LetExpr (Sym s) x (Just y)) =
    case count s y + count s x of
        0 -> replace s (optimize x) (optimize y)
        1 -> replace s (optimize x) (optimize y)
        _ -> LetExpr (Sym s) (optimize x) (Just $ optimize y)

optimize x = f x
    where f = to . greplace optimize . from

class Count a where

    -- | Finds the # of occurences of a symbol of a given type, `a`.
    count :: String -> a -> Int

instance Count Expr where
    count sym (LetExpr (Sym s) _ _) | s == sym =
        0

    count sym (LetExpr _ a (Just b)) =
        count sym a + count sym b

    count sym (AbsExpr (Sym s) _) | s == sym =
        0

    count sym (AbsExpr _ a) =
        count sym a

    count sym (JSExpr a) =
        count sym a

    count sym (VarExpr (SymVal (Sym s))) | s == sym = 1

    count sym (MatExpr ex cases) =
        count sym ex + sum (count sym . snd <$> cases)

    -- Don't count an annotated expression, as these will not be rendered.

    count sym (AnnExpr _ _ (Just x)) =
        count sym x

    count sym ex =
        gfold (count sym :: Expr -> Int) . from $ ex

instance Count JExpr where

    count s =
        f . jtoGADT
        where
            f :: JMGadt a -> Int
            f (JMGExpr (ValExpr (JVar (StrI z)))) | z == s = 1
            f z = composOpFold 0 (+) f z

class GFold a f where
    gfold :: Monoid b => (a -> b) -> f c -> b

instance GFold a U1 where
    gfold _ _ = mempty

instance (GFold a f) => GFold a (M1 i c f) where
    gfold f (M1 ex) = gfold f ex

instance (GFold a f, GFold a g) => GFold a (f :+: g) where
    gfold f (L1 x) = gfold f x
    gfold f (R1 x) = gfold f x

instance (GFold a f, GFold a g) => GFold a (f :*: g) where
    gfold f (a :*: b) = gfold f a <> gfold f b

instance GFold a (K1 i a) where
    gfold f (K1 ex) = f ex

instance GFold a (K1 i (Maybe a)) where
    gfold f (K1 (Just ex)) = f ex
    gfold _ (K1 Nothing) = mempty

instance GFold a (K1 i c) where
    gfold _ (K1 _) = mempty

instance Monoid Int where
    mempty = 0
    mappend = (+)

------------------------------------------------------------------------------
