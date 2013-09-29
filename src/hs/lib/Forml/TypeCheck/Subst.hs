------------------------------------------------------------------------------

-- Substitutions

-- There are some basic rules to extending substitutions.

-- Substitutions can be applied to types, and it will be useful for
-- calculating substitutions to have a way to get the free `TypeVar`s
-- in a `Type Kind`.

------------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE ViewPatterns      #-}

module Forml.TypeCheck.Subst where

import qualified Data.List as L

import Forml.AST

------------------------------------------------------------------------------

type Subst = [(TypeVar Kind, Type Kind)]

ext :: Subst -> Subst -> Subst
ext new old = [ (u, apply new t) | (u,t) <- old ] ++ new

class Substitute a where
    apply :: Subst -> a -> a
    getVars :: a -> [TypeVar Kind]

instance Substitute (Type Kind) where
    apply s (TypeVar (flip lookup s -> Just u)) = u
    apply _ (TypeVar u) = TypeVar u
    apply s (TypeApp l r) = TypeApp (apply s l) (apply s r)
    apply _ t = t

    getVars (TypeVar u) = [u]
    getVars (TypeApp l r) = getVars l `L.union` getVars r
    getVars _ = []

instance Substitute a => Substitute [a] where
    apply s = map (apply s)
    getVars = L.nub . concatMap getVars

instance Substitute (TypeAbs Kind) where
    apply s (TypeAbsT ks qt) = TypeAbsT ks (apply s qt)
    getVars (TypeAbsT _ qt)  = getVars qt

------------------------------------------------------------------------------
