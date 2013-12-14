------------------------------------------------------------------------------

-- Patterns are a bit more complicated.  They can introduce symbols, so
-- we must return a list of these `Ass`s as well as the their `Type a`.

-- Simple constructor patterns can be checked by introducing a fresh 
-- instance of their `TypeAbs Kind`.  

-- In order to check destruction patterns, we need to recreate the implied
-- (abstraction) type of the arguments, and unify with the constructor's
-- `Ass` from the environment.  

------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Forml.TypeCheck.Patt (
    infer
) where

import Control.Lens

import qualified Data.Map as M

import Forml.AST
import Forml.TypeCheck.Prelude
import Forml.TypeCheck.Ass
import Forml.TypeCheck.Generalize
import Forml.TypeCheck.TypeCheck
import Forml.TypeCheck.Unify
import Forml.TypeCheck.Lit

------------------------------------------------------------------------------

instance Infer Patt where
    infer (ValPatt (LitVal l)) =
        return (litCheck l)

    infer (ValPatt (SymVal (Sym s))) = do
        t <- newTypeVar Star
        ass %= (++ [ s :>: TypeAbsT [] t ])
        return t

    infer (ValPatt (ConVal (TypeSym (TypeSymP l)))) =
        find l >>= infer

    infer (ValPatt (ConVal t)) =
        error $ "FATAL: " ++ show t

    infer (ConPatt (TypeSymP con) ps) = do
        sc <- find con
        x  <- mapM infer ps
        t' <- newTypeVar Star
        t  <- infer sc
        unify t (foldr fn t' x)
        return t'

    infer (RecPatt (Record (unzip . M.toList -> (ks, vs)))) = do
        pattTs <- mapM infer vs
        return (TypeRec . Record . M.fromList . zip ks $ pattTs)

------------------------------------------------------------------------------