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
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Forml.TypeCheck.Patt (
    pattCheck
) where

import qualified Data.List as L
import qualified Data.Map as M

import Forml.AST
import Forml.TypeCheck.Prelude
import Forml.TypeCheck.Ass
import Forml.TypeCheck.Generalize
import Forml.TypeCheck.TypeCheck
import Forml.TypeCheck.Unify
import Forml.TypeCheck.Lit

------------------------------------------------------------------------------

pattCheck :: [Ass] -> Patt -> TypeCheck ([Ass], Type Kind)

pattCheck _ (ValPatt (LitVal l)) =
    return ([], litCheck l)

pattCheck _ (ValPatt (SymVal (Sym s))) = do
    t <- newTypeVar Star
    return ([ s :>: TypeAbsT [] t ], t)

pattCheck as (ValPatt (ConVal (TypeSym (TypeSymP l)))) = do
    sc <- find l as
    t  <- freshInst sc
    return ([], t)

pattCheck _ (ValPatt (ConVal t)) = error $ "FATAL: " ++ show t

pattCheck as (ConPatt (TypeSymP con) ps) = do
    sc <- find con as
    x  <- mapM (pattCheck as) ps
    t' <- newTypeVar Star
    t  <- freshInst sc
    unify t (foldr fn t' (map snd x))
    return (L.concat (map fst x), t')

pattCheck as (RecPatt (Record (unzip . M.toList -> (ks, vs)))) = do
    (ass, pattTs) <- unzip `fmap` mapM (pattCheck as) vs
    return (concat ass, TypeRec . Record . M.fromList . zip ks $ pattTs)

------------------------------------------------------------------------------