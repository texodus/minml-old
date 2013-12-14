------------------------------------------------------------------------------

-- We're going to borrow the approach taken in "Typing Haskell in 
-- Haskell" (1).  The type of a program will be computed via
-- the `StateT` monad transformer, which will hide our substitution
-- environment and unique type variable generator.

-- (1) http://web.cecs.pdx.edu/~mpj/thih/

------------------------------------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}

module Minml.TypeCheck.TypeCheck where

import Control.Lens
import Control.Monad.State

import Minml.AST
import Minml.TypeCheck.Ass
import Minml.TypeCheck.Kind
import Minml.TypeCheck.Subst
import Minml.Utils

------------------------------------------------------------------------------

type TypeCheck a = StateT TypeCheckState (Either Err) a

data TypeCheckState = TypeCheckState {
    _substs :: Subst,
    _seed :: Int,
    _ass :: [Ass]
}

makeLenses ''TypeCheckState

newState :: TypeCheckState
newState = TypeCheckState [] 0 []

class Infer a where
    infer :: a -> TypeCheck (Type Kind)

newTypeVar :: Kind -> TypeCheck (Type Kind)
newTypeVar k = do
    ver <- seed <%= (+1)
    return (TypeVar (TypeVarT k ("tvar_" ++ show ver)))

typErr :: String -> TypeCheck a
typErr = lift . Left . Err

uniErr :: (HasKind t, Fmt t, HasKind u, Fmt u) => 
          String -> t -> u -> TypeCheck a

uniErr msg t u = typErr $
    msg ++ "\n  "
        ++ fmt u ++ " and " 
        ++ fmt t

find :: String -> TypeCheck (TypeAbs Kind)
find i = use ass >>= find'

    where
        find' [] = typErr ("Unbound identifier: " ++ i)
        find' ((i' :>: sc) : as)
            | i == i'   = return sc
            | otherwise = find' as

scopeAss :: TypeCheck a -> TypeCheck a
scopeAss tc = do
    as <- use ass 
    comAss as tc

withAss :: Ass -> TypeCheck a -> TypeCheck a
withAss newAs tc = do
    as  <- ass <<%= (newAs :)
    comAss as tc

comAss :: [Ass] -> TypeCheck a -> TypeCheck a
comAss as tc = do 
    t   <- tc
    ass .= as
    return t

------------------------------------------------------------------------------