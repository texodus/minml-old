------------------------------------------------------------------------------

-- We're going to borrow the approach taken in "Typing Haskell in 
-- Haskell" (1).  The type of a program will be computed via
-- the `StateT` monad transformer, which will hide our substitution
-- environment and unique type variable generator.

-- (1) http://web.cecs.pdx.edu/~mpj/thih/

------------------------------------------------------------------------------

module Forml.TypeCheck.TypeCheck where

import Control.Monad.State

import Forml.AST
import Forml.TypeCheck.Kind
import Forml.TypeCheck.Subst
import Forml.Utils

------------------------------------------------------------------------------

type TypeCheck a = StateT (Subst, Int) (Either Err) a

newTypeVar :: Kind -> TypeCheck (Type Kind)
newTypeVar k = do
    (s, i) <- get
    put (s, i + 1)
    return (TypeVar (TypeVarT k ("tvar_" ++ show i)))

typErr :: String -> TypeCheck a
typErr = lift . Left . Err

uniErr :: (HasKind t, Fmt t, HasKind u, Fmt u) => 
          String -> t -> u -> TypeCheck a

uniErr msg t u = typErr $
    msg ++ "\n  "
        ++ fmt u ++ " and " 
        ++ fmt t

------------------------------------------------------------------------------