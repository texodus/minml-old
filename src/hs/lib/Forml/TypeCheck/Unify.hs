------------------------------------------------------------------------------

-- Unification

-- Unification should modify the substitution environment such that
-- `apply s t == apply s u`.  First apply the current substitution to each,
-- then calculate the Most General Unifier

-- To calculate the most general unifier, recursively descend their 
-- structures until we come to identical types, or a type variable.

-- Once we've committed to creating a new substitution, we need to make
-- sure it's valid by (1) checking that the kinds match, and (2) checking
-- that we are not constructing a replacement for a type into itself.

------------------------------------------------------------------------------

module Forml.TypeCheck.Unify where

import Control.Monad.State
import Control.Lens

import qualified Data.Map as M

import Forml.AST
import Forml.TypeCheck.Kind
import Forml.TypeCheck.Subst
import Forml.TypeCheck.TypeCheck

------------------------------------------------------------------------------

unify :: Type Kind -> Type Kind -> TypeCheck ()
unify t u = do 
    s <- use substs
    apply s t `mgu` apply s u

mgu :: Type Kind -> Type Kind -> TypeCheck ()
mgu (TypeApp f x) (TypeApp g y) = unify f g >> unify x y
mgu (TypeSym t) (TypeSym u) | t == u = return ()
mgu (TypeRec (Record t)) (TypeRec (Record u)) | M.keys t == M.keys u = 
    zipWithM_ mgu (M.elems t) (M.elems u)
mgu (TypeVar u) t = u `varBind` t
mgu t (TypeVar u) = u `varBind` t
mgu t u = uniErr "Types do not unify" t u

varBind :: TypeVar Kind -> Type Kind -> TypeCheck ()
varBind u t 
    | t == TypeVar u     = return ()
    | u `elem` getVars t = uniErr "Occurs check failed" u t -- (2)
    | kind u /= kind t   = uniErr "Kinds do not match"  u t -- (1)
    | otherwise          = extSubst [(u, t)]

extSubst :: Subst -> TypeCheck ()
extSubst new = substs %= ext new
           
------------------------------------------------------------------------------