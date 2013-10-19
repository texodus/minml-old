------------------------------------------------------------------------------

-- Generalization

-- Consider the program

--    "   let f = fun x -> x;               \
--    \   f 1 == f 1                        \
--    \       && f \"test\" == f \"test\"   "

-- Without generalization step, the application of `f 1` will introduce the
-- assumption `f : Num -> Num`, which makes the rest of the expression
-- invalid.

-- Generalization will guarantee that each application of `f` will have
-- new type variables, by replacing the type variable representing `x` with
-- a fresh var at every invocation.

-- Simple generalization is simply the process of replacing some type
-- variables with `TypeGen`, whose integer argument represents the index
-- in the `TypeAbs Kind`s argument list.  This somewhat odd representation
-- will make it easier to instantiate `TypeAbs Kind`s later, as we can figure
-- out what `Kind` to assign to a fresh `TypeVar Kind`.

-- With these tools, we can construct an environment aware `generalize`, which
-- applies the current substitution and only generalizes the free `TypeVar`s
-- in `valT`.

------------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}

module Forml.TypeCheck.Generalize(
    generalize,
    freshInst,
    quantify
) where

import           Control.Applicative
import           Control.Monad.State
import qualified Data.List           as L

import Forml.AST
import Forml.TypeCheck.Ass
import Forml.TypeCheck.Kind
import Forml.TypeCheck.Subst
import Forml.TypeCheck.TypeCheck

------------------------------------------------------------------------------

quantify :: [TypeVar Kind] -> Type Kind -> TypeAbs Kind
quantify vars typ = TypeAbsT kinds (apply subs typ)
    where
        qVars = [ var | var <- getVars typ, var `elem` vars ]
        kinds = map kind qVars
        subs  = zip qVars (map TypeGen [ 0 .. ])

-- | Creates a TypeAbs from a Type

generalize :: [Ass] -> Type Kind -> TypeCheck (TypeAbs Kind)
generalize as valT = do

    subs <- fst <$> get
    return (quantify (getS subs valT L.\\ getS subs as) (apply subs valT))

    where
        getS x = (getVars .) . apply $ x

-- | Creates a new instance of a Type from a TypeAbs

freshInst :: TypeAbs Kind -> TypeCheck (Type Kind)
freshInst (TypeAbsT ks qt) = do

    ts <- mapM newTypeVar ks
    return (inst ts qt)

    where
        inst ts (TypeApp l r) = TypeApp (inst ts l) (inst ts r)
        inst ts (TypeGen n) = ts !! n
        inst _ t = t

------------------------------------------------------------------------------
