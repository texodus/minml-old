------------------------------------------------------------------------------

-- The kinds for `Type a` data structures can be calculated recursively.

-- Kind inference is quite simple - given the Kind we expect of a symbol, we
-- extrapolate the Kinds of it's constituent parts through structural
-- inspection.
------------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE ViewPatterns       #-}

module Forml.TypeCheck.Kind where

import Forml.AST

------------------------------------------------------------------------------

class HasKind t where
    kind :: t -> Kind

instance HasKind (Type Kind) where
    kind (TypeSym tc) = kind tc
    kind (TypeVar u)  = kind u
    kind (TypeApp (kind -> Kfun _ k) _) = k
    kind x = error (show x)

instance HasKind (TypeVar Kind) where
    kind (TypeVarT k _) = k

instance HasKind (TypeSym Kind) where
    kind (TypeSymT k _) = k

toKind :: Kind -> Type () -> Type Kind
toKind k (TypeSym (TypeSymP n)) = TypeSym (TypeSymT k n)
toKind k (TypeVar (TypeVarP n)) = TypeVar (TypeVarT k n)
toKind k (TypeApp f x) =
    TypeApp (toKind (Kfun Star k) f) (toKind Star x)

------------------------------------------------------------------------------
