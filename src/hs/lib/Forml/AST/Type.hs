--------------------------------------------------------------------------------

-- This ensures that any `Type a` will be consistent, and allows us to
-- generate 2 types of `Type a` which share most of their structure.

-- `TypeGen`s are a special `TypeVar` which we use to mark places where
-- we want a type to be polymorphic.

--------------------------------------------------------------------------------

{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections      #-}

module Forml.AST.Type (
    module Forml.AST.Kind,
    TypeSym( .. ),
    TypeAbs( .. ),
    TypeVar( .. ),
    Type( .. )
) where

import Data.Ix
import Data.List

import Forml.AST.Kind
import Forml.AST.Record
import Forml.Utils

------------------------------------------------------------------------------

data TypeVar a where
    TypeVarP :: String -> TypeVar ()
    TypeVarT :: Kind -> String -> TypeVar Kind

data TypeSym a where
    TypeSymP :: String -> TypeSym ()
    TypeSymT :: Kind -> String -> TypeSym Kind

data TypeAbs a where
    TypeAbsP :: Type () -> TypeAbs ()
    TypeAbsT :: [Kind] -> Type Kind -> TypeAbs Kind

data Type a where
    TypeSym :: TypeSym a -> Type a
    TypeVar :: TypeVar a -> Type a
    TypeApp :: Type a -> Type a -> Type a
    TypeRec :: Record (Type a) -> Type a

    TypeGen :: Int -> Type Kind

instance (Fmt a) => Fmt (TypeVar a) where
    fmt (TypeVarP s) = s
    fmt (TypeVarT _ s) = s

instance (Fmt a) => Fmt (TypeSym a) where
    fmt (TypeSymP s) = s
    fmt (TypeSymT _ s) = s

instance (Fmt a) => Fmt (TypeAbs a) where
    fmt (TypeAbsP t)   = "forall ?. " ++ fmt t
    fmt (TypeAbsT k s) = "forall " ++ varNames k ++ ". " ++ fmt s
        where
            varNames = concat . intersperse " " . toTypGen
            toTypGen = map (fmt . TypeGen) . range . (0,) . length

instance (Fmt a) => Fmt (Type a) where
    fmt (TypeSym s) = fmt s
    fmt (TypeVar v) = fmt v
    fmt (TypeApp a b) = "(" ++ fmt a ++ " " ++ fmt b ++ ")"
    fmt (TypeGen i) = "<<" ++ show i ++ ">>"
    fmt (TypeRec m) = fmt m

deriving instance (Ord a) => Ord (Type a)
deriving instance (Ord a) => Ord (TypeVar a)
deriving instance (Ord a) => Ord (TypeSym a)
deriving instance (Ord a) => Ord (TypeAbs a)

deriving instance (Eq a) => Eq (Type a)
deriving instance (Eq a) => Eq (TypeVar a)
deriving instance (Eq a) => Eq (TypeSym a)
deriving instance (Eq a) => Eq (TypeAbs a)

deriving instance (Show a) => Show (Type a)
deriving instance (Show a) => Show (TypeVar a)
deriving instance (Show a) => Show (TypeSym a)
deriving instance (Show a) => Show (TypeAbs a)

--------------------------------------------------------------------------------
