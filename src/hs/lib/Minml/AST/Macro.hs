------------------------------------------------------------------------------

-- | Macros

------------------------------------------------------------------------------

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Minml.AST.Macro(
    Macro(..), 
    Cell(..),
    MacTree(..)
) where

------------------------------------------------------------------------------

-- | A single child node on an n-tree

data Cell where 
    Token :: String -> Cell
    Let   :: String -> Cell
    Arg   :: String -> Cell
    Pat   :: String -> Cell
    Scope :: Cell
    Sep   :: Cell
  
    deriving (Eq, Ord, Show, Read)

-- | Since the n-tree representing a `Macro a` has no root, `Macro a` is
--   newtype'd `[Cell a]`.  `Macro a`s are `Functor`s, `Monoid`s, 
--   and `Replace`s.

data Macro a where
    Term :: Cell -> MacTree a -> Macro a
    Leaf :: a -> Macro a
    deriving (Eq, Functor, Show, Read)

instance Eq a => Ord (Macro a) where
    compare (Term c _) (Term d _) = compare c d
    compare (Term _ _) _ = GT
    compare _ (Term _ _) = LT
    compare _ _ = EQ

-- | A `MacTree a` simply provides a type-safe `Monoid` instance for 
--   a `[Macro a]`.

newtype MacTree a =
    MacTree [Macro a] 
    deriving (Eq, Functor, Ord, Show, Read)

------------------------------------------------------------------------------
