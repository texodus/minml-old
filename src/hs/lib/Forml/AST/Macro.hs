------------------------------------------------------------------------------

-- Macros

------------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Forml.AST.Macro(
    Macro(..), 
    MacroCell(..)
) where

import Data.Monoid

------------------------------------------------------------------------------

-- | A single child node on an n-tree

data MacroCell a = Token String (Macro a) | Arg String (Macro a) | Leaf a
    deriving (Eq, Ord, Show)

instance Functor MacroCell where
    fmap f (Token x m) = Token x (fmap f m)
    fmap f (Arg x m)   = Arg x (fmap f m)
    fmap f (Leaf x)    = Leaf (f x)


-- | Since the n-tree representing a `Macro a` has no root, `Macro a` is
--   newtype'd `[MacroCell a]`.  `Macro a`s are `Functor`s, `Monoid`s, 
--   and `Replace`s

newtype Macro a = Macro [MacroCell a]
    deriving (Eq, Ord, Show)

instance Functor Macro where
    fmap f (Macro xs) = Macro (fmap (fmap f) xs)

instance Monoid (Macro a) where
    mempty = Macro []
    mappend (Macro newCells) (Macro oldCells) =
        Macro [ merged
            | new    <- newCells
            , merged <- insert new oldCells ]


-- | Used by mappend to merge two `Macro a`s, one `MacroCell a` at a time.
--   There are some errors emitted by this function, might want to move
--   these at some point.

insert :: MacroCell a -> [MacroCell a] -> [MacroCell a]      
insert (Token x xs) (Token y ys : zs) | x == y = 
    Token x (mappend xs ys) : zs

insert (Arg x xs) (Arg y ys : zs) | x == y =
    Arg x (mappend xs ys) : zs

insert (Arg x _) (Arg y _ : _) = error $
    "Arg naming conflict: (" ++ x ++ ") and (" ++ y ++ ")"

insert (Leaf _) (Leaf _ : _) = error
    "Notation duplicate"

insert x (y : zs) = y : insert x zs
insert x [] = [x]

------------------------------------------------------------------------------
