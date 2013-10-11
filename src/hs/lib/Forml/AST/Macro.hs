------------------------------------------------------------------------------

-- | Macros

------------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}

module Forml.AST.Macro(
    Macro(..), 
    MacroCell(..)
) where

import Data.Monoid

------------------------------------------------------------------------------

-- | A single child node on an n-tree

data MacroCell a where 
    Token :: String -> Macro a -> MacroCell a
    Arg   :: String -> Macro a -> MacroCell a
    Let   :: String -> Macro a -> MacroCell a
    Pat   :: String -> Macro a -> MacroCell a
    Sep   :: Macro a -> MacroCell a
    Leaf  :: a -> MacroCell a

    deriving (Eq, Ord, Show)

instance Functor MacroCell where
    fmap f (Token x m) = Token x (fmap f m)
    fmap f (Arg x m)   = Arg x (fmap f m)
    fmap f (Let x m)   = Let x (fmap f m)
    fmap f (Pat x m)   = Pat x (fmap f m)
    fmap f (Sep x)     = Sep (fmap f x)
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
    Token x (ys <> xs) : zs

insert (Arg x xs) (Arg y ys : zs) | x == y =
    Arg x (ys <> xs) : zs

insert (Pat x xs) (Pat y ys : zs) | x == y =
    Pat x (ys <> xs) : zs

insert (Let x xs) (Let y ys : zs) | x == y =
    Let x (ys <> xs) : zs

insert (Sep xs) (Sep ys : zs) =
    Sep (ys <> xs) : zs

insert (Arg x _) (Arg y _ : _) = error $
    "Arg naming conflict: (" ++ x ++ ") and (" ++ y ++ ")"

insert (Let x _) (Let y _ : _) = error $
    "Let naming conflict: (" ++ x ++ ") and (" ++ y ++ ")"

insert (Pat x _) (Pat y _ : _) = error $
    "Pat naming conflict: (" ++ x ++ ") and (" ++ y ++ ")"

insert (Leaf _) (Leaf _ : _) = error
    "Notation duplicate"

insert x (y : zs) = y : insert x zs
insert x [] = [x]

------------------------------------------------------------------------------
