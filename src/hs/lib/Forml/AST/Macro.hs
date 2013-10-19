------------------------------------------------------------------------------

-- | Macros

------------------------------------------------------------------------------

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}

module Forml.AST.Macro(
    Macro(..), 
    MacroCell(..),
    MacroList(..)
) where

import Data.Monoid

------------------------------------------------------------------------------

-- | A single child node on an n-tree

data MacroCell where 
    Token :: String -> MacroCell
    Arg   :: String -> MacroCell
    Let   :: String -> MacroCell
    Pat   :: String -> MacroCell
    Scope :: [MacroCell] -> MacroCell
    Sep   :: MacroCell

    deriving (Eq, Ord, Show)

-- | Since the n-tree representing a `Macro a` has no root, `Macro a` is
--   newtype'd `[MacroCell a]`.  `Macro a`s are `Functor`s, `Monoid`s, 
--   and `Replace`s

data Macro a where
    MacroTerm :: MacroCell -> MacroList a -> Macro a
    MacroLeaf :: a -> Macro a
    deriving (Functor, Show)

newtype MacroList a =
    MacroList [Macro a] 
    deriving (Functor, Show)

instance (Show a) => Monoid (MacroList a) where
    mempty = MacroList []
    mappend (MacroList ms1) (MacroList ms2) =
        MacroList $ foldl insert ms1 ms2

-- | Used by mappend to merge two `Macro a`s, one `MacroCell a` at a time.
--   There are some errors emitted by this function, might want to move
--   these at some point.

insert :: (Show a) => [Macro a] -> Macro a -> [Macro a]    
insert (MacroTerm cell1 ms1 : ms) (MacroTerm cell2 ms2)
    | cell1 == cell2 = MacroTerm cell1 (ms1 <> ms2) : ms

insert (MacroTerm cell ms1 : ms2) mt =
    MacroTerm cell ms1 : insert ms2 mt

insert [] mt =
    [mt]

insert x y = 
    error ("Invalid Macro :: " ++ show x ++ " ;;; " ++ show y)

------------------------------------------------------------------------------
