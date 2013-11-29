------------------------------------------------------------------------------

-- | Macros

------------------------------------------------------------------------------

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE GADTs #-}

module Forml.AST.Macro(
    Macro(..), 
    Cell(..),
    MacroList(..)
) where

import Control.Monad
import qualified Data.List as L
import Data.Monoid
import Data.Maybe

import Forml.AST.Replace

------------------------------------------------------------------------------

-- | A single child node on an n-tree

data Cell where 
    Let   :: String -> Cell
    Arg   :: String -> Cell
    Token :: String -> Cell
    Scope :: Cell
    Sep   :: Cell
    Pat   :: String -> Cell
  
    deriving (Eq, Ord, Show)

-- | Since the n-tree representing a `Macro a` has no root, `Macro a` is
--   newtype'd `[Cell a]`.  `Macro a`s are `Functor`s, `Monoid`s, 
--   and `Replace`s.

data Macro a where
    MacroTerm :: Cell -> MacroList a -> Macro a
    MacroLeaf :: a -> Macro a
    deriving (Eq, Functor, Show)

instance Eq a => Ord (Macro a) where
    compare (MacroTerm c _) (MacroTerm d _) = compare c d
    compare (MacroTerm _ _) _ = GT
    compare _ (MacroTerm _ _) = LT
    compare _ _ = EQ

-- | A `MacroList a` simply provides a type-safe `Monoid` instance for 
--   a `[Macro a]`.

newtype MacroList a =
    MacroList [Macro a] 
    deriving (Eq, Functor, Ord, Show)

instance (Show a, Eq a, Replace String a) => Monoid (MacroList a) where
    mempty = MacroList []
    mappend (MacroList ms1) (MacroList ms2) =
        MacroList $ fromMaybe err merged
        where
            err = error ("Invalid Macro " ++ show ms1 ++ " ::: " ++ show ms2)
            merged = foldM insert [] (ms2 ++ ms1)

eqCell :: (Replace String a) => Cell -> Cell -> [Macro a] -> [Macro a] -> Maybe (Cell, [Macro a])

eqCell x y m n | x == y =
   Just (x, m ++ n)

eqCell (Pat x) (Pat y) m n =
    Just (Pat y, n ++ replace x y m)

eqCell (Arg x) (Arg y) m n =
    Just (Arg y, n ++ replace x y m)

eqCell (Let x) (Let y) m n =
    Just (Let y, n ++ replace x y m)

eqCell (Pat _) (Let _) _ _ =
    error "Pattern shadows Let"

eqCell (Let _) (Pat _) _ _ =
    error "Let shadows Pattern"

eqCell _ _ _ _ =
    Nothing

insert :: (Eq a, Replace String a) => [Macro a] -> Macro a -> Maybe [Macro a]

insert 
    (MacroTerm cell1 (MacroList ms1) : ms)
    mt @ (MacroTerm cell2 (MacroList ms2))
    | Just (x, y) <- eqCell cell1 cell2 ms1 ms2 = do
        merged <- foldM insert [] y
        return $ MacroTerm x (MacroList (L.sort merged)) : ms
    | otherwise =
        (MacroTerm cell1 (MacroList ms1) :) `fmap` insert ms mt

insert (MacroLeaf ex : []) t @ (MacroTerm _ _) =
    insert [t] (MacroLeaf ex)

insert [] mt =
    Just [mt]

insert _ _ = 
    Nothing

------------------------------------------------------------------------------
