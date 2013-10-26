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

import Control.Monad
import Data.Monoid
import Data.Maybe

------------------------------------------------------------------------------

-- | A single child node on an n-tree

data MacroCell where 
    Token :: String -> MacroCell
    Arg   :: String -> MacroCell
    Let   :: String -> MacroCell
    Pat   :: String -> MacroCell
    Scope :: MacroCell
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
        MacroList $ fromMaybe
            (error ("Invalid Macro " ++ show ms1 ++ " ::: " ++ show ms2))
            (foldM insert ms1 ms2)

-- | Used by mappend to merge two `Macro a`s, one `MacroCell a` at a time.
--   There are some errors emitted by this function, might want to move
--   these at some point.

insert :: [Macro a] -> Macro a -> Maybe [Macro a]    
insert (MacroTerm cell1 (MacroList ms1) : ms) (MacroTerm cell2 (MacroList ms2))
    | cell1 == cell2 = do
        merged <- foldM insert ms1 ms2
        return $ MacroTerm cell1 (MacroList merged) : ms

insert (MacroTerm cell ms1 : ms2) mt =
    (MacroTerm cell ms1 :) `fmap` insert ms2 mt

insert (MacroLeaf ex : []) t @ (MacroTerm _ _) =
    insert [t] (MacroLeaf ex)

insert [] mt =
    Just [mt]

insert _ _ = 
    Nothing

------------------------------------------------------------------------------
