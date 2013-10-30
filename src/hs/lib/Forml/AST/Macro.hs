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
import qualified Data.List as L
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

-- instance Ord MacroCell where compare = undefined

-- | Since the n-tree representing a `Macro a` has no root, `Macro a` is
--   newtype'd `[MacroCell a]`.  `Macro a`s are `Functor`s, `Monoid`s, 
--   and `Replace`s

data Macro a where
    MacroTerm :: MacroCell -> MacroList a -> Macro a
    MacroLeaf :: a -> Macro a
    deriving (Eq, Functor, Show)

instance Eq a => Ord (Macro a) where
    compare (MacroTerm c _) (MacroTerm d _) = compare c d
    compare (MacroTerm _ _) _ = GT
    compare _ (MacroTerm _ _) = LT
    compare _ _ = EQ


newtype MacroList a =
    MacroList [Macro a] 
    deriving (Eq, Functor, Show)

instance (Show a, Eq a) => Monoid (MacroList a) where
    mempty = MacroList []
    mappend (MacroList ms1) (MacroList ms2) =
        MacroList $ L.sort $ fromMaybe
            (error ("Invalid Macro " ++ show ms1 ++ " ::: " ++ show ms2))
            (foldM insert ms1 ms2)

-- | Used by mappend to merge two `Macro a`s, one `MacroCell a` at a time.
--   There are some errors emitted by this function, might want to move
--   these at some point.

insert :: (Eq a) => [Macro a] -> Macro a -> Maybe [Macro a]    
insert (MacroTerm cell1 (MacroList ms1) : ms) (MacroTerm cell2 (MacroList ms2))
    | cell1 == cell2 = do
        merged <- foldM insert ms1 ms2
        return $ MacroTerm cell1 (MacroList (L.sort merged)) : ms

insert (MacroTerm cell ms1 : ms2) mt =
    (MacroTerm cell ms1 :) `fmap` insert ms2 mt

insert (MacroLeaf ex : []) t @ (MacroTerm _ _) =
    insert [t] (MacroLeaf ex)

insert [] mt =
    Just [mt]

insert _ _ = 
    Nothing

------------------------------------------------------------------------------
