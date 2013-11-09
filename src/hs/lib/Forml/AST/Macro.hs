------------------------------------------------------------------------------

-- | Macros

------------------------------------------------------------------------------

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
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

import Forml.AST.Replace

------------------------------------------------------------------------------

-- | A single child node on an n-tree

data MacroCell where 
    Token :: String -> MacroCell
    Scope :: MacroCell
    Sep   :: MacroCell
    Let   :: String -> MacroCell
    Pat   :: String -> MacroCell
    Arg   :: String -> MacroCell
  
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
    deriving (Eq, Functor, Ord, Show)

instance (Show a, Eq a, Replace String a) => Monoid (MacroList a) where
    mempty = MacroList []
    mappend (MacroList ms1) (MacroList ms2) =
        MacroList $ fromMaybe
            (error ("Invalid Macro " ++ show ms1 ++ " ::: " ++ show ms2))
            (foldM insert [] (ms2 ++ ms1))

-- | Used by mappend to merge two `Macro a`s, one `MacroCell a` at a time.
--   There are some errors emitted by this function, might want to move
--   these at some point.

insert :: (Eq a, Replace String a) => [Macro a] -> Macro a -> Maybe [Macro a]

insert 
    (MacroTerm cell1 (MacroList ms1) : ms)
    (MacroTerm cell2 (MacroList ms2))
    | cell1 == cell2 = do
        merged <- foldM insert [] (ms1 ++ ms2)
        return $ MacroTerm cell1 (MacroList (L.sort merged)) : ms

insert 
    (MacroTerm (Pat cell1) (MacroList ms1) : ms)
    (MacroTerm (Pat cell2) (MacroList ms2)) =
        insertCommon Pat cell1 cell2 ms1 ms2 ms

insert 
    (MacroTerm (Arg cell1) (MacroList ms1) : ms)
    (MacroTerm (Arg cell2) (MacroList ms2)) =
        insertCommon Arg cell1 cell2 ms1 ms2 ms

insert 
    (MacroTerm (Let cell1) (MacroList ms1) : ms)
    (MacroTerm (Let cell2) (MacroList ms2)) =
        insertCommon Let cell1 cell2 ms1 ms2 ms

insert (MacroTerm cell ms1 : ms2) mt =
    (MacroTerm cell ms1 :) `fmap` insert ms2 mt

insert (MacroLeaf ex : []) t @ (MacroTerm _ _) =
    insert [t] (MacroLeaf ex)

insert [] mt =
    Just [mt]

insert _ _ = 
    Nothing

insertCommon ::
    (Eq a, Replace String a) => 
        (String -> MacroCell) ->
        String ->
        String -> 
        [Macro a] -> 
        [Macro a] -> 
        [Macro a] -> 
        Maybe [Macro a]

insertCommon f cell1 cell2 ms1 ms2 ms = do
    merged <- foldM insert [] (ms2 ++ replace cell1 cell2 ms1)
    return $ MacroTerm (f cell2) (MacroList merged) : ms 

------------------------------------------------------------------------------
