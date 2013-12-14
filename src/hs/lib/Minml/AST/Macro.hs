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

module Minml.AST.Macro(
    Macro(..), 
    Cell(..),
    MacList(..)
) where

import Control.Applicative
import Control.Monad
import Data.Monoid
import Data.Maybe

import Minml.AST.Replace

------------------------------------------------------------------------------

-- | A single child node on an n-tree

data Cell where 
    Let   :: String -> Cell
    Arg   :: String -> Cell
    Scope :: Cell
    Sep   :: Cell
    Pat   :: String -> Cell
    Token :: String -> Cell
  
    deriving (Eq, Ord, Show)

-- | Since the n-tree representing a `Macro a` has no root, `Macro a` is
--   newtype'd `[Cell a]`.  `Macro a`s are `Functor`s, `Monoid`s, 
--   and `Replace`s.

data Macro a where
    Term :: Cell -> MacList a -> Macro a
    Leaf :: a -> Macro a
    deriving (Eq, Functor, Show)

instance Eq a => Ord (Macro a) where
    compare (Term c _) (Term d _) = compare c d
    compare (Term _ _) _ = GT
    compare _ (Term _ _) = LT
    compare _ _ = EQ

-- | A `MacList a` simply provides a type-safe `Monoid` instance for 
--   a `[Macro a]`.

newtype MacList a =
    MacList [Macro a] 
    deriving (Eq, Functor, Ord, Show)

instance (Show a, Eq a, Replace String a) => Monoid (MacList a) where
    mempty = MacList []
    mappend (MacList ms1) (MacList ms2) =
        MacList $ fromMaybe err merged
        where
            err = error ("Invalid Macro " ++ show ms1 ++ " ::: " ++ show ms2)
            merged = foldM insert [] (ms1 ++ ms2)



insert :: (Show a, Eq a, Replace String a) =>
    [Macro a] -> Macro a -> Maybe [Macro a]

insert (mac : macs) postMac 
    | Just x <- merge mac postMac =
        Just $ x : macs

insert (Term cell ml : macs) postMac =
    (Term cell ml :) <$> insert macs postMac  

insert (Leaf expr : []) postMac =
    insert [postMac] (Leaf expr)

insert [] m = Just [m]

insert _ _ = Nothing

merge :: (Show a, Replace String a, Eq a) => 
    Macro a -> Macro a -> Maybe (Macro a)

merge (Term x m) (Term y n) | x == y =
    Just $ Term x $ n <> m

merge (Term (Pat x) m) (Term (Pat y) n) =
    Just $ Term (Pat y) (n <> replace x y m)

merge (Term (Arg x) m) (Term (Arg y) n) =
    Just $ Term (Arg y) (n <> replace x y m)

merge (Term (Let x) m) (Term (Let y) n) =
    Just $ Term (Let y) (n <> replace x y m)

merge (Term (Pat _) _) (Term (Let _) _) =
    error "Pattern shadows Let"

merge (Term (Let _) _) (Term (Pat _) _) =
    error "Let shadows Pattern"

merge _ _ =
    Nothing

------------------------------------------------------------------------------
