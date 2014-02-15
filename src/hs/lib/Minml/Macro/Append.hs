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

module Minml.Macro.Append(
    emptyTree,
    appendTree
) where

import Control.Applicative
import Control.Monad

import Minml.AST
import Minml.Replace

------------------------------------------------------------------------------

emptyTree :: MacTree a
emptyTree = MacTree []

appendTree :: (Show a, Eq a, Replace String a) => 
    MacTree a -> MacTree a -> Either String (MacTree a)

appendTree (MacTree ms1) (MacTree ms2) = 
    MacTree `fmap` foldM insert [] (ms2 ++ ms1)

insert :: (Show a, Eq a, Replace String a) =>
    [Macro a] -> Macro a -> Either String [Macro a]

insert (mac : macs) postMac 
    | Right x <- merge mac postMac =
        return $ x : macs

insert (Term cell ml : macs) postMac =
    (Term cell ml :) <$> insert macs postMac  

insert (Leaf expr : []) postMac =
    insert [postMac] (Leaf expr)

insert [] m = return [m]

insert _ _ = Left "Undefined Error"

merge :: (Show a, Replace String a, Eq a) => 
    Macro a -> Macro a -> Either String (Macro a)

merge (Term x m) (Term y n) | x == y =
    Term x `fmap` (n `appendTree` m)

merge (Term (Pat x) m) (Term (Pat y) n) =
    Term (Pat y) `fmap` (n `appendTree` replace x y m)

merge (Term (Arg x) m) (Term (Arg y) n) =
    Term (Arg y) `fmap` (n `appendTree` replace x y m)

merge (Term (Let x) m) (Term (Let y) n) =
    Term (Let y) `fmap` (n `appendTree` replace x y m)

merge (Term (Pat _) _) (Term (Let _) _) =
    Left "Pattern shadows Let"

merge (Term (Let _) _) (Term (Pat _) _) =
    Left "Let shadows Pattern"

merge _ _ =
    Left "Undefined error"

------------------------------------------------------------------------------
