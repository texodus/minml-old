------------------------------------------------------------------------------

-- Expression parser.

-- `LetExpr` and `TypExpr` are roughly identical; here they've been arranged
-- slightly differently to show the symmetry between function application
-- & combinators of the `Applicative` type class.

------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Forml.AST.Replace where

import Control.Arrow

import Forml.AST

------------------------------------------------------------------------------

class Replace a where
    replace :: String -> a -> a -> a

instance Replace Expr where

    replace f _ (LetExpr (Sym f') a b) | f == f' = LetExpr (Sym f') a b
    replace f ex (LetExpr f' a b) = LetExpr f' (replace f ex a) (replace f ex b)
    replace f ex (AppExpr a b) = AppExpr (replace f ex a) (replace f ex b)
    replace f _ (AbsExpr (Sym f') _) | f == f' = undefined
    replace _ _ (AbsExpr _ _) = undefined
    replace f ex (VarExpr (SymVal (Sym f'))) | f == f' = ex
    replace _ _ (VarExpr x) = (VarExpr x)

    replace f ex (MatExpr e xs) = MatExpr (replace f ex e) (map (second (replace f ex)) xs)
    replace _ _ (RecExpr _) = undefined
    replace _ _ (JSExpr _) = undefined

    replace f ex (TypExpr a b e) = TypExpr a b $ replace f ex e

instance (Replace a) => Replace (Macro a) where

    replace sym (Leaf val) (Leaf expr) = Leaf $ replace sym val expr
    replace _ _  (Leaf _) = error "TODO: decide how this should work"
    replace sym y (Token x xs) = Token x (replace sym y `fmap` xs)
    replace sym y (Arg x xs) = Arg x (replace sym y `fmap` xs)


------------------------------------------------------------------------------