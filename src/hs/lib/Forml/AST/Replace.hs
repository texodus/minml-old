------------------------------------------------------------------------------

-- Expression parser.

-- `LetExpr` and `TypExpr` are roughly identical; here they've been arranged
-- slightly differently to show the symmetry between function application
-- & combinators of the `Applicative` type class.

------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Forml.AST.Replace where

import Control.Arrow
import Language.Javascript.JMacro

import Forml.Javascript.Type()
import Forml.Javascript.Val()

import Forml.AST

------------------------------------------------------------------------------

class Replace a b where

    repGen     :: (String -> a -> b -> b) -> String -> a -> b -> b
    replace    :: String -> a -> b -> b
    replaceLet :: String -> a -> b -> b

    replace    = repGen replace
    replaceLet = repGen replaceLet
    repGen     = undefined

instance Replace Expr Expr where

    repGen _ f _ (LetExpr (Sym f') a b) | f == f' = LetExpr (Sym f') a b
    repGen g f ex (LetExpr f' a b) = LetExpr f' (g f ex a) (g f ex b)
    repGen g f ex (AppExpr a b) = AppExpr (g f ex a) (g f ex b)
    repGen _ f _ (AbsExpr (Sym f') _) | f == f' = undefined
    repGen _ _ _ (AbsExpr _ _) = undefined
    repGen _ f ex (VarExpr (SymVal (Sym f'))) | f == f' = ex
    repGen _ _ _ (VarExpr x) = VarExpr x
    repGen g f ex (MatExpr e xs) = MatExpr (g f ex e) (map (second (g f ex)) xs)
    repGen _ _ _ (RecExpr _) = undefined
    repGen _ _ _ (JSExpr _) = undefined
    repGen g f ex (TypExpr a b e) = TypExpr a b $ g f ex e

    replaceLet f t @ (VarExpr (SymVal (Sym f''))) (LetExpr (Sym f') a b) | f == f' =
        LetExpr (Sym f'') (replaceLet f t a) (replaceLet f t b)
    replaceLet f g h = repGen replaceLet f g h

instance Replace String (Macro Expr) where

    replace sym = replace sym . VarExpr . SymVal . Sym   
    replaceLet sym = replaceLet sym . VarExpr . SymVal . Sym   

instance Replace Expr (Macro Expr) where

    replace sym y (Leaf expr) = Leaf $ replace sym y expr
    replace sym y (Token x xs) = Token x (replace sym y `fmap` xs)
    replace sym y (Arg x xs) = Arg x (replace sym y `fmap` xs)

    replaceLet sym y (Leaf expr) = Leaf $ replaceLet sym y expr
    replaceLet sym y (Token x xs) = Token x (replaceLet sym y `fmap` xs)
    replaceLet sym y (Arg x xs) = Arg x (replaceLet sym y `fmap` xs)

instance (JMacro a) => Replace JExpr a where
 
    replace s x = 
        withHygiene (jfromGADT . composOp f . jtoGADT)
        where
            f :: JMGadt a -> JMGadt a
            f (JMGExpr (ValExpr (JVar (StrI z)))) | z == s = JMGExpr x
            f z = composOp f z

------------------------------------------------------------------------------