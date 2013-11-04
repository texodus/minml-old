------------------------------------------------------------------------------

-- | Expressions.

------------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

module Forml.AST.Expr (
    Expr(..)
) where

import Control.Arrow
import Data.Monoid
import Language.Javascript.JMacro
import Text.PrettyPrint.Leijen.Text

import Forml.AST.Patt
import Forml.AST.Record
import Forml.AST.Replace
import Forml.AST.Type
import Forml.AST.Val
import Forml.Utils

------------------------------------------------------------------------------

-- | The Expression AST data type.  `App` is a function application; `Abs` is
--   function literal (function abstraction)

data Expr where
    LetExpr :: Sym  -> Expr -> Maybe Expr -> Expr
    AppExpr :: Expr -> Expr -> Expr
    AbsExpr :: Sym  -> Expr -> Expr
    VarExpr :: Val  -> Expr
    MatExpr :: Expr -> [(Patt, Expr)] -> Expr
    RecExpr :: Record Expr -> Expr
    JSExpr  :: JExpr -> Expr

    TypExpr :: TypeSym () -> TypeAbs () -> Maybe Expr -> Expr

    deriving (Eq, Ord, Show)

instance Monoid (Maybe Expr) where

    mempty = Nothing

    mappend (Just (LetExpr a b c)) x = Just (LetExpr a b (c `mappend` x))
    mappend (Just (TypExpr a b c)) x = Just (TypExpr a b (c `mappend` x))
    mappend (Just _) _ = error "Cannot append terminal parses"
    mappend _ y = y 


instance Replace String Expr where

    -- Lets are special cases to handle recursion

    replace f t (LetExpr (Sym f') a b) | f == f' =
        LetExpr (Sym t) (replace f t . replace f (VarExpr (SymVal (Sym t))) $ a)
                    (replace f t . replace f (VarExpr (SymVal (Sym t))) $ b)

    --replace sym patt (LetExpr (Sym sym') a b) | sym == sym' =
    --    MatExpr (replace sym patt a) [(patt, replace sym patt b)]

    replace f t (AbsExpr (Sym f') ex) | f == f' =
        AbsExpr (Sym t) (replace f t . replace f (VarExpr (SymVal (Sym t))) $ ex)

    replace f _  (LetExpr (Sym f') a b) | f == f' =
        LetExpr (Sym f') a b

    replace f ex (LetExpr f' a b) =
        LetExpr f' (replace f ex a) (replace f ex b)

    replace f ex (AppExpr a b) =
        AppExpr (replace f ex a) (replace f ex b)

    replace f t (VarExpr (SymVal (Sym f'))) | f == f' =
        VarExpr (SymVal (Sym t))

    replace _ _  (VarExpr x) =
        VarExpr x

    replace f ex (MatExpr e xs) =
        MatExpr (replace f ex e) (fmap (second (replace f ex)) xs)

    replace f ex (TypExpr a b e) =
        TypExpr a b $ replace f ex e

    replace f _ (AbsExpr (Sym f') ex) | f == f' =
        AbsExpr (Sym f') ex

    replace f x (AbsExpr y z) =
        AbsExpr y (replace f x z)

    replace f x (RecExpr xs) = RecExpr (fmap (replace f x) xs)
    replace _ _ (JSExpr x) = JSExpr x

instance Replace Expr Expr where

    replace f _  t @ (LetExpr (Sym f') _ _) | f == f' = t

    replace f ex (LetExpr f' a b) =
        LetExpr f' (replace f ex a) (replace f ex b)

    replace f ex (AppExpr a b) =
        AppExpr (replace f ex a) (replace f ex b)

    replace f ex (VarExpr (SymVal (Sym f'))) | f == f' =
        ex

    replace _ _ (VarExpr x) =
        VarExpr x

    replace f ex (MatExpr e xs) =
        MatExpr (replace f ex e) (map (second (replace f ex)) xs)

    replace f ex (TypExpr a b e) =
        TypExpr a b $ replace f ex e

    replace f _ (AbsExpr (Sym f') ex) | f == f' =
        AbsExpr (Sym f') ex

    replace f x (AbsExpr y z) =
        AbsExpr y (replace f x z)

    replace f x (RecExpr xs) = RecExpr (fmap (replace f x) xs)
    replace _ _ (JSExpr x)   = JSExpr x

instance Replace Sym Expr where

    replace f x (LetExpr (Sym f') a b) | f == f' =
        LetExpr x (replace f (VarExpr (SymVal x)) a) (replace f (VarExpr (SymVal x)) b)

    replace f ex (LetExpr f' a b) =
        LetExpr f' (replace f ex a) (replace f ex b)

    replace f ex (AppExpr a b) =
        AppExpr (replace f ex a) (replace f ex b)

    replace _ _ (VarExpr x) =
        VarExpr x

    replace f ex (MatExpr e xs) =
        MatExpr (replace f ex e) (map (second (replace f ex)) xs)

    replace f ex (TypExpr a b e) =
        TypExpr a b $ replace f ex e

    replace f x (AbsExpr (Sym f') ex) | f == f' =
        AbsExpr x (replace f (VarExpr (SymVal x)) ex)

    replace f x (AbsExpr y z) =
        AbsExpr y (replace f x z)

    replace f x (RecExpr xs) = RecExpr (fmap (replace f x) xs)
    replace _ _ (JSExpr x)   = JSExpr x

instance Replace Patt Expr where

    replace f ex (LetExpr f' a b) =
        LetExpr f' (replace f ex a) (replace f ex b)

    replace f ex (AppExpr a b) =
        AppExpr (replace f ex a) (replace f ex b)

    replace _ _  (VarExpr x) =
        VarExpr x

    replace f ex (MatExpr e xs) =
        MatExpr (replace f ex e) (fmap (first (replace f ex) . second (replace f ex)) xs)

    replace f ex (TypExpr a b e) =
        TypExpr a b $ replace f ex e

    replace f x (AbsExpr y z) =
        AbsExpr y (replace f x z)

    replace f x (RecExpr xs) = RecExpr (fmap (replace f x) xs)
    replace _ _ (JSExpr x) = JSExpr x

instance Fmt Expr where

    fmt (LetExpr binding ex (Just cont)) =
        "let " ++ fmt binding ++ " = " ++ fmt ex ++ "; " ++ fmt cont

    fmt (AppExpr f x) =
        "(" ++ fmt f ++ " " ++ fmt x ++ ")"

    fmt (AbsExpr x ret) =
        "(\\ " ++ fmt x ++ " = " ++ fmt ret ++ ")"

    fmt (MatExpr x cs) =
        "match " ++ fmt x ++ " with " ++ fmt cs

    fmt (TypExpr name def (Just cont)) =
        "data " ++ fmt name ++ " = " ++ fmt def ++ "; " ++ fmt cont

    fmt (JSExpr js) =
        "`" ++ (show . renderOneLine . renderJs) js ++ "`"

    fmt (RecExpr js) =
        fmt js

    fmt (VarExpr x) = fmt x

instance Fmt [(Patt, Expr)] where

    fmt ((patt, expr) : rs) =
        fmt patt ++ " -> " ++ fmt expr ++ "; " ++ fmt rs

    fmt [] = ""

------------------------------------------------------------------------------





