------------------------------------------------------------------------------

-- | Expressions.

------------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Forml.AST.Expr (
    Expr(..)
) where

import Control.Arrow
import Language.Javascript.JMacro
import Text.PrettyPrint.Leijen.Text

import Forml.AST.Patt
import Forml.AST.Type
import Forml.AST.Val
import Forml.AST.Record
import Forml.AST.Replace
import Forml.Utils

------------------------------------------------------------------------------

-- | The Expression AST data type.  `App` is a function application; `Abs` is
--   function literal (function abstraction)

data Expr where
    LetExpr :: Sym  -> Expr -> Expr -> Expr
    AppExpr :: Expr -> Expr -> Expr
    AbsExpr :: Sym  -> Expr -> Expr
    VarExpr :: Val  -> Expr
    MatExpr :: Expr -> [(Patt, Expr)] -> Expr
    RecExpr :: Record Expr -> Expr
    JSExpr  :: JExpr -> Expr

    TypExpr :: TypeSym () -> TypeAbs () -> Expr -> Expr

    deriving (Eq, Ord, Show)

instance Replace String Expr where
    replace sym = replace sym . ValPatt . SymVal . Sym 

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

    -- Lets are special cases to handle recursion

    replace f t @ (ValPatt (SymVal f'')) (LetExpr (Sym f') a b) | f == f' =
        LetExpr f'' (replace f t . replace f (VarExpr (SymVal f'')) $ a) 
                    (replace f t . replace f (VarExpr (SymVal f'')) $ b)

    replace sym patt (LetExpr (Sym sym') a b) | sym == sym' =
        MatExpr (replace sym patt a) [(patt, replace sym patt b)] 

    replace f t @ (ValPatt (SymVal f'')) (AbsExpr (Sym f') ex) | f == f' =
        AbsExpr f'' (replace f t . replace f (VarExpr (SymVal f'')) $ ex)

    replace f _  (LetExpr (Sym f') a b) | f == f' = 
        LetExpr (Sym f') a b

    replace f ex (LetExpr f' a b) =
        LetExpr f' (replace f ex a) (replace f ex b)

    replace f ex (AppExpr a b) =
        AppExpr (replace f ex a) (replace f ex b)

    replace f (ValPatt (SymVal f'')) (VarExpr (SymVal (Sym f'))) | f == f' =
        VarExpr (SymVal f'')

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

instance Fmt Expr where

    fmt (LetExpr binding ex cont) =
        "let " ++ fmt binding ++ " = " ++ fmt ex ++ "; " ++ fmt cont

    fmt (AppExpr f x) =
        "(" ++ fmt f ++ " " ++ fmt x ++ ")"

    fmt (AbsExpr x ret) =
        "(\\ " ++ fmt x ++ " = " ++ fmt ret ++ ")"

    fmt (MatExpr x cs) =
        "match " ++ fmt x ++ " with " ++ fmt cs

    fmt (TypExpr name def cont) =
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





