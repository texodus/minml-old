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





