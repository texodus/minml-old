------------------------------------------------------------------------------

-- | Expressions.

------------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances  #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Minml.AST.Expr (
    Expr(..)
) where

import Data.Monoid
import Data.Serialize
import Data.String.Utils
import Language.Javascript.JMacro
import Text.PrettyPrint.Leijen.Text

import Minml.AST.Patt
import Minml.AST.Record
import Minml.AST.Type
import Minml.AST.Val
import Minml.Utils

import GHC.Generics

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
    AnnExpr :: Expr -> Type () -> Maybe Expr -> Expr
    JSExpr  :: JExpr -> Expr

    TypExpr :: TypeSym () -> TypeAbs () -> Maybe Expr -> Expr

    deriving (Eq, Ord, Show, Read, Generic)

instance Serialize Expr

instance Serialize JExpr where
    get = do
        result <- get
        return $ case parseJME $ replace "jmId_###_" "qmId_" result of
            Left x  -> error (show x ++ "\n\n" ++ result)
            Right x -> x

    put x = put . show . renderOneLine . renderPrefixJs "###" $ x

instance Read JExpr where
    readsPrec _ _ = undefined

instance Monoid (Maybe Expr) where

    mempty = Nothing

    mappend (Just (LetExpr a b c)) x = Just (LetExpr a b (c `mappend` x))
    mappend (Just (TypExpr a b c)) x = Just (TypExpr a b (c `mappend` x))
    mappend (Just (AnnExpr a b c)) x = Just (AnnExpr a b (c `mappend` x))
    mappend (Just _) _ = error "Cannot append terminal parses"
    mappend _ y = y

instance Fmt (Maybe Expr) where
    fmt (Just ex) = "; " ++ fmt ex
    fmt Nothing = ""

instance Fmt Expr where

    fmt (LetExpr binding ex cont) =
        "let " ++ fmt binding ++ " = " ++ fmt ex ++ fmt cont

    fmt (AppExpr f x) =
        "(" ++ fmt f ++ " " ++ fmt x ++ ")"

    fmt (AbsExpr x ret) =
        "(\\ " ++ fmt x ++ " = " ++ fmt ret ++ ")"

    fmt (MatExpr x cs) =
        "match " ++ fmt x ++ " with " ++ fmt cs

    fmt (TypExpr name def cont) =
        "data " ++ fmt name ++ " = " ++ fmt def ++ fmt cont

    fmt (JSExpr js) =
        "`" ++ (show . renderOneLine . renderJs) js ++ "`"

    fmt (RecExpr js) =
        fmt js

    fmt (AnnExpr a b cont) = 
        fmt a ++ " : " ++ fmt b ++ fmt cont

    fmt (VarExpr x) = fmt x

instance Fmt [(Patt, Expr)] where

    fmt ((patt, expr) : rs) =
        fmt patt ++ " -> " ++ fmt expr ++ "; " ++ fmt rs

    fmt [] = ""

------------------------------------------------------------------------------





