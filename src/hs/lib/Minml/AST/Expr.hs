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
    JSExpr  :: JExpr -> Expr

    TypExpr :: TypeSym () -> TypeAbs () -> Maybe Expr -> Expr

    deriving (Eq, Ord, Show, Read, Generic)


--class Minml a where
--    mtoGadt   :: a -> MGadt a
--    mfromGadt :: MGadt a -> a

--instance Minml Expr where
--    mtoGadt = MGExpr
--    mfromGadt (MGExpr x) = x
--    mfromGadt _ = error "impossible"


---- | Union type to allow regular traversal by compos.
--data MGadt a where
--    MGExpr :: Expr -> MGadt Expr

--composOpM :: (Compos t, Monad m) =>
--    (forall a. t a -> m (t a)) -> t b -> m (t b)
--composOpM = compos return ap

--composOpFold :: Compos t => b -> (b -> b -> b) ->
--    (forall a. t a -> b) -> t c -> b

--composOpFold z c f =
--    unC . compos (\_ -> C z) (\(C x) (C y) -> C (c x y)) (C . f)

--newtype C b a = C { unC :: b }

--instance Compos MGadt where
--    compos = mcompos

--mcompos :: forall m c. (forall a. a -> m a) -> (forall a b. m (a -> b) -> m a -> m b) -> (forall a. MGadt a -> m (MGadt a)) -> MGadt c -> m (MGadt c)

--mcompos ret app f' v = case v of
--    MGExpr v' -> ret MGExpr `app` case v' of
--        LetExpr s e x -> ret (LetExpr s) `app` f e `app` (f `fmap` x)
--        --AppExpr :: Expr -> Expr -> Expr
--        --AbsExpr :: Sym  -> Expr -> Expr
--        --VarExpr :: Val  -> Expr
--        --MatExpr :: Expr -> [(Patt, Expr)] -> Expr
--        --RecExpr :: Record Expr -> Expr
--        --JSExpr  :: JExpr -> Expr

--        --TypExpr :: TypeSym () -> TypeAbs () -> Maybe Expr -> Expr

--        --ValExpr e -> ret ValExpr `app` f e
--        --SelExpr e e' -> ret SelExpr `app` f e `app` f e'
--        --IdxExpr e e' -> ret IdxExpr `app` f e `app` f e'
--        --InfixExpr o e e' -> ret (InfixExpr o) `app` f e `app` f e'
--        --PPostExpr b o e -> ret (PPostExpr b o) `app` f e
--        --IfExpr e e' e'' -> ret IfExpr `app` f e `app` f e' `app` f e''
--        --NewExpr e -> ret NewExpr `app` f e
--        --ApplExpr e xs -> ret ApplExpr `app` f e `app` mapM' f xs
--        --AntiExpr _ -> ret v'
--        --TypeExpr b e t -> ret (TypeExpr b) `app` f e `app` ret t
--        --UnsatExpr _ -> ret v'

--    where
--        mapM' :: forall a. (a -> m a) -> [a] -> m [a]
--        mapM' g = foldr (app . app (ret (:)) . g) (ret [])
--        f :: forall b. Minml b => b -> m b
--        f x = ret mfromGadt `app` f' (mtoGadt x)
















instance Serialize Expr

instance Serialize JExpr where
    get = do
        result <- get
        return $ case parseJME result of
            Left x  -> error (show x ++ "\n\n" ++ result)
            Right x -> x

    put x = put . show . renderOneLine . renderJs $ x

instance Read JExpr where
    readsPrec _ _ = undefined

instance Monoid (Maybe Expr) where

    mempty = Nothing

    mappend (Just (LetExpr a b c)) x = Just (LetExpr a b (c `mappend` x))
    mappend (Just (TypExpr a b c)) x = Just (TypExpr a b (c `mappend` x))
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

    fmt (VarExpr x) = fmt x

instance Fmt [(Patt, Expr)] where

    fmt ((patt, expr) : rs) =
        fmt patt ++ " -> " ++ fmt expr ++ "; " ++ fmt rs

    fmt [] = ""

------------------------------------------------------------------------------





