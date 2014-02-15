------------------------------------------------------------------------------

-- | Notation parsing

------------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Minml.Parse.Macro(
    macroPRec,
    filterP
) where

import Control.Arrow
import Control.Applicative
import Text.Parsec hiding ((<|>), many)

import Minml.AST
import Minml.Macro.LetPatt
import Minml.Parse.Indent
import Minml.Parse.Syntax
import Minml.Parse.Val()
import Minml.Parse.Patt()
import Minml.Parse.Token
import Minml.Replace

------------------------------------------------------------------------------

-- TODO this can be simplified alot

type PartMacro a = (a -> a, MacTree a)

macroPRec :: (Syntax a, Replace LetPatt a, Replace Sym a, Replace Expr a, Replace Patt a, Replace (Type ()) a, Syntax Expr) => 
    MacTree a -> Parser (PartMacro a)

macroPRec = 
    merge rootP
    where

        merge f (MacTree ms) =
            foldl (<|>) parserZero (fmap f ms)

        rootP (Term Sep xs) = 
            withSep (merge rootP xs)

        rootP (Leaf x) = 
            return (const x, undefined)

        rootP x = 
            bothP rootP x

        scopeP (Term Sep xs) = 
            return (id, xs)

        scopeP (Leaf _) = 
            parserZero

        scopeP x = 
            bothP scopeP x

        bothP m (Term Scope xs) = do
            (ap, cont) <- withCont (merge scopeP xs)
            first (ap .) <$> withSep (merge m cont)

        bothP m (Term (Token "<") exs) =
            reservedOp "<" >> merge m exs

        bothP m (Term (Token "</") exs) = 
            reservedOp "</" >> merge m exs

        bothP m (Term (Token x) exs) = 
            reserved x >> merge m exs

        bothP m (Term (Pat a) exs) = 
            wrap (syntax :: Parser Patt) a m exs

        bothP m (Term (Arg a) exs) =
            wrap (syntax :: Parser Expr) a m exs

        bothP m (Term (Typ a) exs) =
            wrap (syntax :: Parser (Type ())) a m exs

        bothP m (Term (Let a) exs) =
            try (wrap (syntax :: Parser Sym) a m exs <|> wrap letPattP a m exs)
            where
                letPattP = LetPatt <$> syntax <* indented

        bothP _ _ = 
            error "Unimplemented"

        wrap p a m exs = 
            first . (.) . replace a <$> p <*> merge m exs

filterP :: MacTree Expr -> MacTree Expr

filterP (MacTree (Term (Arg _) _ : ms)) =
    filterP (MacTree ms)

filterP (MacTree (x : ms)) =
    case filterP (MacTree ms) of
        (MacTree mss) -> MacTree (x : mss)

filterP x =
    x

------------------------------------------------------------------------------
