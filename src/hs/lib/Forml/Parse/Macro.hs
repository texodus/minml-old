------------------------------------------------------------------------------

-- | Notation parsing

------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Forml.Parse.Macro(
    macroPRec,
    filterP
) where

import Control.Arrow
import Control.Applicative
import Text.Parsec hiding ((<|>), many)

import Forml.AST
import Forml.Macro.LetPatt
import Forml.Macro.Replace
import Forml.Parse.Indent
import Forml.Parse.Val
import Forml.Parse.Patt
import Forml.Parse.Token

------------------------------------------------------------------------------

type PartMacro = (Expr -> Expr, MacList Expr)

macroPRec :: Parser Expr Expr -> MacList Expr -> Parser Expr PartMacro
macroPRec exprP = 
    merge rootP
    where

        merge f (MacList ms) =
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
            wrap pattP a m exs

        bothP m (Term (Arg a) exs) =
            wrap exprP a m exs

        bothP m (Term (Let a) exs) =
            try (wrap symP a m exs <|> wrap letPattP a m exs)
            where
                letPattP = LetPatt <$> pattP <* indented

        bothP _ _ = 
            error "Unimplemented"

        wrap p a m exs = 
            first . (.) . replace a <$> p <*> merge m exs

filterP :: MacList Expr -> MacList Expr

filterP (MacList (Term (Arg _) _ : ms)) =
    filterP (MacList ms)

filterP (MacList (x : ms)) =
    case filterP (MacList ms) of
        (MacList mss) -> MacList (x : mss)

filterP x =
    x

------------------------------------------------------------------------------
