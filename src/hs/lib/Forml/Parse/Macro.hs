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
import Forml.Parse.Indent
import Forml.Parse.Val
import Forml.Parse.Macro.LetPatt
import Forml.Parse.Patt
import Forml.Parse.Token
import Forml.Parse.Replace

------------------------------------------------------------------------------

type PartMacro = (Expr -> Expr, MacroList Expr)

macroPRec :: Parser Expr Expr -> MacroList Expr -> Parser Expr PartMacro
macroPRec exprP = 
    merge rootP
    where

        merge f (MacroList ms) =
            foldl (<|>) parserZero (fmap f ms)

        rootP (MacroTerm Sep xs) = 
            withSep (merge rootP xs)

        rootP (MacroLeaf x) = 
            return (const x, undefined)

        rootP x = 
            bothP rootP x

        scopeP (MacroTerm Sep xs) = 
            return (id, xs)

        scopeP (MacroLeaf _) = 
            parserZero

        scopeP x = 
            bothP scopeP x

        bothP m (MacroTerm Scope xs) = do
            (ap, cont) <- withCont (merge scopeP xs)
            first (ap .) <$> withSep (merge m cont)

        bothP m (MacroTerm (Token "<") exs) =
            reservedOp "<" >> merge m exs

        bothP m (MacroTerm (Token "</") exs) = 
            reservedOp "</" >> merge m exs

        bothP m (MacroTerm (Token x) exs) = 
            reserved x >> merge m exs

        bothP m (MacroTerm (Pat a) exs) = 
            wrap pattP a m exs

        bothP m (MacroTerm (Arg a) exs) =
            wrap exprP a m exs

        bothP m (MacroTerm (Let a) exs) =
            try (wrap symP a m exs <|> wrap letPattP a m exs)
            where
                letPattP = LetPatt <$> pattP <* indented

        bothP _ _ = 
            error "Unimplemented"

        wrap p a m exs = 
            first . (.) . replace a <$> p <*> merge m exs

filterP :: MacroList Expr -> MacroList Expr

filterP (MacroList (MacroTerm (Arg _) _ : ms)) =
    filterP (MacroList ms)

filterP (MacroList (x : ms)) =
    case filterP (MacroList ms) of
        (MacroList mss) -> MacroList (x : mss)

filterP x =
    x

------------------------------------------------------------------------------
