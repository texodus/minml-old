------------------------------------------------------------------------------

-- | Pattern parsing

------------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Forml.Parse.Patt() where

import Control.Applicative

import Forml.AST
import Forml.Parse.Record()
import Forml.Parse.Syntax
import Forml.Parse.Token
import Forml.Parse.Type()
import Forml.Parse.Val()
import Forml.Parse.Indent

------------------------------------------------------------------------------

-- | Pattern parser

instance Syntax Patt where

    syntax = conPatsP <|> valPattP <|> parens syntax <|> recPattP

valPattP :: Parser Patt
valPattP = ValPatt <$> syntax

recPattP :: Parser Patt
recPattP = RecPatt <$> syntax

conPatsP :: Parser Patt
conPatsP = do
    tSym <- syntax
    exs  <- many (indented >> inner)
    return $ case exs of
        [] -> ValPatt . ConVal . TypeSym $ tSym
        _ -> ConPatt tSym exs

inner :: Parser Patt
inner =
    valPattP <|> parens syntax <|> recPattP

------------------------------------------------------------------------------
