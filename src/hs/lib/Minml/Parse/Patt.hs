------------------------------------------------------------------------------

-- | Pattern parsing

------------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Minml.Parse.Patt() where

import Control.Applicative

import Minml.AST
import Minml.Parse.Record()
import Minml.Parse.Syntax
import Minml.Parse.Token
import Minml.Parse.Type()
import Minml.Parse.Val()
import Minml.Parse.Indent

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
