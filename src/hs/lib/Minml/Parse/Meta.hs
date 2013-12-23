------------------------------------------------------------------------------

-- | The Literal parser is a simple parser which generates Lit values.  Here,
--   `stringLiteral` and `naturalOrFloat` come from `T.TokenParser`.

------------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Minml.Parse.Meta where

import Text.Parsec

import Minml.AST.Meta
import Minml.Parse.Syntax
import Minml.Parse.Token

------------------------------------------------------------------------------

instance (Syntax a) => Syntax (Meta a) where
    syntax = withMeta syntax

withMeta :: Parser a -> Parser (Meta a)
withMeta p = do
    start <- getPosition
    expr <- p
    --end <- getPosition
    return $ Meta start "TODO: Fill me in!" expr

------------------------------------------------------------------------------
