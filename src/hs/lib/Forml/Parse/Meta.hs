------------------------------------------------------------------------------

-- | The Literal parser is a simple parser which generates Lit values.  Here,
--   `stringLiteral` and `naturalOrFloat` come from `T.TokenParser`.

------------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Forml.Parse.Meta where

import Text.Parsec

import Forml.AST.Meta
import Forml.Parse.Syntax

------------------------------------------------------------------------------

instance (Syntax a) => Syntax (Meta a) where
 
    syntax = do
        start <- getPosition
        expr <- syntax
        --end <- getPosition
        return $ Meta start "TODO: Fill me in!" expr

------------------------------------------------------------------------------
