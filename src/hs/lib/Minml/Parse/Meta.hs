------------------------------------------------------------------------------

-- | The Literal parser is a simple parser which generates Lit values.  Here,
--   `stringLiteral` and `naturalOrFloat` come from `T.TokenParser`.

------------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Minml.Parse.Meta where

import Text.Parsec

import Minml.AST.Meta
import Minml.Parse.Syntax

------------------------------------------------------------------------------

instance (Syntax a) => Syntax (Meta a) where
 
    syntax = do
        start <- getPosition
        expr <- syntax
        --end <- getPosition
        return $ Meta start "TODO: Fill me in!" expr

------------------------------------------------------------------------------
