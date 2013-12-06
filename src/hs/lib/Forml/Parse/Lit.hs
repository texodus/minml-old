------------------------------------------------------------------------------

-- | The Literal parser is a simple parser which generates Lit values.  Here,
--   `stringLiteral` and `naturalOrFloat` come from `T.TokenParser`.

------------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Forml.Parse.Lit where

import Control.Applicative

import Forml.AST.Lit
import Forml.Parse.Token
import Forml.Parse.Syntax

------------------------------------------------------------------------------

instance Syntax Lit where
 
    syntax =
        stringL <|> numL
 
        where
            stringL = StrLit <$> stringLiteral
            numL    = NumLit . toDouble <$> naturalOrFloat

            toDouble (Left i)  = fromInteger i
            toDouble (Right f) = f

------------------------------------------------------------------------------
