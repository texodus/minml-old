------------------------------------------------------------------------------

-- The Literal parser is a simple parser which generates Lit values.  Here,
-- `stringLiteral` and `naturalOrFloat` come from `T.TokenParser`.

------------------------------------------------------------------------------

module Forml.Parse.Lit (
    litP
) where

import Control.Applicative

import Forml.AST.Lit
import Forml.Parse.Token

------------------------------------------------------------------------------

litP :: Parser s Lit
litP = stringL <|> numL
    where
        stringL = StrLit <$> stringLiteral
        numL    = NumLit . toDouble <$> naturalOrFloat

        toDouble (Left i)  = fromInteger i
        toDouble (Right f) = f

------------------------------------------------------------------------------
