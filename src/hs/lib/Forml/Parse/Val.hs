------------------------------------------------------------------------------

-- Parsers for `Val` and `Sym`.

-- "Alternative" combinator will evaluate to its second argument, iff
-- it's first argument fails and consumes no input - backtracking is explicit
-- via `try`.

------------------------------------------------------------------------------

module Forml.Parse.Val (
    symP,
    valP
) where

import Control.Applicative

import Forml.AST
import Forml.Parse.Lit
import Forml.Parse.Token
import Forml.Parse.Type

------------------------------------------------------------------------------

symP :: Parser Sym
symP = Sym <$> identifier

valP :: Parser Val
valP =
    (SymVal <$> symP)
        <|> (LitVal <$> litP)
        <|> ConVal . TypeSym <$> typSymP

------------------------------------------------------------------------------
