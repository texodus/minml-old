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
import Control.Lens
import Text.Parsec hiding ((<|>))

import Forml.AST
import Forml.Parse.Lit
import Forml.Parse.Token
import Forml.Parse.Type

------------------------------------------------------------------------------

symP :: Parser s Sym
symP = try $ do
    ars <- use macros
    sym <- identifier
    case sym `elem` getReserved ars of
        True -> parserFail ("symbol (`" ++ show sym ++ "` is a keyword)\n\nDEBUG: " ++ show (getReserved ars) )
        False -> return $ Sym sym

getReserved :: [Macro a] -> [String]
getReserved (Token x zs : ys) = [x] ++ getReserved ys ++ getReserved zs
getReserved (Arg _ zs : xs) = getReserved xs ++ getReserved zs
getReserved _ = []

valP :: Parser s Val
valP =
    (SymVal <$> symP)
        <|> (LitVal <$> litP)
        <|> ConVal . TypeSym <$> typSymP

------------------------------------------------------------------------------
