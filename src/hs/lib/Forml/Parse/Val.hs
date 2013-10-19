------------------------------------------------------------------------------

-- | Parsers for `Val` and `Sym`.

--   "Alternative" combinator will evaluate to its second argument, iff
--   it's first argument fails and consumes no input - backtracking is explicit
--   via `try`.

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

-- | Sym parser

symP :: Parser s Sym
symP = try $ do
    ars <- use macros
    sym <- identifier
    if sym `elem` getReserved ars 
        then parserFail ("symbol (`" ++ show sym ++ "` is a keyword)\n\nDEBUG: " ++ show (getReserved ars) )
        else return $ Sym sym

getReserved :: MacroList a -> [String]

getReserved (MacroList (MacroTerm cell xs : ys)) =
    getReserved' cell ++ getReserved xs ++ getReserved (MacroList ys)

getReserved (MacroList (_ : ys)) = getReserved (MacroList ys)

getReserved (MacroList []) = []

getReserved' (Token x) = [x]
getReserved' _ = []

-- | Val parser

valP :: Parser s Val
valP =
    (SymVal <$> symP)
        <|> (LitVal <$> litP)
        <|> ConVal . TypeSym <$> typSymP

------------------------------------------------------------------------------
