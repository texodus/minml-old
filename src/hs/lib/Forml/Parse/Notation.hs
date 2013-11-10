------------------------------------------------------------------------------

-- | Notation parsing

------------------------------------------------------------------------------

module Forml.Parse.Notation (
    notationP
) where

import Control.Applicative
import Control.Lens

import Forml.AST
import Forml.Parse.Macro.Infer
import Forml.Parse.Macro.Scope
import Forml.Parse.Token
import qualified Forml.Parse.MacroToken as M

------------------------------------------------------------------------------

-- | Parses a notation string.  This returns a `MacroCell Expr` constructor,
--   once the body of the notation block has been parsed.

notationP :: Parser Expr (Expr -> Macro Expr)
notationP =
    (inferScope .) <$> (term <|> capture <|> sep <|> lastTerm)
    where
        term = do
            f <- M.identifier <|> M.operator <|> (M.reserved "λ" >> return "λ")
            (toMac (Token f) .) <$> notationP
        
        capture = do
            tok <- use uniqState
            uniqState %= (+1)
            sym <- M.parens M.identifier
            (inferCell ("$macro_" ++ show tok ++ "_") sym .) <$> notationP

        sep = semi >> (toMac Sep .) <$> notationP

        lastTerm = return MacroLeaf

------------------------------------------------------------------------------
