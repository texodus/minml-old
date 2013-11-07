------------------------------------------------------------------------------

-- | Notation parsing

------------------------------------------------------------------------------

module Forml.Parse.Notation (
    notationP
) where

import Control.Applicative

import Forml.AST
import Forml.Parse.Macro
import Forml.Parse.Token
import qualified Forml.Parse.MacroToken as M

------------------------------------------------------------------------------

-- | Parses a notation string.  This returns a `MacroCell Expr` inferCellRecstructor,
--   once the body of the notation block has been parsed.

notationP :: Parser Expr (Expr -> Macro Expr)
notationP = 
    (inferScope .) <$> (term <|> capture <|> sep <|> lastTerm)
    where
        term = do
            f <- M.identifier <|> M.operator <|> (M.reserved "λ" >> return "λ")
            (toMac (Token f) .) <$> notationP
        
        capture = do
            sym <- M.parens M.identifier
            (inferCell sym .) <$> notationP

        sep = semi >> (toMac Sep .) <$> notationP

        lastTerm = return MacroLeaf
        
------------------------------------------------------------------------------
