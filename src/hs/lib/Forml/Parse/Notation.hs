------------------------------------------------------------------------------

-- | Notation parsing

------------------------------------------------------------------------------

module Forml.Parse.Notation (
    notationP
) where

import Control.Applicative

import Forml.AST
import Forml.AST.Replace
import Forml.Parse.Token
import qualified Forml.Parse.MacroToken as M

------------------------------------------------------------------------------

-- | Parses a notation string.  This returns a `MacroCell Expr` constructor,
--   once the body of the notation block has been parsed.

notationP :: Parser Expr (Expr -> MacroCell Expr)
notationP = 
    term <|> capture <|> sep <|> lastTerm
    where
        term = do
            f <- M.identifier <|> M.operator
            ((Token f . toMac) .) <$> notationP
        
        capture = do
            sym <- M.parens M.identifier
            (toArg sym .) <$> notationP

        sep = semi >> ((Sep . toMac) .) <$> notationP

        lastTerm = return Leaf

        toMac = Macro . (:[])

        toArg sym = 
            let escSym = '*' : sym
            in Arg escSym . toMac . replaceLet sym escSym

------------------------------------------------------------------------------
