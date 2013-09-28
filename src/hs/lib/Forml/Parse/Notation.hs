------------------------------------------------------------------------------

-- Converts a notation string to a `Parser Expr Expr` 

------------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Forml.Parse.Notation (
    notationP
) where

import Control.Applicative
import Forml.AST
import Forml.AST.Replace
import Forml.Parse.Token
import qualified Forml.Parse.MacroToken as M

------------------------------------------------------------------------------

notationP :: Parser Expr (Expr -> Macro Expr)
notationP = 
    term <|> capture <|> lastTerm
    where
        term = do
            f <- M.identifier <|> M.operator <|> M.semi
            ((Token f . (:[])) .) <$> notationP
        
        capture = do
            sym <- M.parens M.identifier
            (toArg sym .) <$> notationP

        lastTerm = return Leaf

        toArg sym = 
            let escSym = '*' : sym
            in Arg escSym . (:[]) . replaceLet sym escSym

------------------------------------------------------------------------------
