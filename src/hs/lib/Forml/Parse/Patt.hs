------------------------------------------------------------------------------

-- | Pattern parsing

------------------------------------------------------------------------------

module Forml.Parse.Patt (
    pattP, recPattP
) where

import Control.Applicative

import Forml.AST
import Forml.Parse.Record
import Forml.Parse.Token
import Forml.Parse.Type
import Forml.Parse.Val
import Forml.Parse.Indent

------------------------------------------------------------------------------

-- | Pattern parser

pattP :: Parser Patt
pattP =

    conPatsP <|> valPattP <|> parens pattP <|> recPattP

valPattP :: Parser Patt
valPattP = ValPatt <$> valP

recPattP :: Parser Patt
recPattP = RecPatt <$> recordP pattP

conPatsP :: Parser Patt
conPatsP = do
    tSym <- typSymP
    exs  <- many (indented >> inner)
    return $ case exs of
        [] -> ValPatt . ConVal . TypeSym $ tSym
        _ -> ConPatt tSym exs

inner :: Parser Patt
inner =
    valPattP <|> parens pattP <|> recPattP

------------------------------------------------------------------------------
