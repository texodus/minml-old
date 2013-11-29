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

pattP :: Parser Expr Patt
pattP =

    conPatsP <|> valPattP <|> parens pattP <|> recPattP

valPattP :: Parser Expr Patt
valPattP = ValPatt <$> valP

recPattP :: Parser Expr Patt
recPattP = RecPatt <$> recordP pattP

conPatsP :: Parser Expr Patt
conPatsP = do
    tSym <- typSymP
    exs  <- many (indented >> inner)
    return $ case exs of
        [] -> ValPatt . ConVal . TypeSym $ tSym
        _ -> ConPatt tSym exs

inner :: Parser Expr Patt
inner =
    valPattP <|> parens pattP <|> recPattP

------------------------------------------------------------------------------
