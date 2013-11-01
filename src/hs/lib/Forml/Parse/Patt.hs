------------------------------------------------------------------------------

-- | Pattern parsing

------------------------------------------------------------------------------

module Forml.Parse.Patt (
    pattP
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

pattP :: Parser s Patt
pattP =

    conPatsP <|> valPattP <|> parens pattP <|> recPattP

    where
        valPattP = ValPatt <$> valP
        recPattP = RecPatt <$> recordP pattP
        conPatsP = do
            tSym <- typSymP
            exs  <- many (indented >> inner)
            return $ case exs of
                [] -> ValPatt . ConVal . TypeSym $ tSym
                _ -> ConPatt tSym exs

        inner =
            valPattP <|> parens pattP <|> recPattP


------------------------------------------------------------------------------
