------------------------------------------------------------------------------

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

------------------------------------------------------------------------------

pattP :: Parser s Patt
pattP =

    valPattP <|> conPattP <|> parens conPatsP <|> recPattP

    where
        valPattP = ValPatt <$> valP
        conPattP = flip ConPatt [] <$> typSymP
        conPatsP = ConPatt <$> typSymP <*> many pattP <|> pattP
        recPattP = RecPatt <$> recordP pattP

------------------------------------------------------------------------------
