------------------------------------------------------------------------------

-- Parsec provides tokenization for free, given some basic rules about what
-- defines a keyword, operator, etc.  Record wild card will bind locally all
-- fields of the `TokenParser`, of which there are many (5).

-- (5) http://legacy.cs.uu.nl/daan/download/parsec/parsec.html#TokenParser

------------------------------------------------------------------------------

{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Forml.Parse.MacroToken where

import           Text.Parsec
import           Text.Parsec.Language
import qualified Text.Parsec.Token    as T

import Forml.AST

------------------------------------------------------------------------------

letterP = 
    lowerP <|> oneOf "ABSDEFGHIJKLMNOPQRSTUVWXYZ" <?> "letter"

lowerP = 
    oneOf "abcdefghijklmnopqrstuvwxyz" <?> "lower cased letter"

ohmlDef :: LanguageDef ()
ohmlDef = emptyDef {
    T.reservedNames   = keywords,
    T.reservedOpNames = concat ops,
    T.identStart      = lowerP <|> letterP <|> char '_',
    T.identLetter     = letterP <|> lowerP <|> digit <|> char '_',
    T.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~",
    T.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
}

type MParser = Parsec String ()

T.TokenParser { .. } = T.makeTokenParser ohmlDef

------------------------------------------------------------------------------
