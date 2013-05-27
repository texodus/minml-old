------------------------------------------------------------------------------

-- Parsec provides tokenization for free, given some basic rules about what
-- defines a keyword, operator, etc.  Record wild card will bind locally all
-- fields of the `TokenParser`, of which there are many (5).

-- (5) http://legacy.cs.uu.nl/daan/download/parsec/parsec.html#TokenParser

------------------------------------------------------------------------------

{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Forml.Parse.Token where

import           Text.Parsec
import           Text.Parsec.Language
import qualified Text.Parsec.Token    as T

import Forml.AST

------------------------------------------------------------------------------

ohmlDef :: LanguageDef SourcePos
ohmlDef = emptyDef {
    T.reservedNames   = keywords,
    T.reservedOpNames = concat ops,
    T.identStart      = lower <|> char '_'
}

type Parser = Parsec String SourcePos

T.TokenParser { .. } = T.makeTokenParser ohmlDef

------------------------------------------------------------------------------
