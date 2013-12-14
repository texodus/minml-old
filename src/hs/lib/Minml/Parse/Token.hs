------------------------------------------------------------------------------

-- | Parsec provides tokenization for free, given some basic rules about what
--   defines a keyword, operator, etc.  Record wild card will bind locally all
--   fields of the `TokenParser`, of which there are many (5).

--   (5) http://legacy.cs.uu.nl/daan/download/parsec/parsec.html#TokenParser

------------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances  #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Minml.Parse.Token where

import           Control.Lens
import           Control.Monad.State
import           Text.Parsec
import           Text.Parsec.Language
import qualified Text.Parsec.Token    as T

import Minml.AST

------------------------------------------------------------------------------

ohmlDef :: LanguageDef MacroState
ohmlDef = emptyDef {
    T.reservedNames   = keywords,
    T.reservedOpNames = "=" : concat ops ,
    T.commentLine     = "--",
    T.identStart      = lower <|> char '_',
    T.identLetter     = upper <|> lower <|> digit <|> char '_',
    T.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~",
    T.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
}

instance MonadState st (Parsec tok st) where
    get = getState
    put = setState

data MacroState = MacroState {
    _sourcePos  :: SourcePos,
    _macros     :: MacList Expr,
    _tailMacros :: MacList Expr,
    _uniqState  :: Int
}

makeLenses ''MacroState

antiQuote :: Parser ()
antiQuote = try $ do
    _ <- string "`"
    notFollowedBy (string "`")
    whiteSpace

type Parser = Parsec String MacroState

T.TokenParser { .. } = T.makeTokenParser ohmlDef

------------------------------------------------------------------------------
