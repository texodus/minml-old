------------------------------------------------------------------------------

-- | Metadata

------------------------------------------------------------------------------

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Minml.AST.Meta where

import Control.Applicative
import Control.Lens
import GHC.Read
import Text.Parsec
import Text.Parsec.Pos
import Text.ParserCombinators.ReadPrec
import Text.Read.Lex

import qualified Data.Serialize as S
import qualified GHC.Generics as G

import Minml.Utils

------------------------------------------------------------------------------

instance Read SourcePos where
   readPrec = parens $ prec 10 $ do
        Ident "SourcePos" <- lexP
        arg1 <- step readPrec
        arg2 <- step readPrec
        arg3 <- step readPrec
        return $ newPos arg1 arg2 arg3

data Meta a = Meta {
    _sourcePos :: SourcePos,
    _subExpr :: String,
    _node :: a
} deriving (Functor, Show, Read, G.Generic)

makeLenses ''Meta

instance S.Serialize a => S.Serialize (Meta a)

instance S.Serialize SourcePos where
    get = newPos <$> S.get <*> S.get <*> S.get
    put x = S.put (sourceLine x) >> S.put (sourceColumn x) >> S.put (sourceName x)

instance Eq a => Eq (Meta a) where
    x == y = 
        x^.node == y^.node

instance Ord a => Ord (Meta a) where
    compare x y = 
        compare (x^.node) (y^.node)

instance Fmt a => Fmt (Meta a) where
    fmt x = fmt (x^.node)

------------------------------------------------------------------------------
