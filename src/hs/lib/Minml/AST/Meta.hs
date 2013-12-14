------------------------------------------------------------------------------

-- | Metadata

------------------------------------------------------------------------------

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

module Minml.AST.Meta where

import Control.Lens
import Text.Parsec

import Minml.Utils

------------------------------------------------------------------------------

data Meta a = Meta {
    _sourcePos :: SourcePos,
    _subExpr :: String,
    _node :: a
} deriving (Functor, Show)

makeLenses ''Meta

instance Eq a => Eq (Meta a) where

    x == y = 
        x^.node == y^.node

instance Ord a => Ord (Meta a) where

    compare x y = 
        compare (x^.node) (y^.node)

instance Fmt a => Fmt (Meta a) where

    fmt x = fmt (x^.node)

------------------------------------------------------------------------------
