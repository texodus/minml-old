------------------------------------------------------------------------------

------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}

module Forml.AST.Val (
    module Forml.AST.Type,
    module Forml.AST.Lit,
    Val( .. ),
    Sym( .. )
) where

import Forml.AST.Lit
import Forml.AST.Type
import Forml.Utils

------------------------------------------------------------------------------

data Val where

    SymVal  :: Sym -> Val
    LitVal  :: Lit -> Val
    ConVal  :: Type () -> Val

    deriving (Show, Eq, Ord)

newtype Sym = Sym String deriving (Show, Eq, Ord)

instance Fmt Val where
    fmt (SymVal s) = fmt s
    fmt (LitVal s) = fmt s
    fmt (ConVal s) = fmt s

instance Fmt Sym where
    fmt (Sym s) = s

------------------------------------------------------------------------------

