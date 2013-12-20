------------------------------------------------------------------------------

-- | Value AST

------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}

module Minml.AST.Val (
    module Minml.AST.Type,
    module Minml.AST.Lit,
    Val( .. ),
    Sym( .. )
) where

import GHC.Generics
import Data.Serialize

import Minml.AST.Lit
import Minml.AST.Type
import Minml.Utils

------------------------------------------------------------------------------

-- | Vals can be Symbols, Literals or Constructors (which differ from symbols
--   only in parsing).

data Val where

    SymVal  :: Sym -> Val
    LitVal  :: Lit -> Val
    ConVal  :: Type () -> Val

    deriving (Show, Eq, Ord, Read, Generic)

instance Serialize Val

instance Fmt Val where
    fmt (SymVal s) = fmt s
    fmt (LitVal s) = fmt s
    fmt (ConVal s) = fmt s

-- | `Sym` is just a type alias for String, representing a symbol name.

newtype Sym = Sym String deriving (Show, Eq, Ord, Read, Generic)

instance Serialize Sym

instance Fmt Sym where
    fmt (Sym s) = s

------------------------------------------------------------------------------

