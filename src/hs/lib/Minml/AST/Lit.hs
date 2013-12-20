------------------------------------------------------------------------------

-- | Literals.

------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}

module Minml.AST.Lit (
    Lit( .. ),
) where

import Data.Serialize
import GHC.Generics

import Minml.Utils

------------------------------------------------------------------------------

-- | Currently only supports Strings and Nums.  TODO Bools

data Lit where

    StrLit  :: String -> Lit
    NumLit  :: Double -> Lit

    deriving (Show, Eq, Ord, Read, Generic)

instance Serialize Lit

instance Fmt Lit where fmt = show

------------------------------------------------------------------------------
