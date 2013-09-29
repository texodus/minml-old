------------------------------------------------------------------------------

-- | Literals.

------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}

module Forml.AST.Lit (
    Lit( .. ),
) where

import Forml.Utils

------------------------------------------------------------------------------

-- | Currently only supports Strings and Nums.  TODO Bools

data Lit where

    StrLit  :: String -> Lit
    NumLit  :: Double -> Lit

    deriving (Show, Eq, Ord)

instance Fmt Lit where fmt = show

------------------------------------------------------------------------------
