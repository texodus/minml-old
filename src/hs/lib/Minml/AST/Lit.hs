------------------------------------------------------------------------------

-- | Literals.

------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}

module Minml.AST.Lit (
    Lit( .. ),
) where

import Minml.Utils

------------------------------------------------------------------------------

-- | Currently only supports Strings and Nums.  TODO Bools

data Lit where

    StrLit  :: String -> Lit
    NumLit  :: Double -> Lit

    deriving (Show, Eq, Ord)

instance Fmt Lit where fmt = show

------------------------------------------------------------------------------
