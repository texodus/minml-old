------------------------------------------------------------------------------

-- Literals.

------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}

module Forml.AST.Lit (
    Lit( .. ),
) where

import Forml.Utils

data Lit where

    StrLit  :: String -> Lit
    NumLit  :: Double -> Lit

    deriving (Show, Eq, Ord)

instance Fmt Lit where fmt = show

------------------------------------------------------------------------------
