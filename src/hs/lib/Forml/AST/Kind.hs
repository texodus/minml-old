--------------------------------------------------------------------------------

-- A `Kind` can be thought of as a type of types.

--------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}

module Forml.AST.Kind (
    Kind( .. )
) where

import Forml.Utils

--------------------------------------------------------------------------------

data Kind where
    Star :: Kind
    Kfun :: Kind -> Kind -> Kind
    deriving (Eq, Ord, Show)

instance Fmt Kind where
    fmt Star = "*"
    fmt (Kfun k1 k2) = "(" ++ fmt k1 ++ " -> " ++ fmt k2 ++ ")"

--------------------------------------------------------------------------------
