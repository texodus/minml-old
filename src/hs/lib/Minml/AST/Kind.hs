--------------------------------------------------------------------------------

-- | Type Kinds

--------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}

module Minml.AST.Kind (
    Kind( .. )
) where

import Minml.Utils

--------------------------------------------------------------------------------

-- | A `Kind` can be thought of as a type of types.

data Kind where
    Star :: Kind
    Kfun :: Kind -> Kind -> Kind
    deriving (Eq, Ord, Show, Read)

instance Fmt Kind where
    fmt Star = "*"
    fmt (Kfun k1 k2) = "(" ++ fmt k1 ++ " -> " ++ fmt k2 ++ ")"

--------------------------------------------------------------------------------
