------------------------------------------------------------------------------

-- | Type Kinds

------------------------------------------------------------------------------

{-# LANGUAGE GADTs         #-}
{-# LANGUAGE DeriveGeneric #-}


module Minml.AST.Kind (
    Kind( .. )
) where

import Minml.Utils
import Data.Serialize
import GHC.Generics

------------------------------------------------------------------------------

-- | A `Kind` can be thought of as a type of types.

data Kind where
    Star :: Kind
    Kfun :: Kind -> Kind -> Kind
    deriving (Eq, Ord, Show, Read, Generic)

instance Serialize Kind

instance Fmt Kind where
    fmt Star = "*"
    fmt (Kfun k1 k2) = "(" ++ fmt k1 ++ " -> " ++ fmt k2 ++ ")"

------------------------------------------------------------------------------
