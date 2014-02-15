------------------------------------------------------------------------------

-- | Pattern AST

------------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}

module Minml.AST.Patt (
    Patt(..)
) where

import Data.Serialize
import GHC.Generics

import Minml.AST.Type
import Minml.AST.Val
import Minml.AST.Record
import Minml.Utils

--------------------------------------------------------------------------------

-- | Patterns are either `Val` or `Con` (which is really a decon, amirite?).
--   Note we do not distinguish between literal and symbol matching,
--   because this is captured in the definition of `Val`

data Patt where

    ValPatt :: Val -> Patt
    ConPatt :: TypeSym () -> [Patt] -> Patt
    RecPatt :: Record Patt -> Patt

    deriving (Show, Eq, Ord, Read, Generic)

instance Serialize Patt

instance Fmt Patt where fmt = show

------------------------------------------------------------------------------

