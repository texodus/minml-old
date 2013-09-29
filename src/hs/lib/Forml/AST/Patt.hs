------------------------------------------------------------------------------

-- Patterns are either `Val` or `Con` (which is really a decon, amirite?).
-- Note we do not distinguish between literal and symbol matching,
-- because this is captured in the definition of `Val`

------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}

module Forml.AST.Patt (
    Patt(..)
) where

import Forml.AST.Type
import Forml.AST.Val
import Forml.AST.Record
import Forml.Utils

--------------------------------------------------------------------------------

data Patt where

    ValPatt :: Val -> Patt
    ConPatt :: TypeSym () -> [Patt] -> Patt
    RecPatt :: Record Patt -> Patt

    deriving (Show, Eq, Ord)

instance Fmt Patt where fmt = show

------------------------------------------------------------------------------

