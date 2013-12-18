------------------------------------------------------------------------------

-- | Pattern AST

------------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Minml.AST.Patt (
    Patt(..)
) where

import Minml.AST.Type
import Minml.AST.Val
import Minml.AST.Record
import Minml.AST.Replace
import Minml.Utils

--------------------------------------------------------------------------------

-- | Patterns are either `Val` or `Con` (which is really a decon, amirite?).
--   Note we do not distinguish between literal and symbol matching,
--   because this is captured in the definition of `Val`

data Patt where

    ValPatt :: Val -> Patt
    ConPatt :: TypeSym () -> [Patt] -> Patt
    RecPatt :: Record Patt -> Patt

    deriving (Show, Eq, Ord, Read)

instance Fmt Patt where fmt = show

instance Replace Patt Patt where

    replace f patt (ValPatt (SymVal (Sym t))) | f == t = patt
    replace f patt (ConPatt g xs) = ConPatt g (replace f patt `fmap` xs)
    replace _ _ p = p

------------------------------------------------------------------------------

