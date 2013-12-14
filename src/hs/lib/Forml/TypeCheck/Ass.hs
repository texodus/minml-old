------------------------------------------------------------------------------

-- Assumptions

-- Assumptions are used to keep track of the type of symbols, but only at
-- the time they are added to the `[Ass]`.  We'll need to apply the
-- current `Subst` to know the real type of a symbol given the total
-- information known so far.

-- When we encounter a symbol, we just look it up in the list of assumptions.
-- This is a convenient opportunity to check for undefined symbols!

------------------------------------------------------------------------------

module Forml.TypeCheck.Ass where

import Forml.AST
import Forml.TypeCheck.Subst

------------------------------------------------------------------------------

data Ass = String :>: TypeAbs Kind

instance Substitute Ass where
    apply s (i :>: sc) = i :>: apply s sc
    getVars (_ :>: sc) = getVars sc

------------------------------------------------------------------------------
