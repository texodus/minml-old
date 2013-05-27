------------------------------------------------------------------------------

-- Inference Itself
-- ----------------

-- Literals are trivial and require none of the machinery we just spent
-- 10 slides explaining.  Stay with me for a few minutes though ...

------------------------------------------------------------------------------

module Forml.TypeCheck.Lit where

import Forml.AST
import Forml.TypeCheck.Prelude

------------------------------------------------------------------------------

litCheck :: Lit -> Type Kind
litCheck (StrLit _)  = tString
litCheck (NumLit _)  = tDouble

------------------------------------------------------------------------------