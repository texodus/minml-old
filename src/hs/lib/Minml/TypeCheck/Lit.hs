------------------------------------------------------------------------------

-- Inference Itself
-- ----------------

-- Literals are trivial and require none of the machinery we just spent
-- 10 slides explaining.  Stay with me for a few minutes though ...

------------------------------------------------------------------------------

module Minml.TypeCheck.Lit where

import Minml.AST
import Minml.TypeCheck.Prelude

------------------------------------------------------------------------------

litCheck :: Lit -> Type Kind
litCheck (StrLit _)  = tString
litCheck (NumLit _)  = tDouble

------------------------------------------------------------------------------