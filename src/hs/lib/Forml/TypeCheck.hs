------------------------------------------------------------------------------

-- CHAPTER FOUR: Type Inference
-- ============================

-- Type checking is a broad subject - this is a very simple implementation
-- which neverthless allows for much of the expressiveness you may be
-- famliar with in Haskell.  The strategy basically looks like this:

--    * We need to calculate the `Kind` of each type, which we need to
--      track parametric polymorphism in our datatype constructors.

--    * Introduce an `Assumption` for every symbol in a program,
--      binding the symbol's name & scope to it's type, which may be a
--      variable.

--    * Walk the expression tree using these assumptions, and introduce a
--      `Substitution` for every usage of a symbol, to fit its context.

--    * Where we assign or abstract values, we need to `Unify` two types,
--      by calculating the most general new substitutions which make them
--      equivalent.

--    * Where we want to use a variable in a polymorphic way, we'll need 
--      to `Generalize` some assumptions so they can be instantiated with
--      a fresh set of type variables for every usage. 

------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}

module Forml.TypeCheck where

import Control.Monad.State
import Control.Arrow
import Control.Lens

import Forml.AST
import Forml.TypeCheck.Prelude
import Forml.TypeCheck.TypeCheck
import Forml.TypeCheck.Expr

------------------------------------------------------------------------------

typeCheck :: Expr -> Either Err Expr
typeCheck ex = do
	runStateT (infer ex) st
	return ex

	where
		st :: TypeCheckState
		st = (ass .~ prelude) newState
 
------------------------------------------------------------------------------