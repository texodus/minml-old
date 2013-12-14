------------------------------------------------------------------------------

-- Some examples of our new `Type Kind` vocabulary. Tidy!

--     Double  :: *
--     List    :: * -> *
--     (->)    :: * -> * -> *

------------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Minml.TypeCheck.Prelude where

import Minml.AST
import Minml.TypeCheck.Ass

------------------------------------------------------------------------------

tString  = TypeSym (TypeSymT Star "String" )
tBool    = TypeSym (TypeSymT Star "Boolean")
tDouble  = TypeSym (TypeSymT Star "Double" )
tArrow   = TypeSym (TypeSymT (Kfun Star (Kfun Star Star)) "->")

prelude :: [Ass]
prelude =

    [ "==" :>: TypeAbsT [Star] (TypeGen 0 `fn` TypeGen 0 `fn` tBool)
    , "&&" :>: TypeAbsT [] (tBool `fn` tBool `fn` tBool)
    , "+"  :>: TypeAbsT [] (tDouble `fn` tDouble `fn` tDouble)
    , "-"  :>: TypeAbsT [] (tDouble `fn` tDouble `fn` tDouble)
    , "parseInt" :>: TypeAbsT [Star] (TypeGen 0 `fn` tDouble) ]

infixr 4 `fn`
fn :: Type Kind -> Type Kind -> Type Kind
fn = TypeApp . TypeApp tArrow

------------------------------------------------------------------------------
