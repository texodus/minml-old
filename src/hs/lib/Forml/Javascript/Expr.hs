------------------------------------------------------------------------------

-- Converting our `Expr` type to JMacro's `JExpr` is simple for Vars, 
-- Abstractions and applications.

-- A let can be reduced to an application of an abstraction (though only 
-- in the backend - during type checking, we had generalization to contend
-- with).

-- `TypeExpr`s require are the hardest so far, requiring us to construct
-- a valid, curried constructor function for.  More to follow ...

-- The difficulties involved in matching are punted to the `ToJExpr Match` 
-- which will be defined later on, here we simple check that a match is
-- `true`.  Note the use of `scope` as a parameter to the datatype constructor
-- `Match`, allowing us to interpolate hygienic variables.

------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Forml.Javascript.Expr where

import Language.Javascript.JMacro

import Forml.AST
import Forml.Javascript.Val()
import Forml.Javascript.Ref
import Forml.Javascript.Curried
import Forml.Javascript.Match

------------------------------------------------------------------------------

instance ToJExpr Expr where

    toJExpr (VarExpr v) =
        toJExpr v

    toJExpr (AbsExpr (Sym sym) ex) = 
        intro sym id ex

    toJExpr (isInfix -> Just (x, o, y)) =
        InfixExpr o (toJExpr x) (toJExpr y)

    toJExpr (AppExpr f x) = 
        [jmacroE| `(f)`(`(x)`) |]

    toJExpr (LetExpr (Sym sym) ex expr) = [jmacroE| 

        `(intro sym (const ex) expr)`()

    |]

    toJExpr (TypExpr (TypeSymP sym) typsch expr) = [jmacroE|

        function() {
            var scheme = `(curriedFun sym typsch)`;
            return `(intro sym (const scheme) expr)`()
        }()

    |]

    toJExpr (MatExpr val ((patt, expr):cases)) = [jmacroE|

        (function() {
            var scope = this;
            var vall = `(val)`;
            if (`(Match vall patt scope)`)
                return `(expr)`
            else
                return `(MatExpr val cases)`;
        })()

    |]

    toJExpr (MatExpr _ []) = [jmacroE|
        (function() { throw "Pattern Match Exhausted"; })()
    |]

------------------------------------------------------------------------------
