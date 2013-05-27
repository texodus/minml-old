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
{-# LANGUAGE ExistentialQuantification #-}

module Forml.Javascript.Cases where

import Language.Javascript.JMacro

import Forml.AST
import Forml.Javascript.Val()
import Forml.Javascript.Match

------------------------------------------------------------------------------

data Cases = forall a. ToJExpr a => Cases JExpr [(Patt, a)]

instance ToStat Cases where

    toStat (Cases vall ((patt, expr) : cases)) = [jmacro|

        `(MatchBind vall patt)`;
        if (`(Match vall patt)`)
            return `(expr)`;
        return (function() { 
            `(Cases vall cases)`;
        })();

    |]

    toStat (Cases _ []) = [jmacro|
        throw "Pattern Match Exhausted";
    |]

------------------------------------------------------------------------------
