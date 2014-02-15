------------------------------------------------------------------------------

-- | Converting our `Expr` type to JMacro's `JExpr` is simple for Vars, 
--   Abstractions and applications.

--   A let can be reduced to an application of an abstraction (though only 
--   in the backend - during type checking, we had generalization to contend
--   with).

--   `TypeExpr`s require are the hardest so far, requiring us to construct
--   a valid, curried constructor function for.  More to follow ...

--   The difficulties involved in matching are punted to the `ToJExpr Match` 
--   which will be defined later on, here we simple check that a match is
--   `true`.  Note the use of `scope` as a parameter to the datatype constructor
--   `Match`, allowing us to interpolate hygienic variables.

------------------------------------------------------------------------------

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ExistentialQuantification #-}

module Minml.Javascript.Cases (
    Cases( .. )
) where

import Language.Javascript.JMacro

import Minml.AST
import Minml.Replace.Base

import Minml.Javascript.Val()
import Minml.Javascript.Match
import Minml.Javascript.JMacro

------------------------------------------------------------------------------

-- | Represents a pattern match.

data Cases = forall a. ToJExpr a => Cases JExpr [(Patt, a)]

instance ToStat Cases where

    toStat (Cases vall ((patt, expr) : cases)) = 
        foldl g mat (getBindings vall patt)
        where
            g expr' (i, ex) = replace i ex expr'
            mat = case minify (toJExpr (Match vall patt)) of
                ValExpr (JVar (StrI "true")) -> [jmacro|
                    return `(expr)`;
                |]

                matchIf -> [jmacro|
                    if (`(matchIf)`) return `(expr)`;
                    `(Cases vall cases)`;
                |]

    toStat (Cases _ []) = [jmacro|
        throw "Pattern Match Exhausted";
    |]

------------------------------------------------------------------------------
