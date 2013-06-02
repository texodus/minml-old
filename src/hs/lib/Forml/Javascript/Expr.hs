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
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Forml.Javascript.Expr where

import Language.Javascript.JMacro

import Forml.AST
import Forml.Javascript.Cases
import Forml.Javascript.Curried
import Forml.Javascript.JMacro
import Forml.Javascript.Type()
import Forml.Javascript.Val()

------------------------------------------------------------------------------

instance ToJExpr Expr where

    toJExpr (VarExpr v) =
        toJExpr v

    toJExpr (AbsExpr (Sym sym) ex) = [jmacroE|

        function(arg) {
            `(replace sym arg $ toStat ex)`;
        }

    |]

    toJExpr (isInfix -> Just (x, o, y)) =
        InfixExpr o (toJExpr x) (toJExpr y)

    toJExpr (AppExpr f x) = 
        [jmacroE| `(f)`(`(x)`) |]

    toJExpr x = [jmacroE|

        (function() {
            `(x)`;
        })()

    |]

instance ToStat Expr where

    toStat (LetExpr (Sym sym) ex expr) = [jmacro| 

        var x = `(replace sym x $ toJExpr ex)`;
        `(replace sym x $ toStat expr)`;

    |]

    toStat (TypExpr (TypeSymP sym) typsch expr) = [jmacro|

        `(DeclStat (StrI sym) Nothing)`;
        var x = `(typsch)`;
        `(jsv sym)` = `(curriedFun typsch x)`;
        `(jsv sym)`.__type__ = x;
        `(expr)`;

    |]

    toStat (MatExpr val cases) = [jmacro|

        var x = `(val)`;
        `(Cases x cases)`;

    |]

    toStat x = [jmacro|
        return `(x)`;
    |]

------------------------------------------------------------------------------
