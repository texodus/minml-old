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

import Data.Monoid
import Language.Javascript.JMacro

import Forml.AST
import Forml.Javascript.Val()
import Forml.Javascript.Ref
import Forml.Javascript.Curried
import Forml.Javascript.Match

------------------------------------------------------------------------------

instance ToJExpr (TypeAbs ()) where


    toJExpr (TypeAbsP typ) = 

        ValExpr (JFunc (toIdent `fmap` ids) (body ids))

        where
            ids = [(0 :: Integer) .. countIds typ]
            
            body (arg : args) =
                [jmacro| this[`(arg)`] = `(ValExpr . JVar . toIdent $ arg)`; |] 
                    `mappend` body args
            body [] = mempty

            toIdent = StrI . ("$" ++) . show

            countIds (isFun -> Just (_, fs)) = 1 + countIds fs
            countIds (isFun -> Nothing) = -1
            countIds _ = error "FATAL: countIds"


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
            `(DeclStat (StrI sym) Nothing)`;
            var x = `(typsch)`;
            `(ref sym)` = `(curriedFun typsch x)`;
            `(ref sym)`.__type__ = x;
            return `(expr)`;
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
