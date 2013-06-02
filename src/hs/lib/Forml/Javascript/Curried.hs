------------------------------------------------------------------------------

-- We saw two new constructs during `Expr` rendering.  `curriedFun` is to
-- artificially construct a curried version of a data constructors.  We're
-- going to use the first call in the curried chain to introduce an array to
-- capture arguments through the partial application.  This means we need to
-- treat constructor functions and empty constructors differently.

------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

module Forml.Javascript.Curried (
    curriedFun
) where

import Language.Javascript.JMacro

import Forml.AST

------------------------------------------------------------------------------

curriedFun :: TypeAbs () -> JExpr -> JExpr
curriedFun t typ =  curriedFun' [] t
    where
        curriedFun' args (TypeAbsP (isFun -> Just (_, fs))) = [jmacroE|

            function(x) {
                return `(curriedFun' (args ++ [x]) (TypeAbsP fs))`;
            }

        |]

        curriedFun' args _ = [jmacroE| 

            new `(ApplExpr typ args)`

        |]

------------------------------------------------------------------------------