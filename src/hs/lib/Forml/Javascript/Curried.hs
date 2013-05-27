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

curriedFun :: String -> TypeAbs () -> JExpr

curriedFun sym (TypeAbsP (isFun -> Just (_, fs))) = [jmacroE|

    function(x) {
        var args = [];
        `(args)`.push(x); 
        return `(curriedFun' sym args (TypeAbsP fs))`;
    }

|]

curriedFun sym ts = curriedFun' sym "" ts

curriedFun' :: (ToJExpr a) => String -> a -> TypeAbs () -> JExpr
curriedFun' sym args (TypeAbsP (isFun -> Just (_, fs))) = [jmacroE|

    function(x) {
        `(args)`.push(x); 
        return `(curriedFun' sym args (TypeAbsP fs))`;
    }

|]

curriedFun' sym args _ = [jmacroE| 

    {
        attrs: `(args)`,
        type: `(sym)`
    }

|]

------------------------------------------------------------------------------