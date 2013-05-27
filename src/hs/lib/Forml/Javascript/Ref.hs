------------------------------------------------------------------------------

-- This enables us also to do one of the only manual processes necessary -
-- at some point we'll need to introduce new variables into scope,
-- with a specific name and value.

------------------------------------------------------------------------------

{-# LANGUAGE QuasiQuotes #-}

module Forml.Javascript.Ref where

import Language.Javascript.JMacro

------------------------------------------------------------------------------

ref :: String -> JExpr
ref = ValExpr . JVar . StrI

intro :: (ToJExpr a, ToJExpr b) => String -> (JExpr -> a) -> b -> JExpr
intro sym f expr = [jmacroE| 
    function(arg) {                              // (1)
        `(DeclStat (StrI sym) Nothing)`;         // (2)
        `(ref sym)` = `(f arg)`;                 // (3)
        return `(expr)`;
    }
|]

------------------------------------------------------------------------------
