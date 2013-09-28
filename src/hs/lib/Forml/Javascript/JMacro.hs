------------------------------------------------------------------------------
------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Forml.Javascript.JMacro (
    minify
) where

import Language.Javascript.JMacro

import Forml.Javascript.Type()
import Forml.Javascript.Val()

------------------------------------------------------------------------------

minify :: JExpr -> JExpr
minify orig @ (InfixExpr "&&" x y) = case (minify x, minify y) of
    (ValExpr (JVar (StrI "true")), z) -> z
    (z, ValExpr (JVar (StrI "true"))) -> z
    _ -> orig
minify x = x

------------------------------------------------------------------------------
