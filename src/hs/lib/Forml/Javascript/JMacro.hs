------------------------------------------------------------------------------
------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Forml.Javascript.JMacro (
    replace,
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

replace :: JMacro a => String -> JExpr -> a -> a
replace s x = 
    withHygiene (jfromGADT . composOp f . jtoGADT)
    where
        f :: JMGadt a -> JMGadt a
        f (JMGExpr (ValExpr (JVar (StrI z)))) | z == s = JMGExpr x
        f z = composOp f z

------------------------------------------------------------------------------
