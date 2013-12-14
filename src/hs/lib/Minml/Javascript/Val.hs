------------------------------------------------------------------------------

------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Minml.Javascript.Val where

import Language.Javascript.JMacro

import Minml.AST
import Minml.Javascript.Lit()

------------------------------------------------------------------------------

instance ToJExpr Sym where
    toJExpr (Sym x) = jsv x

instance ToJExpr Val where
    toJExpr (SymVal s) = toJExpr s
    toJExpr (LitVal l) = toJExpr l
    toJExpr (ConVal (TypeSym (TypeSymP s))) = jsv s
    toJExpr (ConVal t) = error $ "FATAL: " ++ show t

------------------------------------------------------------------------------