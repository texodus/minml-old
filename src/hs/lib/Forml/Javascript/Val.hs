------------------------------------------------------------------------------

------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Forml.Javascript.Val where

import Language.Javascript.JMacro

import Forml.AST
import Forml.Javascript.Ref
import Forml.Javascript.Lit()

------------------------------------------------------------------------------

instance ToJExpr Sym where
    toJExpr (Sym x) = ref x

instance ToJExpr Val where
    toJExpr (SymVal s) = toJExpr s
    toJExpr (LitVal l) = toJExpr l
    toJExpr (ConVal (TypeSym (TypeSymP s))) = ref s
    toJExpr (ConVal t) = error $ "FATAL: " ++ show t

------------------------------------------------------------------------------