------------------------------------------------------------------------------

-- Marshalling the OHML AST into `JExpr`s - the strategy is to rely
-- on instances of `ToJExpr` for our AST.

------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Forml.Javascript.Lit where

import Language.Javascript.JMacro

import Forml.AST

----------------------------------------------------------------------------

instance ToJExpr Lit where
    toJExpr (StrLit s)  = toJExpr s
    toJExpr (NumLit n)  = toJExpr n

------------------------------------------------------------------------------