------------------------------------------------------------------------------

-- | Record Javascript.  Records are simply represented as functions.

------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Minml.Javascript.Record where

import Language.Javascript.JMacro

import Minml.AST

------------------------------------------------------------------------------

instance (ToJExpr a) => ToJExpr (Record a) where

    toJExpr (Record m) =
        ValExpr (JHash (toJExpr `fmap` m))

------------------------------------------------------------------------------
