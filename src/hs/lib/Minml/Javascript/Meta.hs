------------------------------------------------------------------------------

------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Minml.Javascript.Meta where

import Control.Lens
import Language.Javascript.JMacro

import Minml.AST

------------------------------------------------------------------------------

instance ToJExpr a => ToJExpr (Meta a) where
    toJExpr x = toJExpr (x^.node)

instance ToStat a => ToStat (Meta a) where
    toStat x = toStat (x^.node)

------------------------------------------------------------------------------