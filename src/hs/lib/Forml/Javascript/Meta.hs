------------------------------------------------------------------------------

------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Forml.Javascript.Meta where

import Control.Lens
import Language.Javascript.JMacro

import Forml.AST

------------------------------------------------------------------------------

instance ToJExpr a => ToJExpr (Meta a) where
    toJExpr x = toJExpr (x^.node)

instance ToStat a => ToStat (Meta a) where
    toStat x = toStat (x^.node)

------------------------------------------------------------------------------