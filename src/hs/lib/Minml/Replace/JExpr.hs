------------------------------------------------------------------------------

-- | Replace typeclass

------------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances  #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Minml.Replace.JExpr where

import Language.Javascript.JMacro

import Minml.AST
import Minml.Replace.Base

------------------------------------------------------------------------------

--instance (JMacro a, ToJExpr a, ToJExpr Expr) => Replace Expr a where

--    replace s x = replace s (toJExpr x)

------------------------------------------------------------------------------
