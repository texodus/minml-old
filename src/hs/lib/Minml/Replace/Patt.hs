------------------------------------------------------------------------------

-- | Pattern Inlining.

------------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Minml.Replace.Patt  where

import GHC.Generics

import Minml.AST
import Minml.Replace.Base
import Minml.Replace.Generic

------------------------------------------------------------------------------

instance Replace Patt Patt where
	replace f patt (ValPatt (SymVal (Sym t))) | f == t = patt
	replace f x y = to . greplace (replace f x :: Patt -> Patt) . from $ y

------------------------------------------------------------------------------

