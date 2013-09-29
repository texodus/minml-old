------------------------------------------------------------------------------

------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Forml.Javascript.Type where

import Data.Monoid
import Language.Javascript.JMacro

import Forml.AST
import Forml.Javascript.Val()

------------------------------------------------------------------------------

instance ToJExpr (TypeAbs ()) where

    toJExpr (TypeAbsP typ) = 

        ValExpr (JFunc (toIdent `fmap` ids) (body ids))

        where
            ids = [(0 :: Integer) .. countIds typ]
            
            body = foldr (mappend . assign) mempty
                where assign arg = [jmacro|
                    this[`(arg)`] = `(ValExpr . JVar . toIdent $ arg)`; 
                |]

            toIdent = StrI . ("$" ++) . show

            countIds (isFun -> Just (_, fs)) = 1 + countIds fs
            countIds (isFun -> Nothing) = -1
            countIds _ = error "FATAL: countIds"

------------------------------------------------------------------------------
