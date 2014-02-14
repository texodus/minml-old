------------------------------------------------------------------------------

-- | Replace typeclass

------------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

module Minml.AST.Replace where

import Language.Javascript.JMacro

------------------------------------------------------------------------------

-- | Represents a data structure where, given a string symbol, all matches
--   some class specific notion in `b` can be replaced with `a`.

class Replace a b where
    replace :: String -> a -> b -> b

instance (Functor f, Replace a b) => Replace a (f b) where
    replace sym = fmap . replace sym

instance (JMacro a) => Replace JExpr a where

    replace s x =
        withHygiene (jfromGADT . f . jtoGADT)
        where
            f :: JMGadt a -> JMGadt a
            f (JMGExpr (ValExpr (JVar (StrI z)))) | z == s = JMGExpr x
            f z = composOp f z

------------------------------------------------------------------------------
