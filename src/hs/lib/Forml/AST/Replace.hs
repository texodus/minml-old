------------------------------------------------------------------------------

-- | Replace typeclass

------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Forml.AST.Replace where

import Language.Javascript.JMacro

------------------------------------------------------------------------------

-- | Represents a data structure where, given a string symbol, all matches
--   some class specific notion in `b` can be replaced with `a`.

class Replace a b where

    repGen     :: (forall c d. (Replace c d) => String -> c -> d -> d) -> String -> a -> b -> b
    replace    :: String -> a -> b -> b
    replaceLet :: String -> a -> b -> b

    replace    = repGen replace
    replaceLet = repGen replaceLet
    repGen     = undefined

instance (Functor f, Replace a b) => Replace a (f b) where
    repGen g sym   = fmap . repGen g sym
    replaceLet sym = fmap . replaceLet sym

instance (JMacro a) => Replace JExpr a where
 
    replace s x = 
        withHygiene (jfromGADT . composOp f . jtoGADT)
        where
            f :: JMGadt a -> JMGadt a
            f (JMGExpr (ValExpr (JVar (StrI z)))) | z == s = JMGExpr x
            f z = composOp f z

------------------------------------------------------------------------------