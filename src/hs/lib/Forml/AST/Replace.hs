------------------------------------------------------------------------------

-- Expression parser.

-- `LetExpr` and `TypExpr` are roughly identical; here they've been arranged
-- slightly differently to show the symmetry between function application
-- & combinators of the `Applicative` type class.

------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Forml.AST.Replace where

import Language.Javascript.JMacro

------------------------------------------------------------------------------

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