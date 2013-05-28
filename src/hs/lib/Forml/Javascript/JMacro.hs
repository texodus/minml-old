------------------------------------------------------------------------------
------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Forml.Javascript.JMacro (
    replace,
    minify
) where

import Control.Applicative
import Control.Monad.State.Strict
import qualified Data.Map as M
import Language.Javascript.JMacro hiding (withHygiene)

import Forml.Javascript.Type()
import Forml.Javascript.Val()

------------------------------------------------------------------------------

minify :: JExpr -> JExpr
minify orig @ (InfixExpr "&&" x y) = case (minify x, minify y) of
    (ValExpr (JVar (StrI "true")), z) -> z
    (z, ValExpr (JVar (StrI "true"))) -> z
    _ -> orig
minify x = x

replace :: JMacro a => String -> JExpr -> a -> a
replace s x = 
    withHygiene (jfromGADT . composOp f . jtoGADT)
    where
        f :: JMGadt a -> JMGadt a
        f (JMGExpr (ValExpr (JVar (StrI z)))) | z == s = JMGExpr x
        f z = composOp f z

withHygiene ::  JMacro a => (a -> a) -> a -> a
withHygiene f x = jfromGADT $ case jtoGADT x of
    JMGExpr z -> JMGExpr $ UnsatExpr $ inScope z
    JMGStat z -> JMGStat $ UnsatBlock $ inScope z
    JMGVal  z -> JMGVal $ UnsatVal $ inScope z
    JMGId _ -> jtoGADT $ f x
    where
        inScope z = IS $ do
            ([StrI a], b) <- splitAt 1 `fmap` get
            put b
            return $ withHygiene_ a f z

jsReplace_ :: JMacro a => [(Ident, Ident)] -> a -> a
jsReplace_ xs e = jfromGADT $ go (jtoGADT e)
    where
        go :: forall a. JMGadt a -> JMGadt a
        go v = case v of
            JMGId i -> maybe v JMGId (M.lookup i mp)
            _ -> composOp go v
        mp = M.fromList xs

jsUnsat_ :: JMacro a => [Ident] -> a -> IdentSupply a
jsUnsat_ xs e = IS $ do
    (idents,is') <- splitAt (length xs) <$> get
    put is'
    return $ jsReplace_ (zip xs idents) e

withHygiene_ :: JMacro a => String -> (a -> a) -> a -> a
withHygiene_ un f x = jfromGADT $ case jtoGADT x of
    JMGStat _ -> jtoGADT $ UnsatBlock (jsUnsat_ is' x'')
    JMGExpr _ -> jtoGADT $ UnsatExpr (jsUnsat_ is' x'')
    JMGVal  _ -> jtoGADT $ UnsatVal (jsUnsat_ is' x'')
    JMGId _ -> jtoGADT $ f x
    where
        (x', (StrI l : _)) = runState (runIdentSupply $ jsSaturate_ x) is 
        is' = take lastVal is
        x'' = f x'
        lastVal = read (reverse . takeWhile (/= '$') . reverse $ l) :: Int
        is = [ StrI ("inSat_" ++ un ++ "$" ++ show it)
             | it <- [(0::Integer) ..] ]

jsSaturate_ :: (JMacro a) => a -> IdentSupply a
jsSaturate_ = IS . fmap jfromGADT . go . jtoGADT
    where
        go :: forall a. JMGadt a -> State [Ident] (JMGadt a)
        go v = case v of
            JMGStat (UnsatBlock us) -> go =<< (JMGStat <$> runIdentSupply us)
            JMGExpr (UnsatExpr us) -> go =<< (JMGExpr <$> runIdentSupply us)
            JMGVal (UnsatVal us) -> go =<< (JMGVal <$> runIdentSupply us)
            _ -> composOpM go v

------------------------------------------------------------------------------
