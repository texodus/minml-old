------------------------------------------------------------------------------

-- | Generic Replace - an exercise in over engineering in Haskell

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

module Minml.Replace.Generic where

import Control.Arrow
import GHC.Generics

------------------------------------------------------------------------------

class GReplace a f where
    greplace :: (a -> a) -> f b -> f b

instance GReplace a U1 where
    greplace _ ex = ex

instance (GReplace a f) => GReplace a (M1 i c f) where
    greplace f (M1 ex) = M1 $ greplace f ex

instance (GReplace a f, GReplace a g) => GReplace a (f :+: g) where
    greplace f (L1 x) = L1 $ greplace f x
    greplace f (R1 x) = R1 $ greplace f x

instance (GReplace a f, GReplace a g) => GReplace a (f :*: g) where
    greplace f (a :*: b) = greplace f a :*: greplace f b

instance GReplace a (K1 i a) where
    greplace f (K1 ex) = K1 (f ex)

instance GReplace a (K1 i c) where
    greplace _ (K1 x) = K1 x

instance (Functor c) => GReplace a (K1 i (c a)) where
    greplace f (K1 x) = K1 (f `fmap` x)

instance (Functor c) => GReplace a (K1 i (c (b, a))) where
    greplace f (K1 x) = K1 (second f `fmap` x)

instance (Functor c) => GReplace a (K1 i (c (a, b))) where
    greplace f (K1 x) = K1 (first f `fmap` x)

------------------------------------------------------------------------------
