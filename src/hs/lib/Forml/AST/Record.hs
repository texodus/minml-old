------------------------------------------------------------------------------

-- Expressions.

-- Here, `AbsExpr` is an Abstraction, and `AppExpr` is an Application.

------------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverlappingInstances #-}

module Forml.AST.Record (
    Record( .. ),
) where

import qualified Data.Map as M
import qualified Data.List as L

import Forml.Utils

------------------------------------------------------------------------------

newtype Record a = Record (M.Map String a) deriving (Eq, Show, Ord)

instance (Fmt a) => Fmt (Record a) where

    fmt (Record m) =
        "{" ++ showRec m ++ "}"

instance Functor Record where

    fmap f (Record m) = Record (f `fmap` m)

showRec :: (Fmt a) => M.Map String a -> String 
showRec = concat . L.intersperse ", " . map showPair . M.toList

showPair :: Fmt a => (String, a) -> String
showPair (a, b) = a ++ ": " ++ fmt b

------------------------------------------------------------------------------





