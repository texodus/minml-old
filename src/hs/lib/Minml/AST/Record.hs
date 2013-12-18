------------------------------------------------------------------------------

-- | Record Expressions.

------------------------------------------------------------------------------

{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverlappingInstances #-}

module Minml.AST.Record (
    Record( .. )
) where

import qualified Data.Map as M
import qualified Data.List as L

import Minml.Utils

------------------------------------------------------------------------------

-- | A record is simply a map of key / a pairs.  Used for both
--   record tpes and record expressions

newtype Record a =
    Record (M.Map String a) 
    deriving (Eq, Functor, Show, Ord, Read)

instance (Fmt a) => Fmt (Record a) where
    fmt (Record m) = "{" ++ showRec m ++ "}"

showRec :: (Fmt a) => M.Map String a -> String 
showRec = L.intercalate ", " . map showPair . M.toList

showPair :: Fmt a => (String, a) -> String
showPair (a, b) = a ++ ": " ++ fmt b

------------------------------------------------------------------------------





