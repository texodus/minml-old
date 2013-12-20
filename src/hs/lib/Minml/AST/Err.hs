--------------------------------------------------------------------------------

-- | Err type

--------------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric #-}

module Minml.AST.Err where

import Data.Serialize
import GHC.Generics

import Minml.Utils

--------------------------------------------------------------------------------

newtype Err = Err String deriving (Eq, Show, Ord, Read, Generic)

instance Serialize Err

instance Fmt Err where
    fmt (Err x) = "ERROR " ++ x

--------------------------------------------------------------------------------