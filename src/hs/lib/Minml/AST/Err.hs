--------------------------------------------------------------------------------

-- | Err type

--------------------------------------------------------------------------------

module Minml.AST.Err where

import Minml.Utils

--------------------------------------------------------------------------------

newtype Err = Err String deriving (Eq, Show, Ord)

instance Fmt Err where

    fmt (Err x) = "ERROR " ++ x

--------------------------------------------------------------------------------