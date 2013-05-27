
module Forml.Utils where

class Fmt a where
    fmt :: a -> String

instance Fmt () where fmt _ = "()"
