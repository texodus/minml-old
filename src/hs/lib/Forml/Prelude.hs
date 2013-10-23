{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Forml.Prelude where

import Data.FileEmbed
import qualified Data.ByteString.UTF8 as B

import Forml.AST

prelude :: String
prelude = B.toString $(embedFile "src/forml/prelude.forml")

appendPrelude :: String -> Either Err String
appendPrelude src =
    return src -- $ concat [prelude, "\n\n", src]