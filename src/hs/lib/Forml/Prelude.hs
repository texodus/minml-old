{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Forml.Prelude where

import Data.Char (isSpace)
import Data.FileEmbed
import qualified Data.ByteString.UTF8 as B

import Forml.AST

prelude :: String
prelude = B.toString $(embedFile "src/forml/prelude.forml")

appendPrelude :: String -> Either Err String
appendPrelude src =
    return $ concat [prelude, "\n\n", unindent src]

unindent :: String -> String
unindent src = unlines $ fmap (drop (findLevel (lines src))) $ lines src

	where
		findLevel (x : xs) 
		    | trim x == "" = findLevel xs
		    | otherwise    = length x - length (dropWhile isSpace x)


trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace