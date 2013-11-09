------------------------------------------------------------------------------

-- | Notation parsing

------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}

module Forml.Parse.Macro.Infer(
    toMac,
    inferCell
) where

import Control.Arrow
import Data.Maybe

import Forml.AST
import Forml.Parse.Replace
import Forml.Parse.Macro.MacroCell

------------------------------------------------------------------------------

toMac :: MacroCell -> Macro Expr -> Macro Expr
toMac cell = MacroTerm cell . MacroList . (:[])

inferCell :: String -> String -> Macro Expr -> Macro Expr
inferCell uniq sym = 
    uncurry ($) . (inferCell' (uniq ++ sym) &&& id) . replace sym (uniq ++ sym)

fromMac :: Macro Expr -> Expr
fromMac (MacroTerm _ (MacroList [x])) = fromMac x
fromMac (MacroLeaf x) = x
fromMac _ = undefined

inferCell' :: String -> Macro Expr -> Macro Expr -> Macro Expr
inferCell' sym = toMac . fromMaybe (Arg sym) . toMacroCell sym . fromMac


------------------------------------------------------------------------------
