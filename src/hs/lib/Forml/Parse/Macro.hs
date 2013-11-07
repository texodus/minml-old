------------------------------------------------------------------------------

-- | Notation parsing

------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}

module Forml.Parse.Macro(
    module Forml.Parse.Macro.Scope,
    toMac,
    inferCell
) where

import Control.Arrow
import Data.Maybe

import Forml.AST
import Forml.Parse.Replace
import Forml.Parse.Macro.Scope
import Forml.Parse.Macro.MacroCell

------------------------------------------------------------------------------

toMac :: MacroCell -> Macro Expr -> Macro Expr
toMac cell = MacroTerm cell . MacroList . (:[])

inferCell :: String -> Macro Expr -> Macro Expr
inferCell sym = 
    uncurry ($) . (inferCell' sym &&& inline)
    where
        inline = replace sym patt . replace sym (esc sym)
        patt = ValPatt (SymVal (Sym (esc sym)))

fromMac :: Macro Expr -> Expr
fromMac (MacroTerm _ (MacroList [x])) = fromMac x
fromMac (MacroLeaf x) = x
fromMac _ = undefined

inferCell' :: String -> Macro Expr -> Macro Expr -> Macro Expr
inferCell' sym = toMac . fromMaybe (Arg (esc sym)) . toMacroCell sym . fromMac


------------------------------------------------------------------------------
