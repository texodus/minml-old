------------------------------------------------------------------------------

-- | Notation parsing

------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}

module Minml.Macro.Infer(
    toMac,
    inferCell
) where

import Control.Arrow
import Data.Maybe

import Minml.AST
import Minml.Macro.Cell

------------------------------------------------------------------------------

toMac :: Cell -> Macro Expr -> Macro Expr
toMac cell = Term cell . MacTree . (:[])

fromMac :: Macro Expr -> Expr
fromMac (Term _ (MacTree [x])) = fromMac x
fromMac (Leaf x) = x
fromMac _ = undefined

inferCell :: String -> Macro Expr -> Macro Expr
inferCell sym = 
    uncurry toMac . (cell &&& id)
    where 
        cell = fromMaybe (Arg sym) . toCell sym . fromMac

------------------------------------------------------------------------------
