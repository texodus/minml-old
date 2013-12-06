------------------------------------------------------------------------------

-- | Notation parsing

------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}

module Forml.Macro.Infer(
    toMac,
    inferCell
) where

import Control.Arrow
import Data.Maybe

import Forml.AST
import Forml.Macro.Cell

------------------------------------------------------------------------------

toMac :: Cell -> Macro Expr -> Macro Expr
toMac cell = Term cell . MacList . (:[])

fromMac :: Macro Expr -> Expr
fromMac (Term _ (MacList [x])) = fromMac x
fromMac (Leaf x) = x
fromMac _ = undefined

inferCell :: String -> Macro Expr -> Macro Expr
inferCell sym = 
    uncurry toMac . (cell &&& id)
    where 
        cell = fromMaybe (Arg sym) . toCell sym . fromMac

------------------------------------------------------------------------------
