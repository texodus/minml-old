------------------------------------------------------------------------------

-- | MacroCell inference

------------------------------------------------------------------------------

{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}

module Forml.Parse.Macro.MacroCell where

import qualified Data.Set as S
import qualified Data.Map as P
import Data.Maybe

import Forml.AST

------------------------------------------------------------------------------

-- | Existential container for heterogenous lists of `MacroCell`s

data Box where
    Box :: ToMacroCell a => a -> Box

class ToMacroCell a where
    toMacroCell :: String -> a -> Maybe MacroCell

instance ToMacroCell Patt where

    toMacroCell sym (ValPatt (SymVal (Sym f))) | f == sym = Just (Pat sym)
    toMacroCell sym (ConPatt _ xs) = equ sym xs

    toMacroCell _ _ = Nothing

instance ToMacroCell Box where
    toMacroCell s (Box x) = toMacroCell s x

instance ToMacroCell Expr where

    toMacroCell sym (LetExpr (Sym f) a (Just b)) =
        isArg f sym [a, b]

    toMacroCell sym (AppExpr a b) =
        equ sym [a, b]

    toMacroCell sym (VarExpr (SymVal (Sym f)))
        | f == sym =
            Just (Arg sym)

    toMacroCell _ (VarExpr _) =
        Nothing

    toMacroCell sym (MatExpr e xs) =
        equ sym (map (Box . fst) xs ++ (map (Box . snd) xs))

    toMacroCell sym (TypExpr _ _ (Just e)) =
       toMacroCell sym e

    toMacroCell sym (AbsExpr (Sym f) ex) =
        isArg f sym [ex]

    toMacroCell sym (AbsExpr _ z) =
        toMacroCell sym z

    toMacroCell _ (JSExpr _) =
        Nothing

    toMacroCell sym (RecExpr (Record xs)) =
        equ sym (P.elems xs)

equ :: (ToMacroCell a) => String -> [a] -> Maybe MacroCell
equ n (catMaybes . fmap (toMacroCell n) -> xs) = 
    case S.size . S.fromList $ xs of
        0 -> Nothing
        1 -> Just $ head xs
        _ -> error ("A macro argument is used in multiple contexts: " ++ show xs)

isArg :: ToMacroCell a => String -> String -> [a] -> Maybe MacroCell
isArg f sym xs
    | f == sym && maybe True (== Arg sym) (equ sym xs) =
        Just (Let sym)
    | otherwise =
        equ sym xs

------------------------------------------------------------------------------
