------------------------------------------------------------------------------

-- | Cell inference

------------------------------------------------------------------------------

{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}

module Forml.Macro.Cell where

import qualified Data.Set as S
import qualified Data.Map as P
import Data.Maybe

import Forml.AST

------------------------------------------------------------------------------

-- | Existential container for heterogenous lists of `Cell`s

data Box where
    Box :: ToCell a => a -> Box

class ToCell a where
    toCell :: String -> a -> Maybe Cell

instance ToCell Patt where

    toCell sym (ValPatt (SymVal (Sym f))) | f == sym = Just (Pat sym)
    toCell sym (ConPatt _ xs) = equ sym xs

    toCell _ _ = Nothing

instance ToCell Box where
    toCell s (Box x) = toCell s x

instance ToCell Expr where

    toCell sym (LetExpr (Sym f) a (Just b)) =
        isArg f sym [a, b]

    toCell sym (AppExpr a b) =
        equ sym [a, b]

    toCell sym (VarExpr (SymVal (Sym f)))
        | f == sym =
            Just (Arg sym)

    toCell _ (VarExpr _) =
        Nothing

    toCell sym (MatExpr _ xs) =
        equ sym (map (Box . fst) xs ++ map (Box . snd) xs)

    toCell sym (TypExpr _ _ (Just e)) =
       toCell sym e

    toCell sym (AbsExpr (Sym f) ex) =
        isArg f sym [ex]

    toCell _ (JSExpr _) =
        Nothing

    toCell sym (RecExpr (Record xs)) =
        equ sym (P.elems xs)

    toCell _ _ = error "PARADOX: I should not be"

equ :: (ToCell a) => String -> [a] -> Maybe Cell
equ n (catMaybes . fmap (toCell n) -> xs) = 
    case S.size . S.fromList $ xs of
    0 -> Nothing
    1 -> Just $ head xs
    _ -> error ("A macro argument is used in multiple contexts: " ++ show xs)

isArg :: ToCell a => String -> String -> [a] -> Maybe Cell
isArg f sym xs
    | f == sym && maybe True (== Arg sym) (equ sym xs) =
        Just (Let sym)
    | otherwise =
        equ sym xs

------------------------------------------------------------------------------
