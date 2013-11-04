------------------------------------------------------------------------------

-- | Notation parsing

------------------------------------------------------------------------------

module Forml.Parse.Macro where

import Control.Applicative
import Control.Arrow
import qualified Data.Set as S
import qualified Data.Map as P
import Data.Maybe

import Forml.AST
import Forml.AST.Replace

------------------------------------------------------------------------------

inferScope :: Macro Expr -> Macro Expr
inferScope (MacroTerm (Let x) (MacroList xs)) =
    MacroTerm (Let x) (MacroList (applyScope 0 . inferScope <$> xs))

inferScope (MacroTerm x (MacroList xs)) =
    MacroTerm x (MacroList (inferScope <$> xs))

inferScope x = x

applyScope :: Int -> Macro Expr -> Macro Expr
applyScope n y =
    applyScope' n y
    where
        applyScope' 0 (MacroTerm Sep _) = skipTokens y
        applyScope' m (MacroTerm Sep (MacroList [x])) = applyScope' (m - 1) x
        applyScope' m (MacroTerm Scope (MacroList [x])) = applyScope' (m + 1) x
        applyScope' m (MacroTerm _ (MacroList [x])) = applyScope' m x
        applyScope' _ _ = y

        skipTokens (MacroTerm (Token c) (MacroList [x])) =
            MacroTerm (Token c) (MacroList [skipTokens x])

        skipTokens x =
            MacroTerm Scope (MacroList [x])


toMac :: MacroCell -> Macro Expr -> Macro Expr
toMac cell = MacroTerm cell . MacroList . (:[])

inferCell :: String -> Macro Expr -> Macro Expr
inferCell sym = 
    
    uncurry ($) . (inferCell' sym &&& replace sym (ValPatt (SymVal (Sym (esc sym)))) . replace sym (esc sym))
 
esc = ('*':)

fromMac :: Macro Expr -> Expr
fromMac (MacroTerm _ (MacroList [x])) = fromMac x
fromMac (MacroLeaf x) = x
fromMac _ = undefined

inferCell' :: String -> Macro Expr -> Macro Expr -> Macro Expr
inferCell' sym = toMac . fromMaybe (Arg (esc sym)) . inferCellRec sym . fromMac

equate sym as | types sym as < 2 =
    head (fmap (inferCellRec sym) as)

equate sym as =
    error $ show as ++ " possible derivations"

types :: String -> [Expr] -> Int
types sym = S.size . S.fromList . catMaybes . fmap (inferCellRec sym)

equate2 sym ps as | S.size 
    (S.fromList (catMaybes (patt sym `fmap` ps)) 
        `S.union` S.fromList (catMaybes (inferCellRec sym `fmap` as))) < 2 =

    head (patt sym `fmap` ps)

equate2 sym _ as = error (show as++" possible derivations")

patt sym (ValPatt (SymVal (Sym f))) | f == sym = Just (Pat (esc sym))
patt sym (ConPatt _ xs) = equate2 sym xs []

patt _ _ = Nothing

inferCellRec :: String -> Expr -> Maybe MacroCell
inferCellRec sym (LetExpr (Sym f) a (Just b)) 
    | f == sym && maybe True (== Arg (esc sym)) (equate sym [a, b]) =
        Just (Let (esc sym))

    | otherwise = equate sym [a, b]

inferCellRec sym (AppExpr a b) = equate sym [a, b]

inferCellRec sym (VarExpr (SymVal (Sym f))) | f == sym = Just (Arg (esc sym))
inferCellRec sym (VarExpr _) = Nothing

inferCellRec sym (MatExpr e xs) = equate2 sym (map fst xs) (e : map snd xs)

inferCellRec sym (TypExpr _ _ (Just e)) = inferCellRec sym e

inferCellRec sym (AbsExpr (Sym f) ex)
    | f == sym && (
        isNothing (inferCellRec sym ex) ||
        inferCellRec sym ex == Just (Arg (esc sym))) = 

    Just (Let (esc sym))

inferCellRec sym (AbsExpr (Sym f) _) | f == sym = error "Unimplemented fox"

inferCellRec sym (AbsExpr _ z) = inferCellRec sym z

inferCellRec sym (JSExpr _) = Nothing

inferCellRec sym (RecExpr (Record xs)) =
    case catMaybes (fmap (inferCellRec sym) (P.elems xs)) of
        [x] -> Just x
        []  -> Nothing
        _   -> error "Too many futzpahs"

------------------------------------------------------------------------------
