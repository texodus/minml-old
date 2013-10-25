------------------------------------------------------------------------------

-- | Notation parsing

------------------------------------------------------------------------------

module Forml.Parse.Notation (
    notationP
) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Monoid
import qualified Data.Set as S
import qualified Data.Map as P
import Data.Maybe

import Forml.AST
import Forml.AST.Replace
import Forml.Parse.Token
import qualified Forml.Parse.MacroToken as M

------------------------------------------------------------------------------

-- | Parses a notation string.  This returns a `MacroCell Expr` inferCellRecstructor,
--   once the body of the notation block has been parsed.

notationP :: Parser Expr (Expr -> Macro Expr)
notationP = 
    (inferScope .) <$> (term <|> capture <|> sep <|> lastTerm)
    where
        term = do
            f <- M.identifier <|> M.operator <|> (M.reserved "λ" >> return "λ")
            (toMac (Token f) .) <$> notationP
        
        capture = do
            sym <- M.parens M.identifier
            (inferCell sym .) <$> notationP

        sep = semi >> (toMac Sep .) <$> notationP

        lastTerm = return MacroLeaf


inferScope :: Macro Expr -> Macro Expr
inferScope (MacroTerm (Let x) (MacroList xs)) =
    MacroTerm (Let x) (MacroList (applyScope . inferScope <$> xs))

inferScope (MacroTerm x (MacroList xs)) =
    MacroTerm x (MacroList (inferScope <$> xs))

inferScope x = x

applyScope :: Macro Expr -> Macro Expr
applyScope ex = case findSep ex of
    [(ys, zs)] -> MacroTerm (Scope ys) zs
    [] -> ex

findSep :: Macro Expr -> [([MacroCell], MacroList Expr)]
findSep (MacroLeaf _) = []  -- TODO emit warning for inconcistent scope
findSep (MacroTerm Sep xs) = [([Sep], xs)]
findSep (MacroTerm cell (MacroList xs)) =
    first (cell:) <$> concatMap findSep xs
        -- [] -> []
        --Just (x, y) -> Just (cell : x, y)

toMac :: MacroCell -> Macro Expr -> Macro Expr
toMac cell = MacroTerm cell . MacroList . (:[])

inferCell :: String -> Macro Expr -> Macro Expr
inferCell sym = 
    
    uncurry ($) . (inferCell' &&& replace sym escSym)
    
    where
        escSym = '*' : sym

        fromMac :: Macro Expr -> Expr
        fromMac (MacroTerm _ (MacroList [x])) = fromMac x
        fromMac (MacroLeaf x) = x
        fromMac _ = undefined

        inferCell' :: Macro Expr -> Macro Expr -> Macro Expr
        inferCell' = toMac . fromMaybe (Arg escSym) . inferCellRec . fromMac

        equate as | types as < 2 =
            head (fmap inferCellRec as)

        equate as =
            error (show as++" possible derivations for "++show sym)

        types :: [Expr] -> Int
        types = S.size . S.fromList . catMaybes . fmap inferCellRec

        equate2 ps as | S.size 
            (S.fromList (catMaybes (inferCellRec `fmap` as)) 
                `S.union` S.fromList (catMaybes (patt `fmap` ps))) < 2 =

            inferCellRec (head as)

        equate2 _ as = error (show as++" possible derivations for "++show sym)

        patt (ValPatt (SymVal (Sym f))) | f == sym = Just (Pat escSym)

        patt _ = Nothing

        inferCellRec :: Expr -> Maybe MacroCell
        inferCellRec (LetExpr (Sym f) a b) 
            | f == sym && maybe True (== Arg escSym) (equate [a, b]) =
                Just (Let escSym)

            | otherwise = equate [a, b]

        inferCellRec (AppExpr a b) = equate [a, b]

        inferCellRec (VarExpr (SymVal (Sym f))) | f == sym = Just (Arg escSym)
        inferCellRec (VarExpr _) = Nothing

        inferCellRec (MatExpr e xs) = equate2 (map fst xs) (e : map snd xs)

        inferCellRec (TypExpr _ _ e) = inferCellRec e

        inferCellRec (AbsExpr (Sym f) ex)
            | f == sym && (
                isNothing (inferCellRec ex) ||
                inferCellRec ex == Just (Arg escSym)) = 

            Just (Let escSym)

        inferCellRec (AbsExpr (Sym f) _) | f == sym = error "Unimplemented fox"

        inferCellRec (AbsExpr _ z) = inferCellRec z

        inferCellRec (JSExpr _) = Nothing

        inferCellRec (RecExpr (Record xs)) =
            case catMaybes (fmap inferCellRec (P.elems xs)) of
                [x] -> Just x
                []  -> Nothing
                _   -> error "Too many futzpahs"

------------------------------------------------------------------------------
