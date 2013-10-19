------------------------------------------------------------------------------

-- | Notation parsing

------------------------------------------------------------------------------

module Forml.Parse.Notation (
    notationP
) where

import Control.Applicative
import qualified Data.Set as S
import Data.Maybe

import Forml.AST
import Forml.AST.Replace
import Forml.Parse.Token
import qualified Forml.Parse.MacroToken as M

------------------------------------------------------------------------------

-- | Parses a notation string.  This returns a `MacroCell Expr` constructor,
--   once the body of the notation block has been parsed.

notationP :: Parser Expr (Expr -> Macro Expr)
notationP = 
    term <|> capture <|> sep <|> lastTerm
    where
        term = do
            f <- M.identifier <|> M.operator <|> (M.reserved "λ" >> return "λ")
            (toMac (Token f) .) <$> notationP
        
        capture = do
            sym <- M.parens M.identifier
            (toArg sym .) <$> notationP

        sep = semi >> (toMac Sep .) <$> notationP

        lastTerm = return MacroLeaf

        toMac :: MacroCell -> Macro Expr -> Macro Expr
        toMac cell = MacroTerm cell . MacroList . (:[])

        toArg :: String -> Macro Expr -> Macro Expr
        toArg sym expr = 
            toMac (con' (fromCell expr)) . replace sym escSym $ expr
            where
                escSym = '*' : sym

                fromCell :: Macro Expr -> Expr
                fromCell (MacroTerm _ (MacroList [x])) = fromCell x
                fromCell (MacroLeaf x) = x

                con' :: Expr -> MacroCell
                con' as = fromMaybe (Arg escSym) (con as)

                equate as | types as < 2 =
                    head (fmap con as)

                equate as =
                    error (show as++" possible derivations for "++show sym)

                types :: [Expr] -> Int
                types = S.size . S.fromList . catMaybes . fmap con

                equate2 ps as | S.size 
                    (S.fromList (catMaybes (con `fmap` as)) 
                        `S.union` S.fromList (catMaybes (patt `fmap` ps))) < 2 =

                    con (head as)

                equate2 _ as = error (show as++" possible derivations for "++show sym)

                patt (ValPatt (SymVal (Sym f))) | f == sym = Just (Pat escSym)

                patt _ = Nothing

                con :: Expr -> Maybe MacroCell
                con (LetExpr (Sym f) a b) 
                    | f == sym && (
                        isNothing (equate [a, b]) ||
                        equate [a, b] == Just (Arg escSym)) = 

                        Just (Let escSym)

                   -- | f /= sym && (
                   --     isJust (con a) ||
                   --     equate [a, b] == Just (Arg escSym)) = 

                   --     Just (Let escSym)

                con (LetExpr _ a b) = equate [a, b]

                con (AppExpr a b) = equate [a, b]

                con (VarExpr (SymVal (Sym f))) | f == sym = Just (Arg escSym)
                con (VarExpr _) = Nothing

                con (MatExpr e xs) = equate2 (map fst xs) (e : map snd xs)

                con (TypExpr a b e) = con e

                con (AbsExpr (Sym f) ex)
                    | f == sym && (
                        isNothing (con ex) ||
                        con ex == Just (Arg escSym)) = 

                    Just (Let escSym)

                con (AbsExpr (Sym f) z) | f == sym = error "Fail"

                con (AbsExpr _ z) = con z

                --replace f x (RecExpr xs) = RecExpr (fmap (replace f x) xs)
                con (JSExpr _)  = Nothing
                
------------------------------------------------------------------------------
