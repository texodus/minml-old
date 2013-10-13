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

notationP :: Parser Expr (Expr -> MacroCell Expr)
notationP = 
    term <|> capture <|> sep <|> lastTerm
    where
        term = do
            f <- M.identifier <|> M.operator <|> (M.reserved "λ" >> return "λ")
            ((Token f . toMac) .) <$> notationP
        
        capture = do
            sym <- M.parens M.identifier
            (toArg sym .) <$> notationP

        sep = semi >> ((Sep . toMac) .) <$> notationP

        lastTerm = return Leaf

        toMac = Macro . (:[])

        toArg :: String -> MacroCell Expr -> MacroCell Expr
        toArg sym expr = 
            con' (fromCell expr) . toMac . replace sym escSym $ expr
            where
                escSym = "*" ++ sym

                fromCell (Arg _ (Macro [x])) = fromCell x
                fromCell (Pat _ (Macro [x])) = fromCell x
                fromCell (Let _ (Macro [x])) = fromCell x
                fromCell (Sep (Macro [x]))   = fromCell x
                fromCell (Token _ (Macro [x])) = fromCell x
                fromCell (Leaf x) = x
                fromCell _ = undefined

                con' as ms = fromMaybe (Arg escSym ms) (con as ms)

                equate as ms | S.size (S.fromList (catMaybes (flip con ms `fmap` as))) < 2 = con (head as) ms
                equate as _ = error (show as++" possible derivations for "++show sym)

                equate2 ps as ms | S.size 
                    (S.fromList (catMaybes (flip con ms `fmap` as)) 
                        `S.union` S.fromList (catMaybes (flip patt ms `fmap` ps))) < 2 =

                    con (head as) ms

                equate2 ps as _ = error (show as++" possible derivations for "++show sym)

                patt (ValPatt (SymVal (Sym f))) ms | f == sym = Just (Pat escSym ms)

                patt _ _ = Nothing

                con (LetExpr (Sym f) a b) ms 
                    | f == sym && (
                        isNothing (equate [a, b] ms) ||
                        equate [a, b] ms == Just (Arg escSym ms)) = 

                    Just (Let escSym ms)

                con (LetExpr f a b) ms = equate [a, b] ms

                con (AppExpr a b) ms = equate [a, b] ms

                con (VarExpr (SymVal (Sym f))) ms | f == sym = Just (Arg escSym ms)
                con (VarExpr _) ms = Nothing

                con (MatExpr e xs) ms = equate2 (map fst xs) (e : map snd xs) ms

                con (TypExpr a b e) ms = con e ms

                con (AbsExpr (Sym f) ex) ms
                   | f == sym && (
                        isNothing (con ex ms) ||
                        con ex ms == Just (Arg escSym ms)) = 

                    Just (Let escSym ms)

                con (AbsExpr (Sym f) z) ms | f == sym = error "Fail"

                con (AbsExpr _ z) ms = con z ms

                --replace f x (RecExpr xs) = RecExpr (fmap (replace f x) xs)
                con (JSExpr _) ms  = Nothing
------------------------------------------------------------------------------
