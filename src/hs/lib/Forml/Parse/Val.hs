------------------------------------------------------------------------------

-- Parsers for `Val` and `Sym`.

-- "Alternative" combinator will evaluate to its second argument, iff
-- it's first argument fails and consumes no input - backtracking is explicit
-- via `try`.

------------------------------------------------------------------------------

module Forml.Parse.Val (
    symP,
    valP
) where

import Control.Applicative
import Text.Parsec hiding ((<|>))

import Forml.AST
import Forml.Parse.Lit
import Forml.Parse.Token
import Forml.Parse.Type
import qualified Forml.Parse.MacroToken as M

------------------------------------------------------------------------------

symP :: Parser s Sym
symP = try $ do
    (_, rs) <- getState
    sym <- identifier
    case sym `elem` getReserved rs of
        True -> parserFail (show sym ++ "   " ++ show (getReserved rs) )
        False -> return $ Sym sym

getReserved :: [(String, t)] -> [String]
getReserved ((name, _) : xs) =
    getReserved xs ++ case runParser keywordP () "KeywordParser" name of
        Left _ -> error "PARADOX"
        Right x -> x
getReserved [] = []

keywordP :: Parsec String () [String]
keywordP = term <|> capture <|> lastTerm
    where
        term = do
            f <- M.identifier
            restP <- keywordP
            return (f : restP)
        
        capture = do
            M.parens (M.identifier)
            keywordP

        lastTerm = eof >> return []

valP :: Parser s Val
valP =
    (SymVal <$> symP)
        <|> (LitVal <$> litP)
        <|> ConVal . TypeSym <$> typSymP

------------------------------------------------------------------------------
