------------------------------------------------------------------------------

-- Converts a notation string to a `Parser Expr Expr` 

------------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Forml.Parse.Notation (
    notationP
) where


import Control.Applicative
import Text.Parsec hiding (many, (<|>))
import Forml.AST
import Forml.AST.Replace
import Forml.Parse.Token
import qualified Forml.Parse.MacroToken as M

------------------------------------------------------------------------------

keywordP :: (ToSym a, Replace a) => Parsec String () (a -> Macro a)
keywordP = 
    term <|> capture <|> lastTerm
    where
        term = do
            f <- M.identifier <|> M.semi
            ((Token f . (:[])) .) <$> keywordP
        
        capture = do
            sym <- M.parens M.identifier
            ((Arg ('*' : sym) . (:[]) . replace sym (Leaf (toSym ('*' : sym)))) .) <$> keywordP

        lastTerm = return Leaf

notationP :: (ToSym a, Replace a) => Parser a (a -> Macro a)
notationP = do
    reservedOp "`" 
    rule <- anyChar `manyTill` reservedOp "`" 
    reservedOp "="
    case runParser keywordP () "KeywordParser" rule of
        Left _ -> error ("Failed to parse notation `" ++ rule ++ "`")
        Right newMacs -> return newMacs


------------------------------------------------------------------------------
