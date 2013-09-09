------------------------------------------------------------------------------

-- Converts a notation string to a `Parser Expr Expr` 

------------------------------------------------------------------------------

{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Forml.Parse.Notation (
    notationP
) where


import Control.Applicative
import Control.Lens
import Text.Parsec hiding (many, (<|>))
import Forml.AST
import Forml.Parse.Token
import qualified Forml.Parse.MacroToken as M

------------------------------------------------------------------------------

keywordP :: a -> Parsec String () (Macro a)
keywordP expr = term <|> capture <|> lastTerm

    where
        term = do
            f <- M.identifier <|> M.semi
            restP <- keywordP expr
            return (Token f [restP])
        
        capture = do
            sym <- M.parens M.identifier
            Arg sym . (:[]) <$> keywordP expr

        lastTerm = return (Leaf expr)

-- TODO merge muthafucka!

notationP :: Parser a a -> Parser a a
notationP parser = do
    reservedOp "`" 
    rule <- anyChar `manyTill` reservedOp "`" 
    reservedOp "="
    ex <- parser
    case runParser (keywordP ex) () "KeywordParser" rule of
        Left _ -> error ("Failed to parse notation `" ++ rule ++ "`")
        Right newMacs -> do
            macros  %= mergeWith [newMacs]
            parser

mergeWith :: [Macro a] -> [Macro a] -> [Macro a]
mergeWith newMacs oldMacs =
    [ merged | new <- newMacs, merged <- mergeWith' new oldMacs ]
    where
        mergeWith' (Token x xs) (Token y ys : zs) | x == y = 
            Token x (mergeWith xs ys) : zs
        mergeWith' (Arg x xs) (Arg y ys : zs) | x == y =
            Arg x (mergeWith xs ys) : zs
        mergeWith' (Arg x _) (Arg y _ : _) =
            error ("Arg naming conflict: (" ++ x ++ ") and (" ++ y ++ ")")
        mergeWith' (Leaf _) (Leaf _ : _) = 
            error "Notation duplicate"
        mergeWith' x (y : zs) = y : mergeWith' x zs
        mergeWith' x [] = [x]




------------------------------------------------------------------------------
