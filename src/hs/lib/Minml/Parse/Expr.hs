------------------------------------------------------------------------------

-- | Expression parser.
--
--   `LetExpr` and `TypExpr` are roughly identical; here they've been arranged
--   slightly differently to show the symmetry between function application
--   & combinators of the `Applicative` type class.

------------------------------------------------------------------------------

{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Minml.Parse.Expr where

import Control.Applicative
import Control.Lens
import Language.Javascript.JMacro
import Text.Parsec.Expr

import Text.Parsec hiding (many, optional, (<|>))

import Minml.AST
import Minml.Macro.Append
import Minml.Parse.Indent
import Minml.Parse.Infix()
import Minml.Parse.Notation
import Minml.Parse.Macro
import Minml.Parse.Patt()
import Minml.Parse.Record()
import Minml.Parse.Syntax
import Minml.Parse.Token
import Minml.Parse.Type()
import Minml.Parse.Val()

------------------------------------------------------------------------------

instance Syntax Expr where

    syntax = do
        table <- syntax
        buildExpressionParser table termP

termP :: Parser Expr
termP = letMacroP
    <|> macroP
    <|> typExprP
    <|> matExprP
    <|> jsExprP
    <|> try recExprP 
    <|> valExprP 
    <|> parens syntax
    <?> "Expression"

letMacroP :: Parser Expr
letMacroP = withScope $ do
    antiQuote
    Notation def <- syntax
    antiQuote
    reservedOp "="
    ms <- def <$> withCont syntax
    macs <- appendTree (MacTree [ms]) <$> use macros
    newMacs <- either parserFail return macs
    macros .= newMacs
    withSep syntax

macroP :: Parser Expr
macroP = ($ undefined) . fst <$> (use macros >>= macroPRec . filterP)

jsExprP :: Parser Expr
jsExprP =
    unwrap
        (reservedOp "``" >> (anyChar `manyTill` reservedOp "``"))
        <?> "Javascript"
    where
    unwrap :: Parser String -> Parser Expr
    unwrap f = do
        f' <- f
        case parseJME f' of
            Left x  -> parserFail (show x)
            Right x -> return (JSExpr x)

matExprP :: Parser Expr
matExprP =
    pure MatExpr
        <*  reserved "match"
        <*> syntax
        <*  reserved "with"
        <*> withCont (try caseP `sepEndBy` sep)
        <?> "Match Expression"
    where
        caseP = (,) <$> syntax <* toOp <*> withCont syntax

toOp :: Parser ()
toOp  = reservedOp "->" <|> reservedOp "="

valExprP :: Parser Expr
valExprP = VarExpr <$> syntax <?> "Value"

recExprP :: Parser Expr
recExprP =
    RecExpr <$> syntax <?> "Record Expression"

typExprP :: Parser Expr
typExprP =
    pure TypExpr
        <*  optional (reserved "data")
        <*> try (syntax <*  reserved ":")
        <*> syntax
        <*> capture (withSep syntax)
        <?> "Type Kind Expression"

capture :: Parser Expr -> Parser (Maybe Expr)
capture prsr = Just <$> prsr <|> do
    ms <- use macros
    tailMacros .= ms
    return Nothing

------------------------------------------------------------------------------
