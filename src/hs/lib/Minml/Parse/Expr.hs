------------------------------------------------------------------------------

-- | Expression parser.

--   `LetExpr` and `TypExpr` are roughly identical; here they've been arranged
--   slightly differently to show the symmetry between function application
--   & combinators of the `Applicative` type class.

------------------------------------------------------------------------------

{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Minml.Parse.Expr where

import Control.Applicative
import Control.Arrow
import Control.Lens
import Data.Monoid
import Language.Javascript.JMacro
import Text.Parsec.Expr

import Text.Parsec hiding (many, optional, (<|>))

import Minml.AST
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

    syntax = letMacroP
        <|> macroP
        <|> jsExprP
        <|> recExprP
        <|> matExprP
        <|> typExprP
        <|> appExprP
        <?> "Expression"

letMacroP :: Parser Expr
letMacroP = withScope $ do
    antiQuote
    Notation def <- syntax
    antiQuote
    reservedOp "="
    ms <- def <$> withCont syntax
    macros %= mappend (MacList [ms])
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

recExprP :: Parser Expr
recExprP =
    RecExpr <$> syntax
        <?> "Record Expression"

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

appExprP :: Parser Expr
appExprP = do
    table <- syntax
    buildExpressionParser (opPs table) termP <?> "Application"

    where
        opPs table =
            [ Infix appl AssocLeft ]
                : toInfixTerm opConst AssocLeft (tail ops)
                ++ [table]

        toInfixTerm optr assoc =
            fmap . fmap $
                flip Infix assoc
                <<< uncurry (*>)
                <<< reservedOp
                &&& return . optr

        appl = indented >> return AppExpr
        valExprP = VarExpr <$> syntax <?> "Value"
        termP = valExprP <|> matExprP <|> macroP <|> parens syntax
        opConst = (AppExpr .) . AppExpr . VarExpr . SymVal . Sym

------------------------------------------------------------------------------
