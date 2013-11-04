------------------------------------------------------------------------------

-- | Expression parser.

--   `LetExpr` and `TypExpr` are roughly identical; here they've been arranged
--   slightly differently to show the symmetry between function application
--   & combinators of the `Applicative` type class.

------------------------------------------------------------------------------

{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}

module Forml.Parse.Expr (
    exprP
) where

import Control.Applicative
import Control.Arrow
import Control.Lens
import Data.Monoid
import Language.Javascript.JMacro
import Text.Parsec                hiding (many, (<|>), optional)
import Text.Parsec.Expr

import Forml.AST
import Forml.AST.Replace
import Forml.Parse.Indent
import Forml.Parse.Notation
import Forml.Parse.Patt
import Forml.Parse.Record
import Forml.Parse.Token
import Forml.Parse.Type
import Forml.Parse.Val

------------------------------------------------------------------------------

exprP :: Parser Expr Expr
exprP =

    letMacroP
        <|> macroP
        <|> jsExprP
        <|> recExprP
        <|> matExprP
        <|> typExprP
        <|> appExprP
        <?> "Expression"

letMacroP :: Parser Expr Expr
letMacroP = withScope $ do
    antiQuote 
    def <- notationP
    antiQuote 
    reservedOp "="
    ms <- def <$> withCont exprP
    macros %= mappend (MacroList [ms])
    withSep exprP

macroP :: Parser Expr Expr
macroP = ($ undefined) . fst <$> (use macros >>= merge rootP)
    where
        merge f (MacroList ms) = foldl (<|>) parserZero (fmap f ms)

        rootP (MacroTerm Sep xs) = withSep (merge rootP xs)
        rootP (MacroLeaf x) = return (const x, undefined)
        rootP x = bothP rootP x

        scopeP (MacroTerm Sep xs) = return (id, xs)
        scopeP (MacroLeaf _) = parserZero
        scopeP x = try $ bothP scopeP x

        bothP m (MacroTerm Scope xs) = do
            (ap, cont)   <-  withCont (merge scopeP xs)
            first (ap .) <$> withSep (merge m cont)

        bothP m (MacroTerm (Token "<") exs) = reservedOp "<" >> merge m exs
        bothP m (MacroTerm (Token "</") exs) = reservedOp "</" >> merge m exs
        bothP m (MacroTerm (Token x) exs) = reserved x >> merge m exs
        bothP m (MacroTerm (Let a) exs) = try $ wrap symP a m exs
        bothP m (MacroTerm (Pat a) exs) = wrap pattP a m exs
        bothP m (MacroTerm (Arg a) exs) = wrap exprP a m exs

        bothP _ _ = error "Unimplemented"

        wrap p a m exs = first . (.) . replace a <$> p <*> merge m exs

jsExprP :: Parser Expr Expr
jsExprP =
    unwrap
        (reservedOp "``" >> (anyChar `manyTill` reservedOp "``"))
        <?> "Javascript"
    where
        unwrap :: Parser Expr String -> Parser Expr Expr
        unwrap f = do
            f' <- f
            case parseJME f' of
                Left x  -> parserFail (show x)
                Right x -> return (JSExpr x)

matExprP :: Parser Expr Expr
matExprP =
    pure MatExpr
        <*  reserved "match"
        <*> exprP
        <*  reserved "with"
        <*> withCont (try caseP `sepEndBy` sep)
        <?> "Match Expression"
    where
        caseP = (,) <$> pattP <* toOp <*> withCont exprP 

toOp :: Parser Expr ()
toOp  = reservedOp "->" <|> reservedOp "="

recExprP :: Parser Expr Expr
recExprP =
    RecExpr <$> recordP exprP
        <?> "Record Expression"

typExprP :: Parser Expr Expr
typExprP =
    pure TypExpr
        <*  optional (reserved "data")
        <*> try (typSymP
        <*  reserved ":")
        <*> typAbsP
        <*> capture (withSep exprP)
        <?> "Type Kind Expression"

capture :: Parser Expr Expr -> Parser Expr (Maybe Expr)
capture prsr = Just <$> prsr <|> do
    ms <- use macros
    tailMacros .= ms
    return Nothing

appExprP :: Parser Expr Expr
appExprP = buildExpressionParser opPs termP <?> "Application"

    where
        opPs =
            [ Infix ap AssocLeft ]
                : toInfixTerm opConst AssocLeft (tail ops)

        toInfixTerm optr assoc =
            fmap . fmap $
                flip Infix assoc
                <<< uncurry (*>)
                <<< reservedOp
                &&& return . optr

        ap = indented >> return AppExpr
        valExprP = VarExpr <$> valP <?> "Value"
        termP = valExprP <|> matExprP <|> macroP <|> parens exprP
        opConst = (AppExpr .) . AppExpr . VarExpr . SymVal . Sym

------------------------------------------------------------------------------
