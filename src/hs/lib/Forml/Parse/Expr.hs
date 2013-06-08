------------------------------------------------------------------------------

-- Expression parser.

-- `LetExpr` and `TypExpr` are roughly identical; here they've been arranged
-- slightly differently to show the symmetry between function application
-- & combinators of the `Applicative` type class.

------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Forml.Parse.Expr (
    exprP
) where

import Control.Applicative
import Control.Arrow
import Text.Parsec         hiding (many, (<|>))
import Text.Parsec.Expr
import Language.Javascript.JMacro

import Forml.AST
import Forml.Parse.Indent
import Forml.Parse.Macro
import Forml.Parse.Patt
import Forml.Parse.Record
import Forml.Parse.Token
import Forml.Parse.Type
import Forml.Parse.Val

------------------------------------------------------------------------------

exprP :: Parser Expr Expr
exprP =

    notationP
        <|> macroP
        <|> jsExprP
        <|> recExprP
        <|> absExprP absExpr
        <|> matExprP
        <|> try letExprP
        <|> try typExprP
        <|> appExprP

macroP :: Parser Expr Expr
macroP = do
    (_, rs) <- getState
    foldl (<|>) (parserFail "marco") (macroExpP exprP <$> rs)

notationP :: Parser Expr Expr
notationP = do
    reservedOp "`" 
    rule <- anyChar `manyTill` reservedOp "`"
    reservedOp "="
    ex <- exprP
    (st, rs) <- getState
    setState (st, rs ++ [(rule, ex)])
    exprP

jsExprP :: Parser Expr Expr
jsExprP =
    (unwrap 
        $ reservedOp "``" >> (anyChar `manyTill` reservedOp "``") )
        <?> "Javascript"
    where
        unwrap :: Parser Expr String -> Parser Expr Expr
        unwrap f = do
            f' <- f
            case parseJME f' of
                Left x  -> parserFail (show x) 
                Right x -> return (JSExpr x)

absExprP :: Parser Expr () -> Parser Expr Expr
absExprP f =
    pure (AbsExpr (Sym "_match"))
        <*  f
        <*> (pure (MatExpr (VarExpr (SymVal (Sym "_match"))) . (:[])) <*> caseP)
        <?> "Abstraction"

absExpr :: Parser Expr ()
absExpr =
    reserved "fun" <|> reservedOp "\\" <|> reservedOp "λ"


matExprP =
    pure MatExpr 
        <*  reserved "match"
        <*> exprP
        <*  reserved "with"
        <*  indented
        <*> withScope (try ((,) <$> pattP <* toOp <*> exprP) `sepEndBy` sep)
        <?> "Match Expression"

caseP = (,) <$> pattP <* toOp <*> exprP
toOp  = reservedOp "->" <|> reservedOp "="
    
letExprP =
    pure LetExpr
        <*  (reserved "let" <|> return ())
        <*> symP
        <*> (valLetP <|> absExprP (return ()))
        <*> withSep exprP
        <?> "Let Expression"

valLetP =
    reservedOp "=" >> exprP

recExprP =
    RecExpr <$> recordP exprP 
        <?> "Record Expression"

typExprP =
    pure TypExpr
        <*  (reserved "data" <|> return ())
        <*> typSymP
        <*  reserved ":"
        <*> typAbsP
        <*> withSep exprP 
        <?> "Type Kind Expression"

appExprP = buildExpressionParser opPs termP <?> "Application"

    where
        opPs =
            [[ Infix ap AssocLeft ]]
                ++ toInfixTerm opConst AssocLeft (tail ops)

        toInfixTerm op assoc =
            fmap . fmap $
                flip Infix assoc
                <<< uncurry (*>)
                <<< reservedOp
                &&& return . op

        ap = spaces >> indented >> return AppExpr
        valExprP = VarExpr <$> valP <?> "Value"
        termP = valExprP <|> matExprP <|> macroP <|> parens exprP
        opConst = (AppExpr .) . AppExpr . VarExpr . SymVal . Sym

------------------------------------------------------------------------------