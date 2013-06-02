------------------------------------------------------------------------------

-- Expression parser.

-- `LetExpr` and `TypExpr` are roughly identical; here they've been arranged
-- slightly differently to show the symmetry between function application
-- & combinators of the `Applicative` type class.

------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}

module Forml.Parse.Expr (
    exprP
) where

import Control.Applicative
import Control.Arrow
import Text.Parsec         hiding (many, (<|>))
import Text.Parsec.Expr

import Forml.AST
import Forml.Parse.Indent
import Forml.Parse.Patt
import Forml.Parse.Token
import Forml.Parse.Type
import Forml.Parse.Val

------------------------------------------------------------------------------

exprP :: Parser Expr
exprP =

    try letExprP
        <|> typExprP
        <|> absExprP absExpr
        <|> matExprP
        <|> appExprP

    where

        absExprP f =
            pure AbsExpr
                <*  f
                <*> symP
                <*  (reservedOp "=" <|> reservedOp "->")
                <*> exprP
                <?> "Abstraction"

        absExpr =
            reserved "fun" <|> reservedOp "\\" <|> reservedOp "λ"

        matExprP =
            pure MatExpr 
                <*  reserved "match"
                <*> exprP
                <*  reserved "with"
                <*  indented
                <*> withScope (try caseP `sepEndBy` sep)
                <?> "Match Expression"

            where
                toOp = reservedOp "->" <|> reservedOp "="
                caseP =
                    (,) <$> pattP <* toOp <*> exprP

        letExprP =
            pure LetExpr
                <*  (reserved "let" <|> return ())
                <*> symP
                <*> (valLetP <|> try (absExprP (return ())))
                <*> withSep exprP
                <?> "Let Expression"

        valLetP =
            reservedOp "=" >> exprP

        typExprP =
            pure TypExpr
                <*  reserved "data"
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
                termP = valExprP <|> parens exprP
                opConst = (AppExpr .) . AppExpr . VarExpr . SymVal . Sym

------------------------------------------------------------------------------
