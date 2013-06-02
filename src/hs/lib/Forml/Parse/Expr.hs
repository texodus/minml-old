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
import Language.Javascript.JMacro

import Forml.AST
import Forml.Parse.Indent
import Forml.Parse.Patt
import Forml.Parse.Record
import Forml.Parse.Token
import Forml.Parse.Type
import Forml.Parse.Val

------------------------------------------------------------------------------

exprP :: Parser Expr
exprP =

    jsExprP
        <|> recExprP
        <|> absExprP absExpr
        <|> matExprP
        <|> try letExprP
        <|> try typExprP
        <|> appExprP

    where

        jsExprP =
            (unwrap 
                $ reservedOp "`"
                >> (anyChar `manyTill` reservedOp "`"))
                <?> "Javascript"
            where
                unwrap :: Parser String -> Parser Expr
                unwrap f = do
                    f' <- f

                    case parseJME f' of
                        Left x -> parserFail (show x) 
                        Right x -> return (JSExpr x)

        absExprP f =
            pure (AbsExpr (Sym "_match"))
                <*  f
                <*> (pure (MatExpr (VarExpr (SymVal (Sym "_match"))) . (:[])) <*> caseP)
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

        toOp  = reservedOp "->" <|> reservedOp "="
        caseP = (,) <$> pattP <* toOp <*> exprP

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
                termP = valExprP <|> parens exprP
                opConst = (AppExpr .) . AppExpr . VarExpr . SymVal . Sym

------------------------------------------------------------------------------
