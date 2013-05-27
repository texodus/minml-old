------------------------------------------------------------------------------

-- A neat product of the Parsec's combinator API is that the elemental
-- parsers are themselves full parsers, so they're easy to test.

-- Putting it all together in one parser ...

------------------------------------------------------------------------------

module Forml.Parse.Type (
    typSymP,
    typAbsP,
    typP
) where

import Control.Applicative
import Text.Parsec         hiding (many, (<|>))
import Text.Parsec.Expr

import Forml.AST
import Forml.Parse.Token

------------------------------------------------------------------------------

typSymP :: Parser (TypeSym ())
typSymP = (TypeSymP .) . (:) <$> upper <*> identifier

typAbsP :: Parser (TypeAbs ())
typAbsP = TypeAbsP <$> typP <?> "Type (TypeAbs Kind)"

typP :: Parser (Type ())
typP = buildExpressionParser opPs termP <?> "Type Symbol"

    where
        opPs =
            [ [Infix (spaces >> return TypeApp) AssocLeft]
            , [Infix (reservedOp "->" >> return (fnConst "->")) AssocRight] ]

        fnConst = (TypeApp .) . TypeApp . TypeSym . TypeSymP
        termP   = typVarP <|> parens typP <|> TypeSym <$> typSymP
        typVarP = TypeVar . TypeVarP <$> identifier <?> "Type Variable"

------------------------------------------------------------------------------
