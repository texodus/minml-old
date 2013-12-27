------------------------------------------------------------------------------

-- | A neat product of the Parsec's combinator API is that the elemental
--   parsers are themselves full parsers, so they're easy to test.

--   Putting it all together in one parser ...

------------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances #-}

module Minml.Parse.Type where

import Control.Applicative
import Text.Parsec         hiding (many, (<|>))
import Text.Parsec.Expr

import Minml.AST
import Minml.Parse.Syntax
import Minml.Parse.Token
import Minml.Parse.Indent

------------------------------------------------------------------------------

instance Syntax (TypeSym ()) where
    syntax = (TypeSymP .) . (:) <$> upper <*> identifier

instance Syntax (TypeAbs ()) where
    syntax = TypeAbsP <$> syntax <?> "Type (TypeAbs Kind)"

instance Syntax (Type ()) where
    syntax =
        buildExpressionParser opPs termP <?> "Type Symbol"

        where
            opPs =
                [ [Infix (indented >> return TypeApp) AssocLeft]
                , [Infix (indented >> reservedOp "->" >> return (fnConst "->")) AssocRight] ]

            fnConst = (TypeApp .) . TypeApp . TypeSym . TypeSymP
            termP   = typVarP <|> parens syntax <|> TypeSym <$> syntax
            typVarP = TypeVar . TypeVarP <$> identifier <?> "Type Variable"

------------------------------------------------------------------------------
