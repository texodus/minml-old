------------------------------------------------------------------------------

-- `parseForml` has a type very similar to Parsec's `Parser a` type already.

-- `Parser a` is composable in a manner similar to what we saw in `compile`.
-- It is also an instance of the `Monad` type class, but we're not going
-- to use it; instead, we're going to use an arguably more interesting API,
-- the `Applicative` instance.

-- Applicative functor combinators are left associative - as is
-- function application!

------------------------------------------------------------------------------

module Forml.Parse (
    parseForml
) where

import Control.Applicative
import Control.Arrow
import Text.Parsec hiding ((<|>), many)
import Text.Parsec.Pos

import Forml.AST
import Forml.Parse.Token
import Forml.Parse.Expr
import Forml.Parse.Indent

------------------------------------------------------------------------------

parseForml :: String -> Either Err Expr
parseForml =
    left (Err . show) . runParser grammar (initialPos "", []) "Parsing Forml"

grammar :: Parser Expr Expr
grammar = spaces *> withScope exprP <* eof

------------------------------------------------------------------------------