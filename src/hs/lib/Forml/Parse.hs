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

import Forml.AST
import Forml.Parse.Token
import Forml.Parse.Expr
import Forml.Parse.Indent

------------------------------------------------------------------------------

parseForml :: String -> String -> MacroState -> Either Err (Expr, MacroState)
parseForml name src state =
    left printError $ runParser grammar state name src

printError :: ParseError -> Err
printError err =
    Err . show $ err -- setErrorPos (newPos "Test Case" (sourceLine (errorPos err) - 29) (sourceColumn (errorPos err) + 3)) err

grammar :: Parser (Expr, MacroState)
grammar = do
    whiteSpace
    res <- withScope $ (,) <$> exprP <*> getState
    eof
    return res

--------------------------------------------------------------------------------