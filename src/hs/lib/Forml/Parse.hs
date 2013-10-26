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
import Control.Lens
import Data.Monoid
import Text.Parsec hiding ((<|>), many)
import Text.Parsec.Pos

import Forml.AST
import Forml.Parse.Token
import Forml.Parse.Expr
import Forml.Parse.Indent
import Forml.Parse.Notation

------------------------------------------------------------------------------

parseForml :: String -> Either Err Expr
parseForml =
    left (Err . show) . runParser grammar (MacroState (initialPos "") bootstrap) "Parsing Forml"

bootstrap :: MacroList Expr
bootstrap = foldl1 mappend 

    -- Let
    [ parseNote "fun (x) -> (y)"       (AbsExpr (Sym "x") (VarExpr (SymVal (Sym "y"))))
    , parseNote "let (a) = (b); (c)"   (LetExpr (Sym "a") (VarExpr (SymVal (Sym "b"))) (VarExpr (SymVal (Sym "c"))))
    ]

    where
        parseNote a = case runParser notationP (MacroState (initialPos "") mempty) "" a of
            Left x  -> error . show $ x
            Right x -> MacroList . (:[]) . x

grammar :: Parser Expr Expr
grammar = whiteSpace *> withScope exprP <* eof

  --do
  --  whiteSpace 
  --  withScope $ do
  --      exprP
  --      s <- use macros
  --      error . show $ s


  --  undefined -- eof

--------------------------------------------------------------------------------