------------------------------------------------------------------------------

-- | Parsers for `Val` and `Sym`.

--   "Alternative" combinator will evaluate to its second argument, iff
--   it's first argument fails and consumes no input - backtracking is explicit
--   via `try`.

------------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Minml.Parse.Val where

import Control.Applicative
import Control.Lens
import Text.Parsec hiding ((<|>))

import Minml.AST
import Minml.Parse.Lit()
import Minml.Parse.Syntax
import Minml.Parse.Token
import Minml.Parse.Type()

------------------------------------------------------------------------------

-- | Sym parser

instance Syntax Sym where

    syntax = try $ do
        ars <- use macros
        sym <- identifier <?> "symbol"
        if sym `elem` getReserved ars 
            then parserFail ("symbol (`" ++ sym ++ "` is a keyword)\n\nDEBUG: " ++ show (getReserved ars) )
            else return $ Sym sym

getReserved :: MacTree Expr -> [String]

getReserved (MacTree (Term cell xs : ys)) =
    getReserved' cell ++ getReserved xs ++ getReserved (MacTree ys)

getReserved (MacTree (_ : ys)) = getReserved (MacTree ys)

getReserved (MacTree []) = []

getReserved' :: Cell -> [String]
getReserved' (Token x) = [x]
getReserved' _ = []

-- | Val parser

instance Syntax Val where
    syntax = (SymVal <$> syntax)
        <|> (LitVal <$> syntax)
        <|> ConVal . TypeSym <$> syntax

------------------------------------------------------------------------------
