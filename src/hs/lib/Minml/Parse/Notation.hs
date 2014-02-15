------------------------------------------------------------------------------

-- | Notation parsing

------------------------------------------------------------------------------

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Minml.Parse.Notation where

import Control.Applicative
import Control.Lens

import Minml.AST
import Minml.Macro.Infer
import Minml.Macro.Scope
import Minml.Replace
import Minml.Parse.Syntax
import Minml.Parse.Token

import qualified Minml.Parse.MacroToken as M

------------------------------------------------------------------------------

newtype Notation = Notation (Expr -> Macro Expr)

-- | Parses a notation string.  This returns a `Cell Expr` constructor,
--   once the body of the notation block has been parsed.

instance Syntax Notation where

    syntax = 
        inferScope `comp` term <|> capture <|> sep <|> lastTerm
        where
            term = do
                f <- M.identifier <|> M.operator <|> (M.reserved "λ" >> return "λ")
                toMac (Token f) `comp` syntax
            
            capture = do
                tok <- use uniqState
                uniqState %= (+1)
                sym <- M.parens M.identifier
                let uniq = "$macro_" ++ show tok ++ "_" ++ sym
                (inferCell uniq . replace sym uniq) `comp` syntax

            sep = semi >> toMac Sep `comp` syntax

            lastTerm = return $ Notation Leaf

            comp mc p = do
                Notation ms <- p
                return $ Notation (mc . ms)

------------------------------------------------------------------------------
