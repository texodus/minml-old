------------------------------------------------------------------------------

-- | Record parsing

------------------------------------------------------------------------------

{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Forml.Parse.Record where

import Control.Applicative
import Text.Parsec
import qualified Data.Map as M

import Forml.Parse.Indent
import Forml.Parse.Syntax
import Forml.Parse.Token
import Forml.AST.Record

------------------------------------------------------------------------------

-- | A parser for a Record literal

instance Syntax a => Syntax (Record a) where

    syntax = pure (Record . M.fromList)
        <*  reservedOp "{"
        <*> withScope (pair `sepEndBy` sep)
        <*  reservedOp "}"
        <?> "Record"
 
pair :: Syntax a => Parser (String, a)
pair = (,) <$> identifier <* reservedOp ":" <*> syntax

------------------------------------------------------------------------------
