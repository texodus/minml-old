------------------------------------------------------------------------------

--

------------------------------------------------------------------------------

module Forml.Parse.Record (
    recordP
) where

import Control.Applicative
import Text.Parsec
import qualified Data.Map as M

import Forml.Parse.Indent
import Forml.Parse.Token
import Forml.AST.Record

------------------------------------------------------------------------------

recordP :: Parser a -> Parser (Record a)
recordP x =

    pure (Record . M.fromList)
        <* reservedOp "{"
        <*> withScope (pair x `sepEndBy` sep)
        <* reservedOp "}"
        <?> "Record"
 
pair :: Parser a -> Parser (String, a)
pair x = (,) <$> identifier <* reservedOp ":" <*> x

------------------------------------------------------------------------------
