------------------------------------------------------------------------------

-- Converts a notation string to a `Parser Expr Expr` 

------------------------------------------------------------------------------

{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Forml.Parse.Macro (
    macroExpP
) where

import Control.Applicative
import Text.Parsec         hiding ((<|>))

import           Forml.AST
import           Forml.AST.Replace
import qualified Forml.Parse.MacroToken as M
import           Forml.Parse.Token
import           Forml.Parse.Val

------------------------------------------------------------------------------

macroExpP :: (Replace a) => Parser a a -> (String, a) -> Parser a a
macroExpP prsr (notLang, ex) = case runParser term () "test" notLang of
    Left e -> parserFail (show e)
    Right e -> e

    where
        macroLangP = term <|> capture <|> lastTerm

        term = do
            f <- M.identifier
            restP <- macroLangP
            return $ do
                reserved f <?> ("macro keyword '" ++ f ++ "'")
                restP

        valExprP = VarExpr <$> valP <?> "Value"

        capture = do
            f <- M.parens (M.identifier)
            restP <- macroLangP
            return $ try $ do
                expr' <- prsr
                replace f (expr') <$> restP

        lastTerm = eof >> return (return ex)

------------------------------------------------------------------------------
