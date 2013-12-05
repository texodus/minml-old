------------------------------------------------------------------------------

-- | A library for writing whitespace-aware parsers with parsec.

------------------------------------------------------------------------------

{-# LANGUAGE RankNTypes #-}

module Forml.Parse.Indent (
    indented,
    withSep,
    withCont,
    withScope,
    sep,
    inScope
) where

import Control.Lens
import Control.Monad.State
import Text.Parsec

import Forml.Parse.Token

------------------------------------------------------------------------------

-- | Introduces a new scope around a parser.  Future invocations of
--   `inScope`, for example, will reference the `SourcePos` stored from the
--   most recent invocation of `withScope`, or `initialPos` if it has
--   not been invoked.

withScope :: Parser a -> Parser a
withScope parser = do
    st  <- get
    pos <- getPosition
    sourcePos .= pos
    res <- parser
    tm  <- use tailMacros
    setState st
    tailMacros .= tm
    return res

-- | Fails if the cursor is not within the current scope

inScope :: (Int -> Int -> Bool) -> Parser ()
inScope comp = condSep sourceLine (>) "in scope" >> condSep sourceColumn comp "in scope"

-- | Fails if the cursor is not indented, or on the same line

indented :: Parser ()
indented = condSep sourceColumn (>) "indented"

-- | Tries a parser as a continuation of an existing block

withSep :: Parser a -> Parser a
withSep parser =
    (semi >> withScope parser) <|> (inScope (==) >> withScope parser)

-- | Same as withSep, but without a semi

withCont :: Parser a -> Parser a
withCont parser =
    (condSep sourceLine (==) "inline" >> parser) <|> (inScope (>=) >> withScope parser)

-- | Line separator parser

sep :: Parser ()
sep = void semi <|> inScope (==)

-- | Optionally parse a separator in-scope

condSep :: (SourcePos -> Int) -> (Int -> Int -> Bool) -> String -> Parser ()
condSep f cond name = do
    oldPos <- use sourcePos
    pos    <- getPosition
    unless (f pos `cond` f oldPos) $
        parserFail $ "Statement " ++ name ++ " (introduced at " ++ show oldPos ++ ")"

------------------------------------------------------------------------------