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

withScope :: Parser s a -> Parser s a
withScope parser = do
    st  <- get
    pos <- getPosition
    sourcePos .= pos
    res <- parser
    setState st
    return res

-- | Fails if the cursor is not within the current scope

inScope :: Parser s ()
inScope = condSep sourceLine (>) "in scope" >> condSep sourceColumn (>=) "in scope"

-- | Fails if the cursor is not indented, or on the same line

indented :: Parser s ()
indented = condSep sourceColumn (>) "indented"

-- | Tries a parser as a continuation of an existing block

withSep :: Parser s a -> Parser s a
withSep parser =
    (semi >> parser) <|> (inScope >> withScope parser)

-- | Same as withSep, but without a semi

withCont :: Parser s a -> Parser s a
withCont parser =
    (condSep sourceLine (==) "inline" >> parser) <|> (inScope >> withScope parser)

-- | Line separator parser

sep :: Parser s ()
sep = void semi <|> inScope

-- | Optionally parse a separator in-scope

condSep :: (SourcePos -> Int) -> (Int -> Int -> Bool) -> String -> Parser s ()
condSep f cond name = do
    oldPos <- use sourcePos
    pos    <- getPosition
    if f pos `cond` f oldPos
        then return ()
        else parserFail $ "Statement " ++ name ++ " (introduced at " ++ show oldPos ++ ")"

------------------------------------------------------------------------------