------------------------------------------------------------------------------

-- A library for writing shitespace aware parsers with parsec.

------------------------------------------------------------------------------

{-# LANGUAGE RankNTypes #-}

module Forml.Parse.Indent (
    indented,
    withSep,
    withScope,
    sep,
    inScope
) where

import Text.Parsec

import Forml.Parse.Token

------------------------------------------------------------------------------

-- | Introduces a new scope around a parser.  Future invocations of
--   `inScope`, for example, will reference the `SourcePos` stored from the
--   most recent invocation of `withScope`, or `initialPos` if it has
--   not been invoked.

withScope :: Parser s a -> Parser s a
withScope parser = do
    (st, rs)  <- getState
    pos <- getPosition
    setState (pos, rs)
    res <- parser
    setState (st, rs)
    return res

inScope :: Parser s ()
inScope = condSep sourceLine (>) "in scope" >> condSep sourceColumn (>=) "in scope"

indented :: Parser s ()
indented = condSep sourceColumn (>) "indented"

withSep :: Parser s a -> Parser s a
withSep parser =
    (semi >> parser) <|> (inScope >> withScope parser)

sep :: Parser s ()
sep = (semi >> return ()) <|> inScope

condSep :: (SourcePos -> Int) -> (Int -> Int -> Bool) -> String -> Parser s ()
condSep f cond name = do
    (st, _)  <- getState
    pos <- getPosition
    if f pos `cond` f st
        then return ()
        else parserFail $ "Statement " ++ name ++ " (introduced at " ++ show st ++ ")"

------------------------------------------------------------------------------