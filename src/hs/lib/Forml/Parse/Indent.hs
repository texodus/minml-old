------------------------------------------------------------------------------

-- A library for writing shitespace aware parsers with parsec.

------------------------------------------------------------------------------

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

withScope :: Parser a -> Parser a
withScope parser = do
    st  <- getState
    pos <- getPosition
    setState pos
    res <- parser
    setState st
    return res

inScope :: Parser ()
inScope = condSep sourceLine (>) "in scope" >> condSep sourceColumn (>=) "in scope"

indented :: Parser ()
indented = condSep sourceColumn (>) "indented"

withSep :: Parser a -> Parser a
withSep parser =
    (semi >> parser) <|> (inScope >> withScope parser)

sep :: Parser ()
sep = (semi >> return ()) <|> inScope

condSep :: (SourcePos -> Int) -> (Int -> Int -> Bool) -> String -> Parser ()
condSep f cond name = do
    st  <- getState
    pos <- getPosition
    if f pos `cond` f st
        then return ()
        else parserFail $ "Statement " ++ name ++ " (introduced at " ++ show st ++ ")"

------------------------------------------------------------------------------