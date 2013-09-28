------------------------------------------------------------------------------

-- A library for writing shitespace aware parsers with parsec.

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

inScope :: Parser s ()
inScope = condSep sourceLine (>) "in scope" >> condSep sourceColumn (>=) "in scope"

indented :: Parser s ()
indented = condSep sourceColumn (>) "indented"

withSep :: Parser s a -> Parser s a
withSep parser =
    (semi >> parser) <|> (inScope >> withScope parser)

withCont :: Parser s a -> Parser s a
withCont parser =
    (condSep sourceLine (==) "inline" >> parser) <|> (inScope >> withScope parser)


sep :: Parser s ()
sep = void semi <|> inScope

condSep :: (SourcePos -> Int) -> (Int -> Int -> Bool) -> String -> Parser s ()
condSep f cond name = do
    oldPos <- use sourcePos
    pos    <- getPosition
    if f pos `cond` f oldPos
        then return ()
        else parserFail $ "Statement " ++ name ++ " (introduced at " ++ show oldPos ++ ")"

------------------------------------------------------------------------------