
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}

------------------------------------------------------------------------------

module Minml.Compile(
    module Minml.Compile.Config,
    SourceCode,
    CompiledJS,
    compile,
    parse
) where

import Control.Applicative
import Control.Arrow
import Control.Lens
import Control.Monad
import Data.Maybe
import Data.Monoid
import Text.Parsec.Pos

import Minml.AST
import Minml.Compile.Config
import Minml.Compile.Prelude
import Minml.Compile.RenderText
import Minml.Javascript
import Minml.Parse
import Minml.Parse.Token
import Minml.TypeCheck

------------------------------------------------------------------------------

type SourceCode = String
type CompiledJS = String

compile :: Config -> [(SourceName, SourceCode)] -> Either Err CompiledJS
compile config srcs = do

    let srcs' =
            if config ^. implicitPrelude
            then ("Prelude", prelude) : srcs
            else srcs

    (asts, _) <- foldM parse initialState srcs'
    let ast = fromMaybe undefined (foldl1 (<>) (Just <$> asts))

    (config ^. shouldTypeCheck) `when` void (typeCheck ast)

    js <- generateJs ast
    renderText js

type ParseArtifact = ([Expr], MacroState)

initialState :: ParseArtifact
initialState = ([], emptyState)

parse :: ParseArtifact -> (SourceName, SourceCode) -> Either Err ParseArtifact
parse (xs, MacroState _ _ ms y) (name, src) =
    first ((xs ++) . (:[])) <$> parseMinml name src (MacroState (initialPos "") ms ms y)

------------------------------------------------------------------------------
