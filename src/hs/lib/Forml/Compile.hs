
-- (1) [Write Yourself a Scheme in 48hrs]
--     (http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours),

-- (2) [Parsec]
--     (http://www.haskell.org/haskellwiki/Parsec)

-- (3) [JMacro]
--     (http://www.haskell.org/haskellwiki/Jmacro)

-- (4) [Typing Haskell in Haskell]
--     (http://web.cecs.pdx.edu/~mpj/thih/)

-- (5) [OHML]
--     (https://github.com/texodus/ohml)

-- I. Introduction
-- ===============

-- We can accomplish this in Haskell '98 - but it's not fun!
-- Let's make things complicated by using some extensions!

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}

------------------------------------------------------------------------------

module Forml.Compile where

import Control.Applicative
import Control.Arrow
import Control.Lens
import Control.Monad
import Data.Maybe
import Data.Monoid
import Text.Parsec.Pos

import Forml.AST
import Forml.Config
import Forml.Javascript
import Forml.Parse
import Forml.Parse.Token
import Forml.Prelude
import Forml.RenderText
import Forml.TypeCheck

------------------------------------------------------------------------------

-- So what does this language look like?

-- 2. Compiler "Architecture"
-- ==========================

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

type ParseArtifact = ([Expr], MacroState Expr)
    
initialState :: ParseArtifact
initialState = ([], emptyState)

parse :: ParseArtifact -> (SourceName, SourceCode) -> Either Err ParseArtifact
parse (xs, MacroState _ _ ms y) (name, src) =
    first ((xs ++) . (:[])) <$> parseForml name src (MacroState (initialPos "") ms ms y)

------------------------------------------------------------------------------
