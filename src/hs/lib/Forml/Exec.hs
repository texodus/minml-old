
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

{-# OPTIONS_GHC -fno-warn-missing-fields #-}

------------------------------------------------------------------------------

-- There is one module.  We'll need a few elements from `System` for
-- handling the plumbing aspects, and a handful of elements from the
-- `containers` and `mtl` libraries.

module Forml.Exec where

import Control.Applicative
import Control.Arrow
import Control.Lens
import Control.Monad
import Data.Maybe
import Data.Monoid
import System.Console.CmdLib
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

-- The structure of compilation can be expressed as a simple
-- function composition.

exec :: IO ()
exec = do
    args   <- getArgs
    config <- executeR Config {} args
    srcs   <- mapM readFile (config ^. sourceFiles)
    case compile config srcs of
        Left  e  -> print e
        Right js -> putStrLn js

compile :: Config -> [String] -> Either Err String
compile config srcs = do

    let srcs' =
            if config ^. implicitPrelude
            then prelude : srcs 
            else srcs

    (asts, _) <- foldM parse initialState srcs'
    let ast = fromMaybe undefined (foldl1 (<>) (Just <$> asts))

    (config ^. shouldTypeCheck) `when` void (typeCheck ast)

    js <- generateJs ast
    renderText js

    
initialState :: ([Expr], MacroState Expr)
initialState = ([], emptyState)

parse :: ([Expr], MacroState Expr) -> String -> Either Err ([Expr], MacroState Expr)
parse (xs, MacroState _ _ ms) src =
    first ((xs ++) . (:[])) <$> parseForml src (MacroState (initialPos "") ms ms)

------------------------------------------------------------------------------
