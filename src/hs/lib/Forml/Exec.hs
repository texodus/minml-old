
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

{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

------------------------------------------------------------------------------

-- There is one module.  We'll need a few elements from `System` for
-- handling the plumbing aspects, and a handful of elements from the
-- `containers` and `mtl` libraries. 

module Forml.Exec where

import Control.Applicative
import Control.Monad
import System.Environment

import Forml.AST
import Forml.Parse
import Forml.TypeCheck
import Forml.Javascript
import Forml.RenderText

------------------------------------------------------------------------------

-- So what does this language look like?

-- 2. Compiler "Architecture"
-- ==========================

-- The structure of compilation can be expressed as a simple
-- function composition.  

compile :: String -> Either Err String
compile = parseForml >=> typeCheck >=> generateJs >=> renderText

exec :: IO ()
exec = do
    file   <- head <$> getArgs
    source <- readFile file
    case compile source of
        Left  e  -> print e
        Right js -> putStrLn js

------------------------------------------------------------------------------