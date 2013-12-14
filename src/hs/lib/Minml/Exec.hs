
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

module Minml.Exec where

import Control.Lens
import System.Console.CmdLib

import Minml.Compile
import Minml.Config

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
    case compile config ((config ^. sourceFiles) `zip` srcs) of
        Left  e  -> print e
        Right js -> putStrLn js

------------------------------------------------------------------------------
