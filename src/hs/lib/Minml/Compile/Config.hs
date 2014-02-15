
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

{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

------------------------------------------------------------------------------

-- There is one module.  We'll need a few elements from `System` for
-- handling the plumbing aspects, and a handful of elements from the
-- `containers` and `mtl` libraries.

module Minml.Compile.Config where

import Control.Lens
import System.Console.CmdLib

------------------------------------------------------------------------------

-- So what does this language look like?

-- 2. Compiler "Architecture"
-- ==========================

-- The structure of compilation can be expressed as a simple
-- function composition.

data Config = Config {
    _implicitPrelude :: Bool,
    _sourceFiles     :: [String],
    _shouldTypeCheck :: Bool
} deriving (Eq, Show, Data, Typeable)

makeLenses ''Config

defaultConfig :: Config
defaultConfig = Config True [] True

instance Attributes Config where
    attributes _ = group "Options" [
        _implicitPrelude %> [
            Short "p",
            Long ["prelude"],
            Default True,
            Help "Insert prelude in type checking and code gen"
        ],

        _sourceFiles %> [
            Default ([] :: [String]),
            Extra True,
            Help "Delicious source files"
        ],

        _shouldTypeCheck %> [
            Short "t",
            Long ["typecheck"],
            Default True,
            Help "Type check the sources" 
        ]]

instance RecordCommand Config where
     mode_summary _ = "Welcome to the MIN.ML Programming Language.\n"
     run' = undefined

------------------------------------------------------------------------------
