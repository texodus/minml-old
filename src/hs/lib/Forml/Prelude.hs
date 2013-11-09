
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
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

------------------------------------------------------------------------------

-- There is one module.  We'll need a few elements from `System` for
-- handling the plumbing aspects, and a handful of elements from the
-- `containers` and `mtl` libraries.

module Forml.Prelude where

import qualified Data.ByteString.UTF8 as B

import Data.FileEmbed
import Data.Monoid
import Text.Parsec
import Text.Parsec.Pos

import Forml.AST
import Forml.Parse.Notation
import Forml.Parse.Token

------------------------------------------------------------------------------

-- So what does this language look like?

-- 2. Compiler "Architecture"
-- ==========================

-- The structure of compilation can be expressed as a simple
-- function composition.

bootstrap :: MacroList Expr
bootstrap = foldl1 mappend

    -- Let
    [ parseNote "fun (x) -> (y)"       (AbsExpr (Sym "x") (VarExpr (SymVal (Sym "y"))))
    , parseNote "let (a) = (b); (c)"   (LetExpr (Sym "a") (VarExpr (SymVal (Sym "b"))) (Just (VarExpr (SymVal (Sym "c")))))
    ]

    where
        parseNote a = case runParser notationP emptyState "" a of
            Left x  -> error . show $ x
            Right x -> MacroList . (:[]) . x

emptyState :: MacroState Expr
emptyState = MacroState (initialPos "") bootstrap bootstrap 0

prelude :: String
prelude = B.toString $(embedFile "/Users/slink/work/forml2/src/forml/prelude.forml")

------------------------------------------------------------------------------
