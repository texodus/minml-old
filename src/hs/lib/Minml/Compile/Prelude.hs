
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

module Minml.Compile.Prelude where

import qualified Data.ByteString.UTF8 as B

import Control.Monad
import Data.FileEmbed
import Text.Parsec
import Text.Parsec.Pos

import Minml.AST
import Minml.Parse.Notation
import Minml.Parse.Syntax
import Minml.Parse.Token

------------------------------------------------------------------------------

-- So what does this language look like?

-- 2. Compiler "Architecture"
-- ==========================

-- The structure of compilation can be expressed as a simple
-- function composition.

bootstrap :: MacTree Expr
bootstrap = 
    case foldM appendTree emptyTree defs of
        Right x -> x
        Left _ -> error "FATAL: Error in bootstrap"
    where
        parseNote a = case runParser syntax emptyState "" a of
            Left x  -> error . show $ x
            Right (Notation x) -> MacTree . (:[]) . x

        defs :: [MacTree Expr]
        defs =             
            [ parseNote 
                "fun (x) -> (y)" 
                (AbsExpr (Sym "x") (VarExpr (SymVal (Sym "y"))))

            , parseNote 
                "let (a) = (b); (c)"  
                (LetExpr (Sym "a") (VarExpr (SymVal (Sym "b"))) (Just (VarExpr (SymVal (Sym "c")))))
         
            ]



emptyState :: MacroState
emptyState = MacroState (initialPos "") bootstrap bootstrap 0

prelude :: String
prelude = B.toString $(embedFile "src/minml/prelude.minml")

------------------------------------------------------------------------------
