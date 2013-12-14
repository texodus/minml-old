------------------------------------------------------------------------------

-- | Infix associativity resolver

------------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Minml.Parse.Infix where

import Control.Applicative
import Control.Lens
import Control.Monad.Identity
import Text.Parsec.Expr

import Minml.AST
import Minml.AST.Replace
import Minml.Parse.Macro
import Minml.Parse.Patt()
import Minml.Parse.Record()
import Minml.Parse.Syntax
import Minml.Parse.Token
import Minml.Parse.Type()
import Minml.Parse.Val()

------------------------------------------------------------------------------

type OpTable = [Operator String MacroState Identity Expr]

instance Syntax Expr => Syntax OpTable where
    syntax = do
        MacList macs <- use macros
        return $ foldl macOps [] macs


macOps :: Syntax Expr => OpTable -> Macro Expr -> OpTable
macOps opss (Term (Arg x) (MacList ys)) =
    opss ++ [Postfix $ foldl1 (<|>) (ggg x `fmap` ys)]
    where
        ggg st (Term (Token y) ms) = do
            reservedOp y
            cont <- macroPRec ms
            return (\z -> replace st z . ($ undefined) . fst $ cont)

        ggg _ _ = error "PARADOX: This should not be"

macOps opss _ = opss
    
------------------------------------------------------------------------------
