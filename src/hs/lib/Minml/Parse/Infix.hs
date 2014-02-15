------------------------------------------------------------------------------

-- | Infix associativity resolver

------------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Minml.Parse.Infix where

import Control.Arrow
import Control.Applicative
import Control.Lens
import Control.Monad.Identity
import Text.Parsec.Expr

import Minml.AST
import Minml.Replace
import Minml.Parse.Indent
import Minml.Parse.Macro
import Minml.Parse.Patt()
import Minml.Parse.Record()
import Minml.Parse.Syntax
import Minml.Parse.Token
import Minml.Parse.Type()
import Minml.Parse.Val()

------------------------------------------------------------------------------

type OpTable = [Operator String MacroState Identity Expr]

instance Syntax Expr => Syntax [OpTable] where
    syntax = do
        MacTree macs <- use macros
        return $ [[ Infix appl AssocLeft ]]
            ++ toInfixTerm opConst AssocLeft (tail ops)
            ++ [ foldl macOps [] macs ]

macOps :: Syntax Expr => OpTable -> Macro Expr -> OpTable
macOps opss (Term (Arg x) (MacTree ys)) =
    opss ++ (Postfix . ggg <$> ys)
    where
        ggg :: Macro Expr -> Parser (Expr -> Expr)
        ggg (Term (Token y) ms) = do
            reservedOp y
            cont <- macroPRec ms
            return (\z -> replace x z . ($ undefined) . fst $ cont)

        ggg _ = error "PARADOX: This should not be"

macOps opss _ = opss

toInfixTerm :: Functor f => 
    (String -> a -> a -> a) -> Assoc -> f (f String) -> 
        f (f (Operator String MacroState Identity a))

toInfixTerm optr assoc =
    fmap . fmap $
        flip Infix assoc
        <<< uncurry (*>)
        <<< reservedOp
        &&& return . optr

appl :: Parser (Expr -> Expr -> Expr)
appl = indented >> return AppExpr

opConst :: String -> Expr -> Expr -> Expr
opConst = (AppExpr .) . AppExpr . VarExpr . SymVal . Sym
    
------------------------------------------------------------------------------
