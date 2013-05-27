------------------------------------------------------------------------------

-- Expression checking is the most complex.  Literals are lifted trivially.

-- If you read a theoretical treatment of HM, you will encounter equations
-- that look like this.  The cases of `exprCheck` map directly to these
-- lifted from wikipedia:

--  x : σ ∈ Γ
--  ----------
--  Γ ⊦ x : σ

-- where
--  σ    = a type scheme, `TypeAbs Kind`
--  τ    = a type, `Type Kind`
--  Γ    = a type environment, `TypeCheck a`
--  ⊦     = an assertion.
--  :    = an assumption,  `Ass` type
-- ---   = a judgment, premise is the numerator, conclusion is the denominator

-- `TypExpr a` simply requires that we introduce a new assumption for this
-- constructor.  We borrow part of our generalization mechanism to make
-- these constructors fully polymorphic for all free type variables.
-- This generalization is implied by the premise `eₒ : σ`

--  Γ ⊦ eₒ : σ   Γ, x : σ ⊦ e₁ : τ
--  ------------------------------
--  Γ ⊦ let x = eₒ in e₁ : τ

-- Application checking simply involves verifying the parameter types unify.

--  Γ ⊦ eₒ : τ → τ'   Γ ⊦ e₁ : τ
--  ----------------------------
--  Γ ⊦ eₒ e₁ : τ'

-- Abstraction in our language is easy to typecheck, as we will not
-- generalize the parameter x (notice that `x : τ`)

--  Γ, x : τ ⊦ e : τ'
--  --------------------
--  Γ ⊦ λ x . e : τ → τ' 

-- Pattern matching is just a special case of abstraction & application.
-- We accomplish this recursively to make sure each case is the same.

------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Forml.TypeCheck.Expr (
    exprCheck
) where

import Forml.AST
import Forml.TypeCheck.Kind
import Forml.TypeCheck.Prelude
import Forml.TypeCheck.Ass
import Forml.TypeCheck.Generalize
import Forml.TypeCheck.Subst
import Forml.TypeCheck.TypeCheck
import Forml.TypeCheck.Unify
import Forml.TypeCheck.Lit
import Forml.TypeCheck.Patt

------------------------------------------------------------------------------

exprCheck :: [Ass] -> Expr -> TypeCheck (Type Kind)

exprCheck _ (VarExpr (LitVal l)) =
    return (litCheck l)

exprCheck as (VarExpr (SymVal (Sym sym))) =
    find sym as >>= freshInst

exprCheck as (VarExpr (ConVal (TypeSym (TypeSymP sym)))) =
    find sym as >>= freshInst

exprCheck _ (VarExpr (ConVal t)) =
    error $ "FATAL: " ++ show t

exprCheck as (TypExpr (TypeSymP name) (TypeAbsP typ) expr) =
    exprCheck (name :>: typKAbs : as) expr

    where
        typK = toKind Star typ
        typKAbs = quantify (getVars typK) typK

exprCheck as (LetExpr (Sym sym) val expr) = do
    symT <- newTypeVar Star
    valT <- exprCheck ((sym :>: TypeAbsT [] symT) : as) val
    unify valT symT
    schT <- generalize as valT 
    exprCheck (sym :>: schT : as) expr

exprCheck as (AppExpr f x) = do
    fT   <- exprCheck as f
    xT   <- exprCheck as x
    appT <- newTypeVar Star
    unify (xT `fn` appT) fT
    return appT

exprCheck as (AbsExpr (Sym sym) expr) = do
    symT <- newTypeVar Star
    res  <- exprCheck (sym :>: TypeAbsT [] symT : as) expr
    return (symT `fn` res)

exprCheck as (MatExpr expr patts) = do
    exprT <- exprCheck as expr
    argCheck exprT patts

    where
        argCheck exprT ((patt, res):es) = do
            (pattAs, pattT) <- pattCheck as patt
            unify exprT pattT
            argRecCheck exprT pattAs res es

        argCheck _ [] = error $ "FATAL: argCheck"

        argRecCheck _ pattAs res [] =
            exprCheck (pattAs ++ as) res

        argRecCheck exprT pattAs res es = do
            resT  <- exprCheck (pattAs ++ as) res
            esT   <- argCheck exprT es
            unify resT esT
            return resT

------------------------------------------------------------------------------