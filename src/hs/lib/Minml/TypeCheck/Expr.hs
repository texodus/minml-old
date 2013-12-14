------------------------------------------------------------------------------

-- Expression checking is the most complex.  Literals are lifted trivially.

-- If you read a theoretical treatment of HM, you will encounter equations
-- that look like this.  The cases of `infer` map directly to these
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
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Minml.TypeCheck.Expr (
    infer
) where

import Control.Lens
import Control.Monad

import qualified Data.Map as M

import Minml.AST
import Minml.TypeCheck.Kind
import Minml.TypeCheck.Prelude
import Minml.TypeCheck.Ass
import Minml.TypeCheck.Generalize
import Minml.TypeCheck.Subst
import Minml.TypeCheck.TypeCheck
import Minml.TypeCheck.Unify
import Minml.TypeCheck.Lit
import Minml.TypeCheck.Patt()

------------------------------------------------------------------------------

instance Infer Expr where
    infer (VarExpr (LitVal l)) =
        return (litCheck l)

    infer (VarExpr (SymVal (Sym sym))) =
        find sym >>= infer

    infer (VarExpr (ConVal (TypeSym (TypeSymP sym)))) =
        find sym >>= infer

    infer (JSExpr _) =
        newTypeVar Star

    infer (VarExpr (ConVal t)) =
        error $ "FATAL: " ++ show t

    infer (RecExpr (Record (unzip . M.toList -> (ks, vs)))) =
        liftM (TypeRec . Record . M.fromList . zip ks) (mapM infer vs)

    infer (TypExpr (TypeSymP name) (TypeAbsP typ) (Just expr)) =
        withAss (name :>: typKAbs) $ infer  expr
        
        where
            typK = toKind Star typ
            typKAbs = quantify (getVars typK) typK

    infer (LetExpr (Sym sym) val (Just expr)) = do
        symT <- newTypeVar Star
        valT <- withAss (sym :>: TypeAbsT [] symT) $ infer val
        unify valT symT
        schT <- generalize valT
        withAss (sym :>: schT) $ infer expr

    infer (LetExpr _ _ Nothing) = error "FATAL: incomplete"

    infer (TypExpr _ _ Nothing) = error "FATAL: incomplete"

    infer (AppExpr f x) = do
        fT   <- infer f
        xT   <- infer x
        appT <- newTypeVar Star
        unify (xT `fn` appT) fT
        return appT

    infer (AbsExpr (Sym sym) expr) = do
        symT <- newTypeVar Star
        res  <- withAss (sym :>: TypeAbsT [] symT) $ infer expr
        return (symT `fn` res)

    infer (MatExpr expr patts) = do
        exprT <- infer expr
        argCheck exprT patts

        where
            argCheck exprT [(patt, res)] = do
                pattT <- infer patt
                unify exprT pattT
                infer res

            argCheck exprT (cse:es) = do
                caseT <- scopeAss $ argCheck exprT [cse]
                resT <- argCheck exprT es
                unify caseT resT
                return caseT

            argCheck _ [] =
                error "FATAL: argCheck"


scopeAss :: TypeCheck a -> TypeCheck a
scopeAss tc = do
    as <- use ass 
    comAss as tc

withAss :: Ass -> TypeCheck a -> TypeCheck a
withAss newAs tc = do
    as  <- ass <<%= (newAs :)
    comAss as tc

comAss :: [Ass] -> TypeCheck a -> TypeCheck a
comAss as tc = do 
    t   <- tc
    ass .= as
    return t


------------------------------------------------------------------------------