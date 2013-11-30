------------------------------------------------------------------------------

-- | Notation parsing

------------------------------------------------------------------------------

module Forml.Macro.Scope( inferScope ) where

import Control.Applicative

import Forml.AST

------------------------------------------------------------------------------

inferScope :: Macro Expr -> Macro Expr
inferScope (Term (Let x) (MacList xs)) =
    Term (Let x) (MacList (applyScope 0 . inferScope <$> xs))

inferScope (Term x (MacList xs)) =
    Term x (MacList (inferScope <$> xs))

inferScope x = x

applyScope :: Int -> Macro Expr -> Macro Expr
applyScope n y =
    applyScope' n y
    where
        applyScope' 0 (Term Sep _) =
            skipTokens y

        applyScope' m (Term Sep (MacList [x])) =
            applyScope' (m - 1) x

        applyScope' m (Term Scope (MacList [x])) =
            applyScope' (m + 1) x

        applyScope' m (Term _ (MacList [x])) =
            applyScope' m x

        applyScope' _ _ = y

        skipTokens (Term (Token c) (MacList [x])) =
            Term (Token c) (MacList [skipTokens x])

        skipTokens x =
            Term Scope (MacList [x])

------------------------------------------------------------------------------
