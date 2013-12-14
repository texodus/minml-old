------------------------------------------------------------------------------

-- | Notation parsing

------------------------------------------------------------------------------

module Minml.Macro.Scope( inferScope ) where

import Control.Applicative

import Minml.AST

------------------------------------------------------------------------------

inferScope :: Macro Expr -> Macro Expr
inferScope (Term (Let x) (MacTree xs)) =
    Term (Let x) (MacTree (applyScope 0 . inferScope <$> xs))

inferScope (Term x (MacTree xs)) =
    Term x (MacTree (inferScope <$> xs))

inferScope x = x

applyScope :: Int -> Macro Expr -> Macro Expr
applyScope n y =
    applyScope' n y
    where
        applyScope' 0 (Term Sep _) =
            skipTokens y

        applyScope' m (Term Sep (MacTree [x])) =
            applyScope' (m - 1) x

        applyScope' m (Term Scope (MacTree [x])) =
            applyScope' (m + 1) x

        applyScope' m (Term _ (MacTree [x])) =
            applyScope' m x

        applyScope' _ _ = y

        skipTokens (Term (Token c) (MacTree [x])) =
            Term (Token c) (MacTree [skipTokens x])

        skipTokens x =
            Term Scope (MacTree [x])

------------------------------------------------------------------------------
