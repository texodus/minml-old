------------------------------------------------------------------------------

-- | Notation parsing

------------------------------------------------------------------------------

module Forml.Macro.Scope( inferScope ) where

import Control.Applicative

import Forml.AST

------------------------------------------------------------------------------

inferScope :: Macro Expr -> Macro Expr
inferScope (MacroTerm (Let x) (MacroList xs)) =
    MacroTerm (Let x) (MacroList (applyScope 0 . inferScope <$> xs))

inferScope (MacroTerm x (MacroList xs)) =
    MacroTerm x (MacroList (inferScope <$> xs))

inferScope x = x

applyScope :: Int -> Macro Expr -> Macro Expr
applyScope n y =
    applyScope' n y
    where
        applyScope' 0 (MacroTerm Sep _) =
            skipTokens y

        applyScope' m (MacroTerm Sep (MacroList [x])) =
            applyScope' (m - 1) x

        applyScope' m (MacroTerm Scope (MacroList [x])) =
            applyScope' (m + 1) x

        applyScope' m (MacroTerm _ (MacroList [x])) =
            applyScope' m x

        applyScope' _ _ = y

        skipTokens (MacroTerm (Token c) (MacroList [x])) =
            MacroTerm (Token c) (MacroList [skipTokens x])

        skipTokens x =
            MacroTerm Scope (MacroList [x])

------------------------------------------------------------------------------
