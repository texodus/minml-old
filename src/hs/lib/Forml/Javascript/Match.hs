------------------------------------------------------------------------------

-- We also saw `Match` earlier, a datatype to represent the javascript
-- generation of the application of a value to a pattern - as this is 
-- used in the cond position of an `if` statement earlier.

-- Literal matches are a simple equality test.

-- Symbol matches introduce a new property onto the `scope` object

-- Datatype constructors without arguments are checked via their `type`
-- property.

-- A `ConPatt` with arguments, however, needs to recursively unwrap its
-- arguments as `Match`s.

------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Forml.Javascript.Match (
    MatchBind( .. ),
    Match( .. )
) where

import Data.Monoid
import Language.Javascript.JMacro

import Forml.AST
import Forml.Javascript.Lit()
import Forml.Javascript.Ref

------------------------------------------------------------------------------

data Match     = Match JExpr Patt deriving (Show)
data MatchBind = MatchBind JExpr Patt deriving (Show)

instance ToStat MatchBind where

    toStat (MatchBind val (ValPatt (SymVal (Sym s)))) = [jmacro|

        `(DeclStat (StrI s) Nothing)`;
        `(ref s)` = `(val)`;

    |]

    toStat (MatchBind val (ConPatt (TypeSymP _) ps)) = 
        conds mappend toStat MatchBind val ps

    toStat _ = mempty

instance ToJExpr Match where

    toJExpr (Match val (ValPatt (LitVal l))) =
        [jmacroE| `(l)` == `(val)` |]

    toJExpr (Match _ (ValPatt (SymVal _))) =
        [jmacroE| true |]

    toJExpr (Match val (ValPatt (ConVal (TypeSym (TypeSymP sym))))) =
        InfixExpr " instanceof " val (SelExpr (ref sym) (StrI "__type__"))

    toJExpr (Match _ (ValPatt (ConVal t))) =
        error $ "FATAL: " ++ show t

    toJExpr (Match val (ConPatt (TypeSymP sym) ps)) = [jmacroE|

        `(Match val (ValPatt (ConVal (TypeSym (TypeSymP sym)))))`
            && `(conds (InfixExpr "&&") toJExpr Match val ps)`

    |]

conds f g c val ps = 
    foldl1 f (g `fmap` zipWith c args ps)
    where
        args  = toAcc val `fmap` [0 .. length ps]

toAcc :: JExpr -> Int -> JExpr
toAcc val n = 
    [jmacroE| `(val)`[`(n)`] |]

------------------------------------------------------------------------------
