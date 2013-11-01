------------------------------------------------------------------------------

-- | We also saw `Match` earlier, a datatype to represent the javascript
--   generation of the application of a value to a pattern - as this is 
--   used in the cond position of an `if` statement earlier.

--   Literal matches are a simple equality test.

--   Symbol matches introduce a new property onto the `scope` object

--   Datatype constructors without arguments are checked via their `type`
--   property.

--   A `ConPatt` with arguments, however, needs to recursively unwrap its
--   arguments as `Match`s.

------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Forml.Javascript.Match (
    Match( .. ),
    getBindings
) where

import qualified Data.Map as M
import Language.Javascript.JMacro

import Forml.AST
import Forml.Javascript.Lit()

------------------------------------------------------------------------------

data Match = Match JExpr Patt deriving (Show)

instance ToJExpr Match where

    toJExpr (Match val (ValPatt (LitVal l))) =
        [jmacroE| `(l)` == `(val)` |]

    toJExpr (Match _ (ValPatt (SymVal _))) =
        [jmacroE| true |]

    toJExpr (Match val (ValPatt (ConVal (TypeSym (TypeSymP sym))))) =
        InfixExpr " instanceof " val (SelExpr (jsv sym) (StrI "__type__"))

    toJExpr (Match _ (ValPatt (ConVal t))) =
        error $ "FATAL: " ++ show t

    toJExpr (Match val (RecPatt (Record (unzip . M.toList -> (ks, ps))))) = [jmacroE|

        `(getGenConds ks (InfixExpr "&&") ((toJExpr .) . Match) val ps)`

    |]

    toJExpr (Match val (ConPatt (TypeSymP sym) [])) = [jmacroE|

        `(Match val (ValPatt (ConVal (TypeSym (TypeSymP sym)))))`

    |]

    toJExpr (Match val (ConPatt (TypeSymP sym) ps)) = [jmacroE|

        `(Match val (ValPatt (ConVal (TypeSym (TypeSymP sym)))))`
            && `(getConds (InfixExpr "&&") ((toJExpr .) . Match) val ps)`

    |]

getBindings val (ValPatt (SymVal (Sym s))) = [(s, val)]
getBindings val (ConPatt (TypeSymP _) []) = []
getBindings val (ConPatt (TypeSymP _) ps) = getConds (++) getBindings val ps
getBindings val (RecPatt (Record (unzip . M.toList -> (ks, ps)))) =
    getGenConds ks (++) getBindings val ps
getBindings _ _ = []

getGenConds keys f c val ps = 
    foldl1 f (zipWith c args ps)
    where
        args  = toAcc val `fmap` keys 

getConds f c val ps = getGenConds [0 .. length ps] f c val ps

toAcc :: ToJExpr a => JExpr -> a -> JExpr
toAcc val n = [jmacroE| `(val)`[`(n)`] |]

------------------------------------------------------------------------------
