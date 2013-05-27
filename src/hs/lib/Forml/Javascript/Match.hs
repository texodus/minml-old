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

module Forml.Javascript.Match where

import Data.Monoid
import Language.Javascript.JMacro

import Forml.AST
import Forml.Javascript.Lit()
import Forml.Javascript.Ref

------------------------------------------------------------------------------


data MatchBind = MatchBind JExpr Patt deriving (Show)

instance ToStat MatchBind where

    toStat (MatchBind val (ValPatt (SymVal (Sym s)))) = [jmacro|

        `(DeclStat (StrI s) Nothing)`;
        `(ref s)` = `(val)`;

    |]

    toStat (MatchBind val (ConPatt (TypeSymP _) ps)) = 

        foldl1 mappend (fmap toStat (zipWith MatchBind (toAcc `fmap` [0 .. length ps]) ps))

        where
            toAcc n = [jmacroE| `(val)`[`(n)`] |]

    toStat _ = mempty

data Match = Match JExpr Patt deriving (Show)

instance ToJExpr Match where

    toJExpr (Match val (ValPatt (LitVal l))) =

        [jmacroE| `(l)` == `(val)` |]

    toJExpr (Match _ (ValPatt (SymVal _))) = [jmacroE|

        true

    |]

    toJExpr (Match val (ValPatt (ConVal (TypeSym (TypeSymP sym))))) =

        [jmacroE| `(InfixExpr " instanceof " val (SelExpr (ref sym) (StrI "__type__")))` |]

    toJExpr (Match _ (ValPatt (ConVal t))) =

        error $ "FATAL: " ++ show t

    toJExpr (Match val (ConPatt (TypeSymP sym) ps)) = [jmacroE|

        `(Match val (ValPatt (ConVal (TypeSym (TypeSymP sym)))))` && (function() {
            var result = true;
            for (var arg in `(val)`) {
                if (arg != "__type__") {
                    var argg = `(val)`[arg];
                    result = result && `(conds argg ps)`;
                }
            }
            return result;
        })()

    |]

conds :: JExpr -> [Patt] -> JExpr

conds _ [] = [jmacroE| true |]
conds val (x : xs) = [jmacroE|

    `(Match val x)` && `(conds val xs)`

|]

------------------------------------------------------------------------------
