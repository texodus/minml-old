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

import Language.Javascript.JMacro

import Forml.AST
import Forml.Javascript.Lit()

------------------------------------------------------------------------------

data Match = Match JExpr Patt JExpr deriving (Show)

instance ToJExpr Match where

    toJExpr (Match val (ValPatt (LitVal l)) _) =

        [jmacroE| `(l)` == `(val)` |]

    toJExpr (Match val (ValPatt (SymVal (Sym s))) scope) = [jmacroE|

        (function() {
            `(scope)`[`(s)`] = `(val)`;
            return true;
        })()

    |]

    toJExpr (Match val (ValPatt (ConVal (TypeSym (TypeSymP s)))) _) =

        [jmacroE| `(val)`.type == `(s)` |]

    toJExpr (Match _ (ValPatt (ConVal t)) _) =

        error $ "FATAL: " ++ show t

    toJExpr (Match val (ConPatt (TypeSymP sym) ps) scope) = [jmacroE|

        `(val)`.type == `(sym)` && (function() {
            var result = true;
            for (var arg in `(val)`.attrs) {
                var argg = `(val)`.attrs[arg];
                result = result && `(conds argg ps scope)`;
            }
            return result;
        })()

    |]

conds :: JExpr -> [Patt] -> JExpr -> JExpr

conds _ [] _ = [jmacroE| true |]
conds val (x : xs) scope = [jmacroE|

    `(Match val x scope)` && `(conds val xs scope)`

|]

------------------------------------------------------------------------------
