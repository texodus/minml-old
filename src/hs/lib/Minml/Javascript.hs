------------------------------------------------------------------------------

-- Code Generation

-- Q: What is JMacro?

-- A: JMacro is a library for the programmatic generation of Javascript code.
--    It is designed to be multipurpose -- it is useful whether you are 
--    writing nearly vanilla Javascript or you are programmatically generating
--    Javascript either in an ad-hoc fashion or as the backend to a compiler
--    or EDSL. (1)

-- Sounds useful, if only we were in the midst of writing a Javascript
-- compiler backend ...

-- (1) http://www.haskell.org/haskellwiki/Jmacro

------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

module Minml.Javascript (
    generateJs
) where

import Language.Javascript.JMacro

import Minml.AST
import Minml.Javascript.Expr()

------------------------------------------------------------------------------

generateJs :: (ToJExpr a) => a -> Either Err JStat
generateJs = Right . consoleLog . toJExpr

    where
        consoleLog x = scopify [jmacro| 
            var y = `(x)`;   
            console.log(y);
        |]                       

------------------------------------------------------------------------------
