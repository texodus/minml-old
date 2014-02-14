------------------------------------------------------------------------------

-- | JExpr -> String

------------------------------------------------------------------------------

module Minml.Compile.RenderText (
    renderText
) where

import Language.Javascript.JMacro
import Text.PrettyPrint.Leijen.Text

import Minml.AST

------------------------------------------------------------------------------

-- | Renders a `JExpr` as a string

renderText :: (JsToDoc a, JMacro a) => a -> Either Err String
renderText = Right . show . renderOneLine . renderJs

------------------------------------------------------------------------------