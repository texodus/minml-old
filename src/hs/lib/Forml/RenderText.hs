------------------------------------------------------------------------------

-- | JExpr -> String

------------------------------------------------------------------------------

module Forml.RenderText (
    renderText
) where

import Language.Javascript.JMacro
import Text.PrettyPrint.Leijen.Text

import Forml.AST

------------------------------------------------------------------------------

-- | Renders a `JExpr` as a string

renderText :: (JsToDoc a, JMacro a) => a -> Either Err String
renderText = Right . show . renderOneLine . renderJs

------------------------------------------------------------------------------