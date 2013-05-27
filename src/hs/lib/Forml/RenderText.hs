------------------------------------------------------------------------------

------------------------------------------------------------------------------

module Forml.RenderText (
    renderText
) where

import Language.Javascript.JMacro
import Text.PrettyPrint.Leijen.Text

import Forml.AST

------------------------------------------------------------------------------

renderText :: JExpr  -> Either Err String
renderText = Right . show . renderOneLine . renderJs

------------------------------------------------------------------------------