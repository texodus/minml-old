------------------------------------------------------------------------------

-- | The `Syntax` class is the framework for parsing, providing a polymorphic
--   abstraction for mapping text to AST, as well as enabling seamless 
--   circular dependencies.

------------------------------------------------------------------------------

module Forml.Parse.Syntax where

import Forml.Parse.Token

------------------------------------------------------------------------------

class Syntax a where syntax :: Parser a

------------------------------------------------------------------------------
