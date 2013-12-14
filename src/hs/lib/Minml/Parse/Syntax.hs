------------------------------------------------------------------------------

-- | The `Syntax` class is the framework for parsing, providing a polymorphic
--   abstraction for mapping text to AST, as well as enabling seamless 
--   circular dependencies.

------------------------------------------------------------------------------

module Minml.Parse.Syntax where

import Minml.Parse.Token

------------------------------------------------------------------------------

class Syntax a where syntax :: Parser a

------------------------------------------------------------------------------
