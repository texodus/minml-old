module Unit.Javascript.ExprSpec where

import Control.Monad
import Test.Hspec
import Test.HUnit

import Minml.AST
import Minml.Javascript
import Minml.RenderText

import Unit.Asts
import Unit.Scripts
import Unit.Source

assertGenerate :: Expr -> Either Err String -> Assertion
assertGenerate a b = assertEqual "" b (join . fmap renderText . generateJs $ a)


spec :: Spec
spec =

    describe "Minml.Javascript.Expr" $

        toSpec "scripts" (\x -> assertGenerate (asts !! x) (scripts !! x))

------------------------------------------------------------------------------