module Unit.TypeCheck.ExprSpec where

import Test.Hspec
import Test.HUnit

import Minml.TypeCheck
import Minml.AST

import Unit.Types
import Unit.Source
import Unit.Asts

assertCheck :: Expr -> Either Err Expr -> Assertion
assertCheck a b = assertEqual "" b (typeCheck a)

spec :: Spec
spec =

    describe "Minml.type check" $

        toSpec "type check" (\x -> assertCheck (asts !! x) (types !! x))

------------------------------------------------------------------------------
