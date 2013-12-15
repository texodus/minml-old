module Unit.Parse.ExprSpec where

import Control.Applicative
import Control.Monad
import Test.Hspec
import Test.HUnit

import Minml.Compile
import Minml.AST
import Minml.Prelude
import Unit.Source
import Unit.Asts

assertParse :: String -> Either Err Expr -> Assertion
assertParse a b = assertEqual "" b (head . tail . fst <$> foldM parse ([], emptyState) [("Prelude", prelude), ("Test Case", a)])

spec :: Spec
spec =

    describe "Minml.Parser" $ do

        --it "GENERATING DATA" $ genData

        toSpec "parseing" (\x -> assertParse (sources !! x) (Right $ asts !! x))

        describe "parsing failure cases" $ do

            it "should parse anything it can format, and they should be the same" $ pendingWith "TODO this is useful, but transforms make this difficult"
 
                -- property $ \x -> parseMinml (fmt x) == Right x

            it "should error when trying to parse a missing let continuation" $ assertParse

                "   let x = 1;   "
                $ Left (Err "\"Test Case\" (line 1, column 17):\nunexpected end of input\nexpecting Expression")

            it "should fail to parse match mixed semicolon" $ assertParse

                "   let fib = fun n ->                      \
                \       match n with                        \
                \       0 -> 0;                             \
                \       1 -> 1;                             \
                \       n -> fib (n - 1) + fib (n - 2);     \
                \   fib 7                                   "

                $ Left (Err "\"Test Case\" (line 1, column 223):\nunexpected \"7\"\nexpecting \"->\" or \"=\"")

------------------------------------------------------------------------------
