module Unit.Parse.ExprSpec where

import Control.Applicative
import Control.Monad
import Test.Hspec
import Test.HUnit
import Test.QuickCheck

import Minml.Compile
import Minml.AST
import Minml.Prelude
import Minml.Utils

import Unit.Source
import Unit.Asts
import Utils()

assertFail :: String -> Either Err Expr -> Assertion
assertFail a b = assertEqual "" b (parseMinml a)

parseMinml :: SourceCode -> Either Err Expr
parseMinml a = 
    head . tail . fst <$> foldM parse ([], emptyState) [("Prelude", prelude), ("Test Case", a)]

reflect :: Expr -> Bool
reflect x = case parseMinml (fmt x) of
    Left e -> error (show e)
    Right _ -> True

spec :: Spec
spec =

    describe "Minml.Parser" $ do

        --it "GENERATING DATA" $ genData

        toSpec "parseing" (\x -> assertFail (sources !! x) (Right $ asts !! x))

        describe "parsing failure cases" $ do

            it "should parse anything it can format, and they should be the same" $ 

                property reflect

            it "should error when trying to parse a missing let continuation" $ assertFail

                "   let x = 1;   "
                $ Left (Err "\"Test Case\" (line 1, column 17):\nunexpected end of input\nexpecting Expression")

            it "should fail to parse match mixed semicolon" $ assertFail

                "   let fib = fun n ->                      \
                \       match n with                        \
                \       0 -> 0;                             \
                \       1 -> 1;                             \
                \       n -> fib (n - 1) + fib (n - 2);     \
                \   fib 7                                   "

                $ Left (Err "\"Test Case\" (line 1, column 223):\nunexpected \"7\"\nexpecting \"->\" or \"=\"")

------------------------------------------------------------------------------
