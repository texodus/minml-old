module Unit.Javascript.LitSpec where

import Test.Hspec

import Minml.AST
import Minml.Javascript.Lit()

import Utils

spec :: Spec
spec = do

    describe "Minml.Javascript.Lit" $ do

        describe "toJExpr" $ do

            it "should parse integers" $ assertGenerate
                (NumLit 1) $ Right "1.0"

            it "should parse floats" $ assertGenerate
                (NumLit 3.14) $ Right "3.14"

            it "should parse strings" $ assertGenerate
                (StrLit "test") $ Right "\"test\""

            it "should parse stringswith escapes " $ assertGenerate
                (StrLit "te\"st") $ Right "\"te\\\"st\""


