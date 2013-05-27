module Unit.Javascript.ValSpec where

import Test.Hspec

import Forml.AST
import Forml.Javascript.Val()

import Utils

spec :: Spec
spec = do

    describe "Forml.Javascript.Val" $ do

    	describe "toJExpr" $ do

            it "should parse partial keywords" $ assertGenerate
                (Sym "func")  $ Right "func"

            it "should parse partial keywords" $ assertGenerate
                (Sym "lett") $ Right "lett"

        describe "valP" $ do

            it "should parse floats" $ assertGenerate
                (LitVal (NumLit 1.0)) $ Right "1.0"

            it "should parse non-keywords" $ assertGenerate
                (LitVal (StrLit "test")) $ Right "\"test\""

            it "should parse constructors" $ assertGenerate
                (ConVal (TypeSym (TypeSymP "True"))) $ Right "True"

            it "should parse symbols" $ assertGenerate
                (SymVal (Sym "x")) $ Right "x"

