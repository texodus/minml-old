module Unit.Parse.ValSpec where

import Test.Hspec

import Forml.AST
import Forml.Parse.Val()
import Forml.Parse.Token
import Forml.Parse.Syntax

import Utils

symP :: Parser Sym
symP = syntax

valP :: Parser Val
valP = syntax

spec :: Spec
spec =

    describe "Forml.Parse.Val" $ do

        describe "symP" $ do

            it "should parse non-keywords" $ assertParse symP
                "func" $ Right (Sym "func")

            it "should parse integers" $ assertParse symP
                "lett" $ Right (Sym "lett")

            it "should fail to parse keywords" $ assertParse symP
                "with" $ Left (Err "(line 1, column 5):\nunexpected reserved word \"with\"\nexpecting symbol")

            it "should fail to parse integers" $ assertParse symP
                "NotASym" $ Left (Err "(line 1, column 1):\nunexpected \"N\"\nexpecting symbol")

        describe "valP" $ do

            it "should parse floats" $ assertParse valP
                "1.0" $ Right (LitVal (NumLit 1.0))

            it "should parse non-keywords" $ assertParse valP
                "\"test\"" $ Right (LitVal (StrLit "test"))

            it "should parse constructors" $ assertParse valP
                "True" $ Right (ConVal (TypeSym (TypeSymP "True")))

            it "should parse symbols" $ assertParse valP
                "x" $ Right (SymVal (Sym "x"))

            it "should fail to parse keywords" $ assertParse valP
                "with" $ Left (Err "(line 1, column 5):\nunexpected reserved word \"with\"\nexpecting symbol")

