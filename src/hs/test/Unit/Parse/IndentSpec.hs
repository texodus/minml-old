module Unit.Parse.IndentSpec where

import Test.Hspec

import Text.Parsec

import Forml.AST
import Forml.Parse.Indent
import Forml.Parse.Token

import Utils

spec :: Spec
spec = do

    describe "Forml.Parser.Indent" $ do

        describe "indented" $ do

            it "should respect indented" $ assertParse

                (spaces >> withScope (reserved "test" >> indented >> reserved "test"))

                "    test\n     test"
                (Right ())

            it "should fail when not indented" $ assertParse

                (spaces >> withScope (reserved "test" >> indented >> reserved "test"))

                "    test\n    test"
                (Left (Err "(line 2, column 5):\nunexpected \"t\"\nStatement indented (introduced at (line 1, column 5))"))

    	describe "withSep" $ do

            it "should respect semicolons" $ assertParse

                (reserved "test" >> withSep (reserved "test"))

                "test;   test"
                (Right ())

            it "should respect new lines" $ assertParse

                (reserved "test" >> withSep (reserved "test"))

                "test\ntest"
                (Right ())

            it "should fail improperly indented scopes" $ assertParse

                (spaces >> withScope (reserved "test" >> withSep (reserved "test")))

                "  test\ntest"
                (Left (Err "(line 2, column 1):\nunexpected \"t\"\nexpecting \";\"\nStatement in scope (introduced at (line 1, column 3))"))

            it "should respect state reset with try" $ assertParse

                (spaces >> withScope (reserved "test" >> (withSep (reserved "test") <|> reserved "test")))

                "   test\ntest"
                (Right ())
