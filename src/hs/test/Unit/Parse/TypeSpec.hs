module Unit.Parse.TypeSpec where

import Test.Hspec

import Forml.AST
import Forml.Parse.Type

import Utils

spec :: Spec
spec = do

    describe "Forml.Parser.Lit" $ do

        describe "typSymP" $ do

            it "should parse type" $ assertParse typSymP
                "Boolean" $
                Right (TypeSymP "Boolean")

            it "should fail to parse symbols" $ assertParse typSymP
                "notAType" $
                Left (Err "(line 1, column 1):\nunexpected \"n\"\nexpecting uppercase letter")

            it "should fail to parse integers" $ assertParse typSymP
                "3" $
                Left (Err "(line 1, column 1):\nunexpected \"3\"\nexpecting uppercase letter")

        describe "typAbsP" $ do

            it "should parse types" $ assertParse typAbsP
                "Boolean" $
                Right (TypeAbsP (TypeSym (TypeSymP "Boolean")))

            it "should parse type abstractions" $ assertParse typAbsP
                "Boolean -> Int" $
                Right (TypeAbsP (TypeApp (TypeApp (TypeSym (TypeSymP "->")) (TypeSym (TypeSymP "Boolean"))) (TypeSym (TypeSymP "Int"))))

            it "should parse parens" $ assertParse typAbsP
                "Boolean -> (Int -> Int)" $
                Right (TypeAbsP (TypeApp (TypeApp (TypeSym (TypeSymP "->")) 
                                                  (TypeSym (TypeSymP "Boolean")))
                                         (TypeApp (TypeApp (TypeSym (TypeSymP "->")) (TypeSym (TypeSymP "Int")))
                                                  (TypeSym (TypeSymP "Int")))))

            it "should parse type abstractions" $ assertParse typAbsP
                "a" $
                Right (TypeAbsP (TypeVar (TypeVarP "a")))

            it "should parse type abstractions" $ assertParse typAbsP
                "List a -> List a b -> String" $
                Right (TypeAbsP (TypeApp (TypeApp (TypeSym (TypeSymP "->"))
                                                  (TypeApp (TypeSym (TypeSymP "List"))
                                                           (TypeVar (TypeVarP "a"))))
                                         (TypeApp (TypeApp (TypeSym (TypeSymP "->"))
                                                           (TypeApp (TypeApp (TypeSym (TypeSymP "List"))
                                                                             (TypeVar (TypeVarP "a")))
                                                                    (TypeVar (TypeVarP "b"))))
                                                  (TypeSym (TypeSymP "String")))))