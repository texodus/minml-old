module Unit.Parse.PattSpec where

import Test.Hspec

import Forml.AST
import Forml.Parse.Patt

import Utils

spec :: Spec
spec = do

    describe "Forml.Parser.Patt" $ do

    	describe "pattP" $ do

            it "should parse Cons" $ assertParse pattP
                "True" $ 
                Right (ValPatt (ConVal (TypeSym (TypeSymP "True"))))

            it "should parse Cons w/args" $ assertParse pattP
                "(Cons 1 Nil)" $ 
                Right (ConPatt (TypeSymP "Cons") [ValPatt (LitVal (NumLit 1.0)),ValPatt (ConVal (TypeSym (TypeSymP "Nil")))])

            it "should parse literals" $ assertParse pattP
                "2" $ 
                Right (ValPatt (LitVal (NumLit 2.0)))

            it "should parse symbols" $ assertParse pattP
                "x" $
                Right (ValPatt (SymVal (Sym "x")))

            it "should parse nested" $ assertParse pattP
                "(Cons 1 (Cons 2 Nil))" $ 
                Right (ConPatt (TypeSymP "Cons") 
                	           [ ValPatt (LitVal (NumLit 1.0))
                	           , ConPatt (TypeSymP "Cons") 
                	                     [ ValPatt (LitVal (NumLit 2.0))
                	                     , ValPatt (ConVal (TypeSym (TypeSymP "Nil")))]])
