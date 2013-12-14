module Unit.Parse.PattSpec where

import qualified Data.Map as M
import Test.Hspec

import Minml.AST
import Minml.Parse.Patt()
import Minml.Parse.Syntax
import Minml.Parse.Token

import Utils

pattP :: Parser Patt
pattP = syntax

spec :: Spec
spec =

    describe "Minml.Parser.Patt" $

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

            it "should parse patterns" $ assertParse pattP
                "{x: 1}" $
                Right (RecPatt (Record (M.fromList [("x",ValPatt (LitVal (NumLit 1.0)))])))

            it "should parse nested" $ assertParse pattP
                "(Cons 1 (Cons 2 Nil))" $ 
                Right (ConPatt (TypeSymP "Cons") 
                               [ ValPatt (LitVal (NumLit 1.0))
                               , ConPatt (TypeSymP "Cons") 
                                         [ ValPatt (LitVal (NumLit 2.0))
                                         , ValPatt (ConVal (TypeSym (TypeSymP "Nil")))]])
