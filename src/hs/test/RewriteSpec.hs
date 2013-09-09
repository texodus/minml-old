module RewriteSpec where

import Test.Hspec
import Test.HUnit

import Forml.AST
import Forml.Exec
import Forml.Parse

import Utils hiding (assertParse)

assertNode :: String -> Either Err String -> Assertion
assertNode a b = do
    res <- case compile a of 
        Left x -> return $ Left x
        Right x -> Right `fmap` node x
    flip (assertEqual "") res b

assertParse :: String -> Either Err Expr -> Assertion
assertParse a b = flip (assertEqual "") (parseForml a) b

spec :: Spec
spec = do

    describe "Forml.Parser" $ do


        describe "parse" $ do

            it "should parse a trivial example " $ assertParse
              
                "   `if (a) then (d) else (c)` = match a with   \n\
                \       True = d                                \n\
                \       False = c                               \n\
                \                                               \n\
                \   if False then 3 else 4                      \n"

                (Right (MatExpr (VarExpr (ConVal (TypeSym (TypeSymP "False"))))
                                [ (ValPatt (ConVal (TypeSym (TypeSymP "True"))), VarExpr (LitVal (NumLit 3.0)))
                                , (ValPatt (ConVal (TypeSym (TypeSymP "False"))), VarExpr (LitVal (NumLit 4.0)))]))

            it "should parse a trivial example with some odd whitespace" $ assertParse
              
                "   `if (a) then (d) else (c)` = match a with   \n\
                \       True = d                                \n\
                \       False = c                               \n\
                \                                               \n\
                \   if False                                    \n\
                \       then 3                                  \n\
                \       else 4                                  \n"

                (Right (MatExpr (VarExpr (ConVal (TypeSym (TypeSymP "False"))))
                                [ (ValPatt (ConVal (TypeSym (TypeSymP "True"))), VarExpr (LitVal (NumLit 3.0)))
                                , (ValPatt (ConVal (TypeSym (TypeSymP "False"))), VarExpr (LitVal (NumLit 4.0)))]))

            it "should parse nested macros" $ assertParse
              
                "   `if (a) then (d) else (c)` = match a with   \n\
                \       True = d                                \n\
                \       False = c                               \n\
                \                                               \n\
                \   if False                                    \n\
                \       then if True then 3 else 5              \n\
                \       else 4                                  \n"

                (Right (MatExpr (VarExpr (ConVal (TypeSym (TypeSymP "False")))) [(ValPatt (ConVal (TypeSym (TypeSymP "True"))),MatExpr (VarExpr (ConVal (TypeSym (TypeSymP "True")))) [(ValPatt (ConVal (TypeSym (TypeSymP "True"))),VarExpr (LitVal (NumLit 3.0))),(ValPatt (ConVal (TypeSym (TypeSymP "False"))),VarExpr (LitVal (NumLit 5.0)))]),(ValPatt (ConVal (TypeSym (TypeSymP "False"))),VarExpr (LitVal (NumLit 4.0)))]))

            it "should parse scope introduction" $ pendingWith "TODO add replaceLet"

               --assertParse
              
               -- "   `var (a) to (b) then (c)` =                      \n\
               -- \       let a = b; c                            \n\
               -- \                                               \n\
               -- \   var x to 12 then x + 1                                      \n"

               -- (Right (LetExpr (Sym "x") (VarExpr (LitVal (NumLit 12.0))) (AppExpr (AppExpr (VarExpr (SymVal (Sym "+"))) (VarExpr (SymVal (Sym "x")))) (VarExpr (LitVal (NumLit 1.0))))))

            it "should parse repeating args" $ pendingWith "TODO figure out syntax"
            --assertParse
              
            --    "   `[ (a*) ]` = ``a``                          \n\
            --    \                                               \n\
            --    \   [ 3; 4; 5 ] == [ 3; 4; 5 ]                  \n"

            --    (Right (MatExpr (VarExpr (ConVal (TypeSym (TypeSymP "False")))) [(ValPatt (ConVal (TypeSym (TypeSymP "True"))),MatExpr (VarExpr (ConVal (TypeSym (TypeSymP "True")))) [(ValPatt (ConVal (TypeSym (TypeSymP "True"))),VarExpr (LitVal (NumLit 3.0))),(ValPatt (ConVal (TypeSym (TypeSymP "False"))),VarExpr (LitVal (NumLit 5.0)))]),(ValPatt (ConVal (TypeSym (TypeSymP "False"))),VarExpr (LitVal (NumLit 4.0)))]))

    	describe "run" $ do

            it "should compile & run a trivial example " $ assertNode
              
                "   True: Bool                                  \n\
                \   False: Bool                                 \n\
                \                                               \n\
                \   `if (a) then (d) else (c)` = match a with   \n\
                \       True = d                                \n\
                \       False = c                               \n\
                \                                               \n\
                \   if False then 3 else 4                      \n"

                (Right "4\n")

            it "should compile & run nested macros" $ assertNode
              
                "   True: Bool                                  \n\
                \   False: Bool                                 \n\
                \                                               \n\
                \   `if (a) then (d) else (c)` = match a with   \n\
                \       True = d                                \n\
                \       False = c                               \n\
                \                                               \n\
                \   if True                                     \n\
                \       then if False then 3 else 5             \n\
                \       else 4                                  \n"

                (Right "5\n")



  