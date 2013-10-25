{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module RewriteSpec where

import Test.Hspec
import Test.HUnit
import Text.InterpolatedString.Perl6

import Forml.AST
import Forml.Exec
import Forml.Parse

import Utils hiding (assertParse)

assertNode :: String -> Either Err String -> Assertion
assertNode a b = do
    res <- case compile a of 
        Left x -> return $ Left x
        Right x -> Right `fmap` node x
    assertEqual "" b res

assertParse :: String -> Either Err Expr -> Assertion
assertParse a b = assertEqual "" b (parseForml a)

class Assert a b | b -> a, a -> b where

    (===) :: String -> a -> Assertion
    (=!=) :: String -> b -> Assertion

instance Assert Expr Err where

    x === y = assertParse x (Right y)
    x =!= y = assertParse x (Left y)

instance Assert String String where

    x === y = assertNode x (Right y)
    x =!= y = assertNode x (Left (Err y))


spec :: Spec
spec =

    describe "Forml.Parser" $ do


        describe "parse" $ do



            it "should parse a trivial example " $ [q|
              
                `if (a) then (d) else (c)` =

                    match a with
                        True  = d                             
                        False = c                            
                                                         
                if False then 3 else 4                   

            |] ===

                MatExpr (VarExpr (ConVal (TypeSym (TypeSymP "False"))))
                        [ ( ValPatt (ConVal (TypeSym (TypeSymP "True")))
                          , VarExpr (LitVal (NumLit 3.0)))
                        , ( ValPatt (ConVal (TypeSym (TypeSymP "False")))
                          , VarExpr (LitVal (NumLit 4.0)))]



            it "should parse a trivial example with some odd whitespace" $ [q|
              
                `if (a) then (d) else (c)` = match a with   
                    True = d                                
                    False = c                               
                                                            
                if False                                    
                    then 3                                  
                    else 4                                  

            |] ===

                MatExpr (VarExpr (ConVal (TypeSym (TypeSymP "False"))))
                        [ ( ValPatt (ConVal (TypeSym (TypeSymP "True")))
                          , VarExpr (LitVal (NumLit 3.0)))
                        , ( ValPatt (ConVal (TypeSym (TypeSymP "False")))
                          , VarExpr (LitVal (NumLit 4.0)))]



            it "should parse nested macros" $ assertParse
              
                "   `if (a) then (d) else (c)` = match a with   \n\
                \       True = d                                \n\
                \       False = c                               \n\
                \                                               \n\
                \   if False                                    \n\
                \       then if True then 3 else 5              \n\
                \       else 4                                  \n"

                (Right (MatExpr (VarExpr (ConVal (TypeSym (TypeSymP "False")))) [(ValPatt (ConVal (TypeSym (TypeSymP "True"))),MatExpr (VarExpr (ConVal (TypeSym (TypeSymP "True")))) [(ValPatt (ConVal (TypeSym (TypeSymP "True"))),VarExpr (LitVal (NumLit 3.0))),(ValPatt (ConVal (TypeSym (TypeSymP "False"))),VarExpr (LitVal (NumLit 5.0)))]),(ValPatt (ConVal (TypeSym (TypeSymP "False"))),VarExpr (LitVal (NumLit 4.0)))]))

            it "should parse scope introduction" $ assertParse
              
                "   `bind (a) to (b) in (c)` =    \n\
                \       let a = b                 \n\
                \       c                         \n\
                \                                 \n\
                \   bind x to 12 in x + 1         \n"

                (Right (LetExpr (Sym "x") (VarExpr (LitVal (NumLit 12.0))) (AppExpr (AppExpr (VarExpr (SymVal (Sym "+"))) (VarExpr (SymVal (Sym "x")))) (VarExpr (LitVal (NumLit 1.0))))))

            it "should parse repeating args" $ pendingWith "TODO figure out syntax"
            --assertParse
              
            --    "   `[ (a*) ]` = ``a``                          \n\
            --    \                                               \n\
            --    \   [ 3; 4; 5 ] == [ 3; 4; 5 ]                  \n"

            --    (Right (MatExpr (VarExpr (ConVal (TypeSym (TypeSymP "False")))) [(ValPatt (ConVal (TypeSym (TypeSymP "True"))),MatExpr (VarExpr (ConVal (TypeSym (TypeSymP "True")))) [(ValPatt (ConVal (TypeSym (TypeSymP "True"))),VarExpr (LitVal (NumLit 3.0))),(ValPatt (ConVal (TypeSym (TypeSymP "False"))),VarExpr (LitVal (NumLit 5.0)))]),(ValPatt (ConVal (TypeSym (TypeSymP "False"))),VarExpr (LitVal (NumLit 4.0)))]))

    	describe "run" $ do

            it "should compile & run a trivial example " $ [q|
              
                True: Bool                               
                False: Bool                              
                                                         
                `if (a) then (d) else (c)` = match a with
                    True = d                             
                    False = c                            
                                                         
                if False then 3 else 4                   

            |] ===

                "4\n"

            it "should compile & run nested macros" $ [q|
              
                True: Bool                               
                False: Bool                              
                                                         
                `if (a) then (d) else (c)` = match a with
                    True = d                             
                    False = c                            
                                                         
                if True                                  
                    then if False then 3 else 5          
                    else 4                               

            |] === 

                "5\n"



            it "should compile & run nested definitions" $ [q|
              
                True: Bool                     
                False: Bool                    
                                               
                `do (b)` =                     
                    `bind (a) to (d) in (c)` = 
                         c                     
                    b                          
                                               
                do bind True to False          
                   in 4                        

            |] =!=

                "Unbound identifier: bind"



            it "should compile & run a complicated nested macro" $ [q|

                True:  Bool
                False: Bool

                `if (a) then (b) else (c)` = match a with
                    True  = b
                    False = c

                `if (a) { (b) } else { (c) }` = if a then b else c
                `if (a) { (b) } else (c)`     = if a then b else c
                `if (a); (b); (c)`        = if a then b else c

                if True then if False { 1 } else 2 else 3

            |] ===

                "2\n"



            it "should compile & run nested definitions" $ [q|
              
                True: Bool                                  
                False: Bool                                 
                                                            
                `do (b)` =                                  
                    `bind (a) to (b) in (c)` =              
                         c                                  
                    bind 4 to asd asd fas in b              
                                                            
                do 4

            |] ===

                "4\n"



            it "should compile & run nested definitions, without var capturing" $ [q|
              
                True: Bool
                False: Bool

                `do (b)` =
                    `bind (a) to (d) in (c)` =
                         c
                    bind 4 to asd asd fas in b

                do 4 

            |] ===

                "4\n"



            describe "Let binding replacements" $ do



                it "should compile & run scope introduction" $ [q|
                  
                    `bind (a) to (b) in (c)` =
                        let a = b             
                        c                     
                                              
                    bind x to 12 in x + 1     

                |] ===

                    "13\n"



                it "should compile & run scope introduction" $ [q|
                  
                    `bind (a) to (b) in (c)` =
                        let a = b             
                        c                     
                                              
                    bind x to 12 in x + 1     

                |] ===

                    "13\n"



                it "should compile & run separators with a newline" $ [q|
                  
                    `bind (a) to (b); (c)` =
                        let a = b             
                        c                     
                                              
                    bind x to 12
                    x + 1     

                |] ===

                    "13\n"



                it "should compile & run separators with a semicolon" $ [q|
                  
                    `bind (a) to (b); (c)` =
                        let a = b             
                        c                     
                                              
                    bind x to 12; x + 1     

                |] ===

                    "13\n"



                it "should compile & run recursive references bound to let replacements" $ [q|
                  
                    Cons: a -> List a -> List a         
                    Nil: List a                         
                                                        
                    `check prop (a) of (b)` =

                        a n =                     
                            match n with                    
                                Nil = 0                     
                                (Cons _ xs) = 1 + a xs 
                                                            
                        a b     

                    check prop size of (Cons 1 (Cons 2 Nil)) 

                |] ===

                    "2\n"


            describe "Regression tests" $

                it "Should run programs with 2 arguments" $ [q|
                  
                    f a b = a + b
                    f 1 2

                |] ===

                    "3\n"



 
