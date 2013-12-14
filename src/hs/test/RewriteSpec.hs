{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module RewriteSpec where

import Control.Applicative
import Control.Monad
import Test.Hspec
import Test.HUnit
import Text.InterpolatedString.Perl6

import Forml.AST
import Forml.Config
import Forml.Compile
import Forml.Prelude

import Utils hiding (assertParse)

assertNode :: String -> Either Err String -> Assertion
assertNode a b = do
    res <- case compile defaultConfig [("Test Case", a)] of 
        Left x -> return $ Left x
        Right x -> Right `fmap` nodejs x
    assertEqual "" b res

assertParse :: String -> Either Err Expr -> Assertion
assertParse a b = assertEqual "" b (head . tail . fst <$> foldM parse ([], emptyState) [("Prelude", prelude), ("Test Case", a)])

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
              
                `if (a) than (d) else (c)` =

                    match a with
                        True  = d                             
                        False = c                            
                                                         
                if False than 3 else 4                   

            |] ===

                MatExpr (VarExpr (ConVal (TypeSym (TypeSymP "False"))))
                        [ ( ValPatt (ConVal (TypeSym (TypeSymP "True")))
                          , VarExpr (LitVal (NumLit 3.0)))
                        , ( ValPatt (ConVal (TypeSym (TypeSymP "False")))
                          , VarExpr (LitVal (NumLit 4.0)))]



            it "should parse a trivial example with some odd whitespace" $ [q|
              
                `if (a) than (d) else (c)` = match a with   
                    True = d                                
                    False = c                               
                                                            
                if False                                    
                    than 3                                  
                    else 4                                  

            |] ===

                MatExpr (VarExpr (ConVal (TypeSym (TypeSymP "False"))))
                        [ ( ValPatt (ConVal (TypeSym (TypeSymP "True")))
                          , VarExpr (LitVal (NumLit 3.0)))
                        , ( ValPatt (ConVal (TypeSym (TypeSymP "False")))
                          , VarExpr (LitVal (NumLit 4.0)))]



            it "should parse nested macros" $ assertParse
              
                "   `if (a) than (d) else (c)` = match a with   \n\
                \       True = d                                \n\
                \       False = c                               \n\
                \                                               \n\
                \   if False                                    \n\
                \       than if True than 3 else 5              \n\
                \       else 4                                  \n"

                (Right (MatExpr (VarExpr (ConVal (TypeSym (TypeSymP "False")))) [(ValPatt (ConVal (TypeSym (TypeSymP "True"))),MatExpr (VarExpr (ConVal (TypeSym (TypeSymP "True")))) [(ValPatt (ConVal (TypeSym (TypeSymP "True"))),VarExpr (LitVal (NumLit 3.0))),(ValPatt (ConVal (TypeSym (TypeSymP "False"))),VarExpr (LitVal (NumLit 5.0)))]),(ValPatt (ConVal (TypeSym (TypeSymP "False"))),VarExpr (LitVal (NumLit 4.0)))]))

            it "should parse scope introduction" $ assertParse
              
                "   `bind (a) to (b) in (c)` =    \n\
                \       let a = b                 \n\
                \       c                         \n\
                \                                 \n\
                \   bind x to 12 in x + 1         \n"

                (Right (LetExpr (Sym "x") (VarExpr (LitVal (NumLit 12.0))) (Just (AppExpr (AppExpr (VarExpr (SymVal (Sym "+"))) (VarExpr (SymVal (Sym "x")))) (VarExpr (LitVal (NumLit 1.0)))))))

        describe "run" $ do

            it "should compile & run a trivial example " $ [q|
              
                True: Bool                               
                False: Bool                              
                                                         
                `if (a) than (d) else (c)` = match a with
                    True = d                             
                    False = c                            
                                                         
                if False than 3 else 4                   

            |] ===

                "4\n"

            it "should compile & run nested macros" $ [q|
              
                True: Bool                               
                False: Bool                              
                                                         
                `if (a) than (d) else (c)` = match a with
                    True = d                             
                    False = c                            
                                                         
                if True                                  
                    than if False than 3 else 5          
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

                `when (a) then (b) else (c)` = match a with
                    True  = b
                    False = c

                `when (a) { (b) } else { (c) }` = when a then b else c
                `when (a) { (b) } else (c)`     = when a then b else c
                `when (a); (b); (c)`            = when a then b else c

                when True then when False { 1 } else 2 else 3

            |] ===

                "2\n"



            it "should compile & run nested definitions" $ [q|
              
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
                                                        
                    `check prop (q) of (b)` =

                        a n =                     
                            match n with                    
                            Nil = 0                     
                            Cons _ xs = 1 + a xs 
                                                            
                        a b     

                    check prop size of (Cons 1 (Cons 2 Nil)) 

                |] ===

                    "2\n"


            describe "Pattern Replacements" $ do


                it "should compile & run pattern replacements" $ [q|
                  
                    Cons: a -> List a -> List a         
                    Nil: List a   
                    
                    `bind (a) to (b); (c)` =
                        match a with 
                        b -> c                     
                                              
                    bind Cons 1 Nil to Cons x y
                    x + 1     

                |] === "2\n"


                it "should compile & run nested pattern replacements" $ [q|
                  
                    Cons: a -> List a -> List a         
                    Nil: List a   
                    
                    `bind first (a) to (b); (c)` =
                        match a with 
                        Cons b y -> c
                        z -> 0

                    bind first Cons 1 (Cons 2 Nil) to 1
                    2     

                |] === "2\n"

                it "should compile & run pattern replacements when they collide with let" $ [q|
                  
                    Box: a -> Box a
                    unbox (Box x) = x
                    unbox (Box 5) == 5    

                |] === "true\n"

                it "should compile & run complex pattern replacements when they collide with let" $ [q|
                  
                    Box: a -> Box a
                    unbox z (Box x) = x + z
                    unbox 2 (Box 5) == 7   

                |] === "true\n"

                it "should compile & run let-patterns" $ [q|
                  
                    Box: a -> Box a
                    let (Box x) = Box 5
                    x + 5    

                |] === "10\n"

            describe "XML tests" $

                it "Should run simple XML" $ [q|
 
                    Xml: String -> String -> String
                   
                    `<(a)> (b) </(c)>` = Xml a b
                 
                    <"div"> 
                        <"a">"test"</"a">
                    </"div">

                |] ===

                    "{ '0': 'div', '1': { '0': 'a', '1': 'test' } }\n"

            describe "Infix tests" $ do

                it "Should run simple infix expressions" $ [q|
 
                    `(a) +++ (b)` = a + b
                    2 +++ 2

                |] ===

                    "4\n"

                it "Should run nested infix expressions" $ [q|
 
                    `(a) +++ (b)` = a + b
                    2 +++ 2 +++ 2 +++ 2

                |] ===

                    "8\n"

                it "Should run nested triple infix expressions" $ [q|
 
                    `(a) <- (b) -< (c)` = a + b + c
                    2 <- 2 <- 2 -< 2 -< 2

                |] ===

                    "10\n"

            describe "Javascript replacements" $

                it "Should run simple javascript replacements" $ [q|
 
                    `add (x) to (y)` = ``x + y``
                    add 4 to 4

                |] ===

                    "8\n"


            describe "Regression tests" $

                it "Should run programs with 2 arguments" $ [q|
                  
                    f a b = a + b
                    f 1 2

                |] ===

                    "3\n"



 
