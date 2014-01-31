{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Integration.MacroSpec where

import Test.Hspec
import Text.InterpolatedString.Perl6

import Unit.Source


spec :: Spec
spec =

    describe "Minml.Parser" $ do

        describe "parse" $ do

            sample "a trivial example " [q|
              
                `if (a) than (d) else (c)` =

                    match a with
                        True  = d                             
                        False = c                            
                                                         
                if False than 3 else 4                   

            |]


            sample "a trivial example with some odd whitespace" [q|
              
                `if (a) than (d) else (c)` = match a with   
                    True = d                                
                    False = c                               
                                                            
                if False                                    
                    than 3                                  
                    else 4                                  

            |] 

            sample "nested macros"
              
                "   `if (a) than (d) else (c)` = match a with   \n\
                \       True = d                                \n\
                \       False = c                               \n\
                \                                               \n\
                \   if False                                    \n\
                \       than if True than 3 else 5              \n\
                \       else 4                                  \n"

            sample "scope introduction"
              
                "   `bind (a) to (b) in (c)` =    \n\
                \       let a = b                 \n\
                \       c                         \n\
                \                                 \n\
                \   bind x to 12 in x + 1         \n"

        describe "run" $ do

            sample "a trivial example " [q|
              
                True: Bool                               
                False: Bool                              
                                                         
                `if (a) than (d) else (c)` = match a with
                    True = d                             
                    False = c                            
                                                         
                if False than 3 else 4                   

            |]

            sample "nested macros" [q|
              
                True: Bool                               
                False: Bool                              
                                                         
                `if (a) than (d) else (c)` = match a with
                    True = d                             
                    False = c                            
                                                         
                if True                                  
                    than if False than 3 else 5          
                    else 4                               

            |]

            it "nested definitions" $

                pendingWith "Not yet implemented"

            -- [q|
              
            --    True: Bool                     
            --    False: Bool                    
                                               
            --    `do (b)` =                     
            --        `bind (a) to (d) in (c)` = 
            --             c                     
            --        b                          
                                               
            --    do bind x to 4          
            --       in x + 4                        

            -- |] ===

            --    "8\n"



            sample "nested definitions" [q|
              
                True: Bool                     
                False: Bool                    
                                               
                `do (b)` =                     
                    `bind (a) to (d) in (c)` = 
                         c                     
                    b                          
                                               
                bind True to False          

            |]

            sample "a complicated nested macro" [q|

                `when (a) then (b) else (c)` = match a with
                    True  = b
                    False = c

                `when (a) { (b) } else { (c) }` = when a then b else c
                `when (a) { (b) } else (c)`     = when a then b else c
                `when (a); (b); (c)`            = when a then b else c

                when True then when False { 1 } else 2 else 3

            |]

            sample "nested definitions" $ [q|
              
                `do (b)` =                                  
                    `bind (a) to (b) in (c)` =              
                         c                                  
                    bind 4 to asd asd fas in b              
                                                            
                do 4

            |]

            sample "nested definitions, without var capturing" [q|
              
                True: Bool
                False: Bool

                `do (b)` =
                    `bind (a) to (d) in (c)` =
                         c
                    bind 4 to asd asd fas in b

                do 4 

            |]

            describe "Let binding replacements" $ do



                sample "scope introduction" [q|
                  
                    `bind (a) to (b) in (c)` =
                        let a = b             
                        c                     
                                              
                    bind x to 12 in x + 1     

                |]

                sample "scope introduction" $ [q|
                  
                    `bind (a) to (b) in (c)` =
                        let a = b             
                        c                     
                                              
                    bind x to 12 in x + 1     

                |] 


                sample "separators with a newline" $ [q|
                  
                    `bind (a) to (b); (c)` =
                        let a = b             
                        c                     
                                              
                    bind x to 12
                    x + 1     

                |]

                sample "separators with a semicolon" $ [q|
                  
                    `bind (a) to (b); (c)` =
                        let a = b             
                        c                     
                                              
                    bind x to 12; x + 1     

                |]

                sample "recursive references bound to let replacements" $ [q|
                  
                    Cons: a -> List a -> List a         
                    Nil: List a                         
                                                        
                    `check prop (q) of (b)` =

                        a n =                     
                            match n with                    
                            Nil = 0                     
                            Cons _ xs = 1 + a xs 
                                                            
                        a b     

                    check prop size of (Cons 1 (Cons 2 Nil)) 

                |]

            describe "Pattern Replacements" $ do


                sample "pattern replacements" $ [q|
                  
                    Cons: a -> List a -> List a         
                    Nil: List a   
                    
                    `bind (a) to (b); (c)` =
                        match a with 
                        b -> c                     
                                              
                    bind Cons 1 Nil to Cons x y
                    x + 1     

                |]

                sample "nested pattern replacements" $ [q|
                  
                    Cons: a -> List a -> List a         
                    Nil: List a   
                    
                    `bind first (a) to (b); (c)` =
                        match a with 
                        Cons b y -> c
                        z -> 0

                    bind first Cons 1 (Cons 2 Nil) to 1
                    2     

                |]

                sample "pattern replacements when they collide with let" $ [q|
                  
                    Box: a -> Box a
                    unbox (Box x) = x
                    unbox (Box 5) == 5    

                |]

                sample "complex pattern replacements when they collide with let" $ [q|
                  
                    Box: a -> Box a
                    unbox z (Box x) = x + z
                    unbox 2 (Box 5) == 7   

                |]

                sample "let-patterns" $ [q|
                  
                    Box: a -> Box a
                    let (Box x) = Box 5
                    x + 5    

                |]

            describe "XML tests" $

                sample "simple XML" $ [q|
 
                    Xml: String -> String -> Xml
                   
                    `<(a)> (b) </(c)>` = Xml a b
                 
                    <"div"> 
                        <"a">"test"</"a">
                    </"div">

                |] 

            describe "Javascript replacements" $

                sample "simple javascript replacements" $ [q|
 
                    `add (x) to (y)` = ``x + y``
                    add 4 to 4

                 |]

