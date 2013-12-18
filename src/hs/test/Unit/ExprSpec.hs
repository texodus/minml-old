
module Unit.ExprSpec where

import Test.Hspec

import Unit.Source

spec :: Spec
spec =

    describe "Expressions (from template)" $ do
        
        sample "a trivial example "

            "   2 + 2.0 == 4.0   "

        sample "should parse simple type errors"

            "   4 + \"whoops\"   "

        sample "should parse let expressions"

            "   let x = 1.0;     \
            \   x + 1.0 == 2.0   "

        sample "should parse anonymous functions and application"

            "   (fun x -> x + 4) 2 == 6   "

        sample "should parse anonymous functions in let bindings"

            "   let g = fun x -> x + 4;   \
            \   g 2 == 6                  "

        sample "should parse match expressions"

            "   let fib = fun n ->                      \
            \       match n with                        \
            \       0 -> 0;                             \
            \       1 -> 1;                             \
            \       n -> fib (n - 1) + fib (n - 2);;    \
            \   fib 7                                   "

        sample "should parse match mixed semicolon with newlines"

            "   let fib = fun n ->                      \n\
            \       match n with                        \n\
            \       0 -> 0;                             \n\
            \       1 -> 1;                             \n\
            \       n -> fib (n - 1) + fib (n - 2);     \n\
            \   fib 7                                   \n"

        sample "should parse match expressions with type errors"

            "   match 3 + 3 with \"gotcha\" -> false;   "

        sample "should parse user data types"

            "   data Cons: a -> List a -> List a;        \
            \   data Nil: List a;                        \
            \                                            \
            \   let length = fun n ->                    \
            \       match n with                         \
            \           Nil -> 0;                        \
            \           (Cons _ xs) -> 1 + length xs;;   \
            \                                            \
            \   length (Cons 1 (Cons 2 Nil))             "

        sample "should parse with generic type errors"

            "   data Just: a -> Maybe a;          \
            \   data None: Maybe a;               \
            \                                     \
            \   match Just 5 with                 \
            \       (Just \"blammo\") -> false;   "

        sample "should parse partial keywords"

            "   let letly = 4;                    \
            \   (let func = fun x -> x + ((2));   \
            \   func letly)                       "

        sample "should parse binding tricks"

            "   let x = 3;                 \
            \   let f = fun y -> y + x;    \
            \   let x = 2;                 \
            \   f 3 == 6                   "

        sample "should parse records"

            "   let x = {x: 4; y: 6}   \n\  
            \   x                      \n"

        sample "should parse record Equality"

            "   let x = {x: 4; y: 6}   \n\
            \   let y = {x: 4; z: 3}   \n\
            \   x == y                 \n"

        describe "function application whitespace" $

            sample "should parse function application on separate lines indented"

                "   f       \n\
                \       4     "

        describe "whitespace let" $ do

            sample "should parse whitespace statement sep"

                "   let letly = 4                    \n\
                \   let func = fun x -> x + ((2))    \n\
                \   func letly                       \n"

            sample "should parse whitespace statement sep with proper scope"

                "   let letly =       \n\
                \       let func =    \n\
                \           5         \n\
                \       4             \n\
                \   func 5            \n"

            sample "should parse match expressions with mixed whitespace"

                "   let fib = fun n ->                     \n\
                \       match n with                       \n\
                \       0 -> 0;                            \n\
                \       1 -> 1;                            \n\
                \       n -> fib (n - 1) + fib (n - 2);    \n\
                \   fib 7                                  \n"

        describe "whitespace match" $

            sample "should parse match expressions"

                "   let fib = fun n ->                     \n\
                \       match n with                       \n\
                \       0 -> 0                             \n\
                \       1 -> 1                             \n\
                \       n -> fib (n - 1) + fib (n - 2)     \n\
                \   fib 7                                  \n"

        describe "whitespace data" $

            sample "should parse with generic type errors"

                "   data Just: a -> Maybe a           \n\
                \   data None: Maybe a                \n\
                \                                     \n\
                \   match Just 5 with                 \n\
                \       (Just \"blammo\") -> false    \n"

        describe "without `let` keyword" $ do

            sample "should parse whitespace statement sep"

                "   letly = 4                        \n\
                \   func = fun x -> x + ((2))        \n\
                \   func letly                       \n"

            sample "should parse match expressions"

                "   fib = fun n ->                         \n\
                \       match n with                       \n\
                \       0 -> 0                             \n\
                \       1 -> 1                             \n\
                \       n -> fib (n - 1) + fib (n - 2)     \n\
                \   fib 7                                  \n"

        describe "without `=` reserved operator" $ do

            sample "should parse anonymous functions in let bindings"

                "   let g = fun x = x + 4    \n\
                \   g 2 == 6                 \n"

            sample "should parse whitespace statement sep"

                "   letly = 4                   \n\
                \   func = fun x = x + ((2))    \n\
                \   func letly                  \n"

            sample "should parse match expressions with -> cases"

                "   fib = fun n =                          \n\
                \       match n with                       \n\
                \       0 -> 0                             \n\
                \       1 -> 1                             \n\
                \       n -> fib (n - 1) + fib (n - 2)     \n\
                \   fib 7                                  \n"

            sample "should parse match expressions with = cases"

                "   fib = fun n =                          \n\
                \       match n with                       \n\
                \       0 = 0                              \n\
                \       1 = 1                              \n\
                \       n = fib (n - 1) + fib (n - 2)      \n\
                \   fib 7                                  \n"

        describe "with `λ` reserved operator" $ do

            sample "should parse anonymous functions in let bindings"

                "   let g = λ x = x + 4      \n\
                \   g 2 == 6                 \n"

            sample "should parse whitespace statement sep"

                "   letly = 4                   \n\
                \   func = λ x = x + ((2))      \n\
                \   func letly                  \n"

            sample "should parse match expressions"

                "   fib = λ n =                            \n\
                \       match n with                       \n\
                \       0 -> 0                             \n\
                \       1 -> 1                             \n\
                \       n -> fib (n - 1) + fib (n - 2)     \n\
                \   fib 7                                  \n"

        describe "without `data` keyword" $

            sample "should parse user data types"

                "   Cons: a -> List a -> List a            \n\
                \   Nil: List a                            \n\
                \                                          \n\
                \   length n =                             \n\
                \       match n with                       \n\
                \           Nil -> 0                       \n\
                \           (Cons _ xs) -> 1 + length xs   \n\
                \                                          \n\
                \   length (Cons 1 (Cons 2 Nil))           \n"

        describe "Records" $

            sample "should parse basic record patterns"

                "   f = {x: 3}       \n\
                \   match f with     \n\
                \       {x: x} = x   \n\
                \       _ = 0        \n" 


        describe "let and function binding and implicit match" $

            sample "should compile & run basic unboxing"

                 "   data Just: a -> Maybe a     \n\
                 \   unbox (Just x) = x          \n\
                 \   unbox (Just 5)              \n"

        describe "Antiquotes" $ do

            it "should compile & run basic javascript" $

                pendingWith "This example works, but serializing JExpr does not, so no gold record"

            --    "   data Just: a -> Maybe a       \n\
            --    \   unbox x =                     \n\
            --    \       match x with              \n\
            --    \           (Just x) = ``x``      \n\
            --    \   unbox (Just 5)                \n"

            sample "should compile & run binding tricks"

                "   let x = 3;                 \
                \   let f = fun y -> y + x;    \
                \   let x = 2;                 \
                \   f 3 == 6                   "

            sample "should compile & run basic unboxing"

                "   data Just: a -> Maybe a;          \
                \                                     \
                \   match Just 5 with                 \
                \       (Just x) -> x                 "

------------------------------------------------------------------------------
