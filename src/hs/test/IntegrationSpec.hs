module IntegrationSpec where

import Test.Hspec
import Test.HUnit

import Minml.AST
import Minml.Config
import Minml.Compile

import Utils

assertNode :: String -> Either Err String -> Assertion
assertNode a b = do
    res <- case compile defaultConfig [("Test Case", a)] of 
        Left x -> return $ Left x
        Right x -> Right `fmap` nodejs x
    assertEqual "" b res

spec :: Spec
spec =

    describe "Minml.Parser" $

        describe "parseMinml" $ do

            it "should compile & run a trivial example " $ assertNode
              
                "   2 + 2.0 == 4.0   "

                (Right "true\n")

            it "should compile & run simple type errors" $ assertNode

                "   4 + \"whoops\"   "

                (Left (Err "Types do not unify\n  Double and String"))

            it "should compile & run let expressions" $ assertNode

                "   let x = 1.0;     \
                \   x + 1.0 == 2.0   "

                (Right "true\n")

            it "should error when trying to compile & run a missing let continuation" $ assertNode

                "   let x = 1;   "

                $ Left (Err "\"Test Case\" (line 1, column 17):\nunexpected end of input\nexpecting Expression")

            it "should compile & run anonymous functions and application" $ assertNode

                "   (fun x -> x + 4) 2 == 6   "

                (Right "true\n")

            it "should compile & run anonymous functions in let bindings" $ assertNode
             
                "   let g = fun x -> x + 4;   \
                \   g 2 == 6                  "

                (Right "true\n")

            it "should compile & run match expressions" $ assertNode

                "   let fib = fun n ->                      \
                \       match n with                        \
                \       0 -> 0;                             \
                \       1 -> 1;                             \
                \       n -> fib (n - 1) + fib (n - 2);;    \
                \   fib 7                                   "

                (Right "13\n")

            it "should fail to compile & run match mixed semicolon" $ assertNode

                "   let fib = fun n ->                      \n\
                \       match n with                        \n\
                \       0 -> 0;                             \n\
                \       1 -> 1;                             \n\
                \       n -> fib (n - 1) + fib (n - 2);     \n\
                \   fib 7                                   \n"

                (Right "13\n")

            it "should compile & run match expressions with type errors" $ assertNode

                "   match 3 + 3 with \"gotcha\" -> false;   "

                (Left (Err "Types do not unify\n  String and Double"))

            it "should compile & run user data types" $ assertNode

                "   data Cons: a -> List a -> List a;        \
                \   data Nil: List a;                        \
                \                                            \
                \   let length = fun n ->                    \
                \       match n with                         \
                \           Nil -> 0;                        \
                \           (Cons _ xs) -> 1 + length xs;;   \
                \                                            \
                \   length (Cons 1 (Cons 2 Nil))             "

                (Right "2\n")

            it "should compile & run with generic type errors" $ assertNode

                "   data Just: a -> Maybe a;          \
                \   data None: Maybe a;               \
                \                                     \
                \   match Just 5 with                 \
                \       (Just \"blammo\") -> false;   "

                (Left (Err "Types do not unify\n  String and Double"))

            it "should compile & run partial keywords" $ assertNode

                "   let letly = 4;                    \
                \   (let func = fun x -> x + ((2));   \
                \   func letly)                       "

                (Right "6\n")

            it "should compile & run binding tricks" $ assertNode

                "   let x = 3;                 \
                \   let f = fun y -> y + x;    \
                \   let x = 2;                 \
                \   f 3 == 6                   "

                (Right "true\n")

            it "should compile & run basic unboxing" $ assertNode

                "   data Just: a -> Maybe a;          \
                \                                     \
                \   match Just 5 with                 \
                \       (Just x) -> x                 "

                (Right "5\n")

            describe "function application whitespace" $ do

                it "should fail to compile & run function application on separate lines" $ assertNode

                    "   parseInt  \n\
                    \   4         "

                    $ Left (Err "\"Test Case\" (line 2, column 4):\nunexpected '4'\nexpecting operator or end of input\nStatement indented (introduced at \"Test Case\" (line 1, column 4))")

                it "should compile & run function application on separate lines indented" $ assertNode

                    "   parseInt  \n\
                    \       4       "

                    $ Right "4\n"

            describe "let and function binding" $ do

                it "should compile & run" $ assertNode

                    "   let fib n =                             \n\
                    \       match n with                        \n\
                    \       0 -> 0                              \n\
                    \       1 -> 1                              \n\
                    \       n -> fib (n - 1) + fib (n - 2)      \n\
                    \   fib 7                                   \n"

                    (Right "13\n")

                it "should compile & run user data types" $ assertNode

                    "   data Cons: a -> List a -> List a         \n\
                    \   data Nil: List a                         \n\
                    \                                            \n\
                    \   let length n =                          \n\
                    \       match n with                         \n\
                    \           Nil -> 0                          \n\
                    \           (Cons _ xs) -> 1 + length xs      \n\
                    \                                            \n\
                    \   length (Cons 1 (Cons 2 Nil))             \n"

                    (Right "2\n")

            describe "let and function binding and implicit match" $

                it "should compile & run basic unboxing" $ assertNode

                     "   data Just: a -> Maybe a     \n\
                     \   unbox (Just x) = x          \n\
                     \   unbox (Just 5)              \n"

                     (Right "5\n")

            describe "Antiquotes" $ do

                it "should compile & run basic javascript" $ assertNode

                    "   data Just: a -> Maybe a       \n\
                    \   unbox x =                     \n\
                    \       match x with              \n\
                    \           (Just x) = ``x``      \n\
                    \   unbox (Just 5)                \n"

                    (Right "5\n")

            describe "Records" $ do

                it "should compile & run basic records" $ assertNode

                    "   f = {x: 3}                  \n\
                    \   match f with                \n\
                    \       {x: x} = x              \n\
                    \       _ = 0                   \n"

                    (Right "3\n")



